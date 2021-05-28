## Script: 02_HTEs.R ------------------------------------


# Working setup ---------------------------------
library(data.table)
library(ggplot2)
library(grf)
library(haven) # read .dta files
library(lmtest)
library(sandwich)
library(statar)
library(stringr)


# Helper functions
generate_X_Y_W_C <- function(outcome, covariates) {
  # _______________________________________________________
  # Generates the X, Y, W and cluster components for a given outcome 
  # and a set of covariates by eliminating missing observations
  # from the SC_Data dataset. Assumes the treatment column is
  # named 'treatment', and cluster column is 'b_schoolid.
  # Inputs:
  # - outcome: (str) outcome column name in SC_Data
  # - covariates: (vector of str) covariate column names
  # Returns:
  # A list with X, Y, W, C components each including a matrix
  # or data.table
  # _______________________________________________________
  
  # Filter missing entries
  selected_cols <- c(outcome, covariates, 'treatment', 'b_schoolid')
  data <- SC_Data[, selected_cols, with=FALSE]
  data <- data[complete.cases(data)]
  
  # Setup formula
  fmla <- formula(paste0(outcome, '~', paste(covariates, collapse='+')))
  X <- model.matrix(fmla, data)
  W <- data[, .(treatment)]
  Y <- data[, outcome, with=FALSE]
  C <- data[, .(b_schoolid)]
  
  # Format Y, W, C as numeric vectors
  W <- as.numeric(W[[1]])
  Y <- as.numeric(Y[[1]])
  C <- as.numeric(C[[1]])
  
  list(X = X,Y = Y, W = W, C = C)
}

get_AIPW_scores <- function(var_list, cf) {
  # Get forest predictions. 
  m.hat <- cf$Y.hat  
  e.hat <- cf$W.hat  
  tau.hat <- cf$predictions
  
  # Predicting mu.hat(X[i], 1) and mu.hat(X[i], 0) for obs in held-out sample
  # Note: to understand this, read equations 6-8 in this vignette
  # https://grf-labs.github.io/grf/articles/muhats.html
  mu.hat.0 <- m.hat - e.hat * tau.hat        # E[Y|X,W=0] = E[Y|X] - e(X)*tau(X)
  mu.hat.1 <- m.hat + (1 - e.hat) * tau.hat  # E[Y|X,W=1] = E[Y|X] + (1 - e(X))*tau(X)
  
  # Compute AIPW scores
  aipw.scores <- tau.hat + var_list$W / e.hat * (var_list$Y -  mu.hat.1) - 
    (1 - var_list$W) / (1 - e.hat) * (var_list$Y -  mu.hat.0)
  aipw.scores
}

partial_dependence_single <- function(selected.covariate, covariates, type, X,
                                      causal.forest, grid_size=0){
  # Get data and define other covariates
  data <- as.data.frame(X)
  other.covariates <- covariates[which(covariates != selected.covariate)]
  
  # Define grid
  if (type == 'binary') {
    grid.size <- 2
    covariate.grid <- c(0, 1)
  } else {
    grid.size <- grid_size
    covariate.grid <- seq(min(data[,selected.covariate]), 
                          max(data[,selected.covariate]), length.out=grid.size)
  }
  
  # Take median of other covariates 
  medians <- apply(data[, other.covariates, F], 2, median)
  
  # Construct a dataset
  data.grid <- data.frame(sapply(medians, function(x) rep(x, grid.size)), covariate.grid)
  colnames(data.grid) <- c(other.covariates, selected.covariate)
  
  # Expand the data
  fmla <- formula(paste0('~  ', paste(covariates, collapse = '+')))
  X.grid <- model.matrix(fmla, data.grid)
  
  # Point predictions of the CATE and standard errors 
  forest.pred <- predict(causal.forest, newdata = X.grid, estimate.variance=TRUE)
  tau.hat <- forest.pred$predictions
  tau.hat.se <- sqrt(forest.pred$variance.estimates)
  
  # Plot predictions for each group and 95% confidence intervals around them.
  data.pred <- transform(data.grid, tau.hat=tau.hat, 
                                  ci.low = tau.hat - 2*tau.hat.se, 
                                  ci.high = tau.hat + 2*tau.hat.se)
  ggplot(data.pred) +
    geom_line(aes_string(x=selected.covariate, y="tau.hat", group = 1), color="black") +
    geom_errorbar(aes_string(x=selected.covariate, ymin="ci.low", 
                             ymax="ci.high", width=.2), color="blue") +
    ylab("") +
    ggtitle(paste0("Predicted treatment effect varying '", 
                   selected.covariate, "' (other variables fixed at median)")) +
    scale_x_continuous(selected.covariate, breaks=covariate.grid, 
                       labels=signif(covariate.grid, 2)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold")) 
}

school_level_heterogeneity <- function(var_list, covariates, tau.hat, cf){
  school.mat <- 
    model.matrix(~ b_schoolid + 0, 
                 data = data.frame(var_list$X, b_schoolid = factor(var_list$C)))
  school.size <- colSums(school.mat)
  
  school.X <- (t(school.mat) %*% as.matrix(var_list$X[, covariates])) / school.size
  school.X <- data.frame(school.X)
  colnames(school.X) <- covariates
  
  # Compute doubly robust treatment estimates
  dr.score = tau.hat + var_list$W / cf$W.hat *
    (var_list$Y - cf$Y.hat - (1 - cf$W.hat) * tau.hat) -
    (1 - var_list$W) / (1 - cf$W.hat) * 
    (var_list$Y - cf$Y.hat + cf$W.hat * tau.hat)
  score <- t(school.mat) %*% dr.score / school.size
  
  # Regression forest analysis
  school.forest <- regression_forest(school.X, score)
  school.pred <- predict(school.forest)$predictions
  print(test_calibration(school.forest))
  
  # OLS
  school.DF <- data.frame(school.X, school.score=score)
  print(coeftest(lm(school.score ~ ., data = school.DF), vcov = vcovHC))

}

# 0. Parameters ------------------------------------

# Global student-level controls defined by authors
# Please write below formula without spaces
controls <- 'ageinm+male+refugee+astudent+b_schoolsize+braven_sd+beyes_sd+f_csize'
controls_vec <- strsplit(controls, split='\\+')[[1]]

# Define controls used for each outcome (this adds any controls that are specific to an outcome)
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior
violence.covariates <- c('bsbully_c', 'bstrata', 'b_districtid', controls_vec)

# Outcome 2: Social Exclusion
# ffriend
social.outcome <- 'ffriend'
social.covariates <- c('bfriend', 'bstrata', 'b_districtid', controls_vec)

# Host Emotional support
social.outcome <- 'fhostsupportself'
social.covariates <- c('bhostsupportself', 'bstrata', 'b_districtid', controls_vec)

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation
prosocial.covariates <- c('bstrata', 'b_districtid', controls_vec)

# Outcome 4: Altruism
altruism.covariates <- c('bdonation_sd', 'a2', 'bstrata', 'b_districtid', controls_vec)

# Outcome 5: Achievement Tests
achievement.covariates <- c('bturk_sd', 'bstrata', 'b_districtid', controls_vec)

# 1. Load processed data ------------------------------------
SC_Data <- haven::read_dta('Data/Processed_Data/JS_Stata_Processed.dta')
SC_Data <- as.data.table(SC_Data)
SC_Data <- SC_Data[, b_schoolid := factor(b_schoolid)]

# 2. Pre-specified hypotheses ----------------------
# Social cohesion paper posits 3 subgroups: refugee status, gender and emotional intelligence

# 2.1 Adding interactions to regressions + p-value corrections -----
# Note: this is the method used in the original paper

# They only perform HTE analysis for the following outcomes (Tables 9-13)
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior
m.table9.1 <- lm(formula = paste0(
  'fsbully_c ~ refugee* treatment + bsbully_c  + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data)

# Outcome 2: Social Exclusion
m.table10.1 <- lm(formula = paste0(
  'ffriend ~ refugee* treatment + bfriend  + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data)

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation
m.table11.2 <- lm(formula = paste0(
  'fs_decision_out ~ refugee* treatment  + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data)

# Outcome 4: Altruism
m.table12.1 <- lm(formula = paste0(
  'fdonate ~ a2*treatment + factor(bstrata) + factor(b_districtid) + bdonation_sd +', controls),
  data = SC_Data, subset = SC_Data$refugee == 0)

m.table12.2 <- lm(formula = paste0(
  'fdonate ~ a2*treatment + factor(bstrata) + factor(b_districtid)  + bdonation_sd +', controls),
  data = SC_Data, subset = SC_Data$refugee == 1)

m.table12.3 <- lm(formula = paste0(
  'fdonation_perc ~ a2*treatment + factor(bstrata) + factor(b_districtid) + bdonation_sd +', controls),
  data = SC_Data, subset = SC_Data$refugee == 0)

m.table12.4 <- lm(formula = paste0(
  'fdonation_perc ~ a2*treatment + factor(bstrata) + factor(b_districtid) + bdonation_sd +', controls),
  data = SC_Data, subset = SC_Data$refugee == 1)

# Outcome 5: Achievement Tests
m.table13.1 <- lm(formula = paste0(
  'fturk_sd ~ treatment + bturk_sd + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data, subset = SC_Data$refugee == 0)

m.table13.2 <- lm(formula = paste0(
  'fturk_sd ~ treatment + bturk_sd + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data, subset = SC_Data$refugee == 1)

m.table13.3 <- lm(formula = paste0(
  'fmath_sd ~ treatment + bmath_sd + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data, subset = SC_Data$refugee == 0)

m.table13.4 <- lm(formula = paste0(
  'fmath_sd ~ treatment + bmath_sd + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data, subset = SC_Data$refugee == 1)

# 2.2 AIPW scores and regressing on subgroup membership ------------

# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior
#get_AIPW_scores()

# Outcome 2: Social Exclusion
#get_AIPW_scores()

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation
#get_AIPW_scores()

# Outcome 4: Altruism
#altruism.aipw.scores <- get_AIPW_scores(outcome = 'fdonate', covariates = altruism.covariates)

# Outcome 5: Achievement Tests
#achievement.aipw.scores <- get_AIPW_scores(outcome = 'fturk_sd', covariates = altruism.covariates)

# 3. Data-driven hypotheses ----------------------------
# 3.1 Causal trees with clustering as in Athey & Wager (2019) ------------
 # TODO Eric
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism

# Outcome 5: Achievement Tests

# 3.2 Causal forests -------------------------------------

# * 3.2.1 Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior -------
# * fsbully_c outcome
# Fit causal tree
violence_list <- generate_X_Y_W_C(outcome = 'fsbully_c', covariates = violence.covariates)
violence.n <- dim(violence_list$X)[1]
violence.cf <- causal_forest(X = violence_list$X, Y = violence_list$Y, 
                             W = violence_list$W, clusters = violence_list$C)

# CATE histogram 
violence.tau.hat <- predict(violence.cf)$predictions
hist(violence.tau.hat, main="Violence outcome: CATE estimates", freq=F)

# Variable importance
violence.var_imp <- c(variable_importance(violence.cf))
names(violence.var_imp) <- violence.covariates # TODO What IS NA? The clusters?
violence.sorted_var_imp <- sort(violence.var_imp, decreasing = TRUE)

# Data-driven subgroups
# TODO pending: How to deal with use of clusters argument for folds vs. for clusters?

# Best linear projection
best_linear_projection(violence.cf, violence_list$X)

# Calibration
test_calibration(violence.cf)

# Compare regions with above/below median CATEs (from Wager & Athey)
high_effect <- violence.tau.hat > median(violence.tau.hat)
ate.high <- average_treatment_effect( violence.cf , subset = high_effect )
ate.low <- average_treatment_effect( violence.cf, subset =! high_effect )
paste ("95% CI for difference in ATE:",
       round(ate.high[1] - ate.low[1] , 3) , "+/-",
       round(qnorm(0.975) * sqrt ( ate.high[2]^2 + ate.low[2]^2) , 3))

# Partial dependence
partial_dependence_single(selected.covariate = 'refugee', 
                          covariates = violence.covariates, 
                          type = 'binary', X = violence_list$X,
                          causal.forest = violence.cf)

# * 3.2.2 Outcome 2: Social Exclusion ------------------------------
# * fsbully_c outcome
# Fit causal tree
social_list <- generate_X_Y_W_C(outcome = social.outcome, covariates = social.covariates)
social.n <- dim(social_list$X)[1]
social.cf <- causal_forest(X = social_list$X, Y = social_list$Y, 
                             W = social_list$W, clusters = social_list$C)

# CATE histogram 
social.tau.hat <- predict(social.cf)$predictions
hist(social.tau.hat, main="Social Exclusion outcome: CATE estimates", freq=F)

# Variable importance
social.var_imp <- c(variable_importance(social.cf))
names(social.var_imp) <- social.covariates # TODO What IS NA? The clusters?
social.sorted_var_imp <- sort(social.var_imp, decreasing = TRUE)

# Data-driven subgroups
# TODO pending: How to deal with use of clusters argument for folds vs. for clusters?

# Best linear projection
best_linear_projection(social.cf, social_list$X)

# Calibration
test_calibration(social.cf)

# Compare regions with above/below median CATEs (from Wager & Athey)
high_effect <- social.tau.hat > median(social.tau.hat)
ate.high <- average_treatment_effect( social.cf , subset = high_effect )
ate.low <- average_treatment_effect( social.cf, subset =! high_effect )
paste ("95% CI for difference in ATE:",
       round(ate.high[1] - ate.low[1] , 3) , "+/-",
       round(qnorm(0.975) * sqrt ( ate.high[2]^2 + ate.low[2]^2) , 3))

# Partial dependence
partial_dependence_single(selected.covariate = 'refugee', 
                          covariates = social.covariates, 
                          type = 'binary', X = social_list$X,
                          causal.forest = social.cf)

partial_dependence_single(selected.covariate = 'male', 
                          covariates = social.covariates, 
                          type = 'binary', X = social_list$X,
                          causal.forest = social.cf)

partial_dependence_single(selected.covariate = 'braven_sd', 
                          covariates = social.covariates, 
                          type = 'non-binary', X = social_list$X,
                          causal.forest = social.cf, grid_size = 5)

partial_dependence_single(selected.covariate = 'beyes_sd', 
                          covariates = social.covariates, 
                          type = 'non-binary', X = social_list$X,
                          causal.forest = social.cf, grid_size = 5)

# Regress AIPW scores on group membership
social.aipw <- get_AIPW_scores(social_list, social.cf)

social.aipw.ols <- lm(formula = 'social.aipw ~ refugee', 
          data = transform(social_list$X, aipw.scores = social.aipw))
social.ols.res <- coeftest(social.aipw.ols, vcov = vcovHC(social.aipw.ols, "HC2"))

social.aipw.ols <- lm(formula = 'social.aipw ~ I(braven_sd < -0.6)', 
                      data = transform(social_list$X, aipw.scores = social.aipw))
social.ols.res <- coeftest(social.aipw.ols, vcov = vcovHC(social.aipw.ols, "HC2"))

social.aipw.ols <- lm(formula = 'social.aipw ~ male', 
                      data = transform(social_list$X, aipw.scores = social.aipw))
social.ols.res <- coeftest(social.aipw.ols, vcov = vcovHC(social.aipw.ols, "HC2"))

social.aipw.ols <- lm(formula = 'social.aipw ~ beyes_sd', 
                      data = transform(social_list$X, aipw.scores = social.aipw))
social.ols.res <- coeftest(social.aipw.ols, vcov = vcovHC(social.aipw.ols, "HC2"))


# * 3.2.3 Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation ----------------
prosocial_list <- generate_X_Y_W_C(outcome = 'fs_decision_in', covariates = prosocial.covariates)
prosocial.n <- dim(prosocial_list$X)[1]
prosocial.cf <- causal_forest(X = prosocial_list$X, Y = prosocial_list$Y, 
                           W = prosocial_list$W, clusters = prosocial_list$C)

# CATE histogram 
prosocial.tau.hat <- predict(prosocial.cf)$predictions
hist(prosocial.tau.hat, main="Prosocial Behavior outcome: CATE estimates", freq=F)

# Variable importance
prosocial.var_imp <- c(variable_importance(prosocial.cf))
names(prosocial.var_imp) <- prosocial.covariates # TODO What IS NA? The clusters?
prosocial.sorted_var_imp <- sort(prosocial.var_imp, decreasing = TRUE)

# Data-driven subgroups
# TODO pending: How to deal with use of clusters argument for folds vs. for clusters?

# Best linear projection
best_linear_projection(prosocial.cf, prosocial_list$X)

# Calibration
test_calibration(prosocial.cf)

# Compare regions with above/below median CATEs (from Wager & Athey)
high_effect <- prosocial.tau.hat > median(prosocial.tau.hat)
ate.high <- average_treatment_effect( prosocial.cf , subset = high_effect )
ate.low <- average_treatment_effect( prosocial.cf, subset =! high_effect )
paste ("95% CI for difference in ATE:",
       round(ate.high[1] - ate.low[1] , 3) , "+/-",
       round(qnorm(0.975) * sqrt ( ate.high[2]^2 + ate.low[2]^2) , 3))

# Partial dependence
partial_dependence_single(selected.covariate = 'refugee', 
                          covariates = prosocial.covariates, 
                          type = 'binary', X = prosocial_list$X,
                          causal.forest = prosocial.cf)

partial_dependence_single(selected.covariate = 'braven_sd', 
                          covariates = prosocial.covariates, 
                          type = 'non-binary', X = prosocial_list$X,
                          causal.forest = prosocial.cf, grid_size = 5)

# * 3.2.4 Outcome 4: Altruism -------------------------------
# * fdonate outcome
# Fit causal tree
altruism_list <- generate_X_Y_W_C(outcome = 'fdonate', covariates = altruism.covariates)
altruism.n <- dim(altruism_list$X)[1]
altruism.cf <- causal_forest(X = altruism_list$X, Y = altruism_list$Y, 
                             W = altruism_list$W, clusters = altruism_list$C)

# CATE histogram 
altruism.tau.hat <- predict(altruism.cf)$predictions
hist(altruism.tau.hat, main="Altruism outcome: CATE estimates", freq=F)

# Variable importance
altruism.var_imp <- c(variable_importance(altruism.cf))
names(altruism.var_imp) <- altruism.covariates # TODO What IS NA? The clusters?
altruism.sorted_var_imp <- sort(altruism.var_imp, decreasing = TRUE)

# Data-driven subgroups
# TODO pending: How to deal with use of clusters argument for folds vs. for clusters?

# Best linear projection
best_linear_projection(altruism.cf, altruism_list$X)

# Calibration
test_calibration(altruism.cf)

# Compare regions with above/below median CATEs (from Wager & Athey)
high_effect <- altruism.tau.hat > median(altruism.tau.hat)
ate.high <- average_treatment_effect( altruism.cf , subset = high_effect )
ate.low <- average_treatment_effect( altruism.cf, subset =! high_effect )
paste ("95% CI for difference in ATE:",
       round(ate.high[1] - ate.low[1] , 3) , "+/-",
       round(qnorm(0.975) * sqrt ( ate.high[2]^2 + ate.low[2]^2) , 3))

# Partial dependence
partial_dependence_single(selected.covariate = 'refugee', 
                          covariates = altruism.covariates, 
                          type = 'binary', X = altruism_list$X,
                          causal.forest = altruism.cf)

partial_dependence_single(selected.covariate = 'braven_sd', 
                          covariates = altruism.covariates, 
                          type = 'non-binary', X = altruism_list$X,
                          causal.forest = altruism.cf, grid_size = 5)

# * 3.2.5 Outcome 5: Achievement Tests --------------------------------
# Fit causal tree
achievement_list <- generate_X_Y_W_C(outcome = 'fturk_sd', covariates = achievement.covariates)
achievement.n <- dim(achievement_list$X)[1]
achievement.cf <- causal_forest(X = achievement_list$X, Y = achievement_list$Y, 
                             W = achievement_list$W, clusters = achievement_list$C)

# CATE histogram 
achievement.tau.hat <- predict(achievement.cf)$predictions
hist(achievement.tau.hat, main="Achievement outcome: CATE estimates", freq=F)

# Variable importance
achievement.var_imp <- c(variable_importance(achievement.cf))
names(achievement.var_imp) <- achievement.covariates # TODO What IS NA? The clusters?
achievement.sorted_var_imp <- sort(achievement.var_imp, decreasing = TRUE)

# Data-driven subgroups
# TODO pending

# Best linear projection
best_linear_projection(achievement.cf, achievement_list$X)

# Calibration
test_calibration(achievement.cf)

# Compare regions with above/below median CATEs (from Wager & Athey)
high_effect <- achievement.tau.hat > median(achievement.tau.hat)
ate.high <- average_treatment_effect( achievement.cf , subset = high_effect )
ate.low <- average_treatment_effect( achievement.cf, subset =! high_effect )
paste ("95% CI for difference in ATE:",
       round(ate.high[1] - ate.low[1] , 3) , "+/-",
       round(qnorm(0.975) * sqrt ( ate.high[2]^2 + ate.low[2]^2) , 3))

# Partial dependence
partial_dependence_single(selected.covariate = 'refugee', 
                          covariates = achievement.covariates, 
                          type = 'binary', X = achievement_list$X,
                          causal.forest = achievement.cf)

partial_dependence_single(selected.covariate = 'braven_sd', 
                          covariates = achievement.covariates, 
                          type = 'non-binary', X = achievement_list$X,
                          causal.forest = achievement.cf, grid_size = 5)

# 4. School level heterogeneity Wager & Athey (2019; p. 9) --------
# Create school-level covariates
# Note that we use max here because some rows have NAs
SC_Data_schools <- SC_Data[, .(
  perpetrator = head(perpetrator, 1),
  victim = head(victim, 1),
  events = head(events, 1),
  treatment = head(treatment, 1), 
  bstudentnum_2 = max(bstudentnum_2, na.rm = TRUE),
  bstudentnum_3 = max(bstudentnum_3, na.rm = TRUE),
  n_class = uniqueN(b_classid),
  bactive_syrian_2 = max(bactive_syrian_2, na.rm = TRUE), 
  bactive_syrian_3 = max(bactive_syrian_3, na.rm = TRUE),
  b_provinceid = head(b_provinceid, 1),
  b_districtid = head(b_districtid, 1),
  bstrata = max(bstrata)
), by = .(b_schoolid)]

# School size
SC_Data_schools <- SC_Data_schools[, b_schoolsize := bstudentnum_2 + bstudentnum_3]
SC_Data_schools <- SC_Data_schools[, srefshare := (bactive_syrian_2 + bactive_syrian_3) / b_schoolsize]

# * 4.1 Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior -------
school_level_heterogeneity(var_list = violence_list, 
                           covariates = c('refugee', 'braven_sd', 'beyes_sd', 'male'), 
                           tau.hat = violence.tau.hat, cf = violence.cf)

# * 4.1 Outcome 2: Social Exclusion -------
school_level_heterogeneity(var_list = social_list, 
                           covariates = c('refugee'), 
                           tau.hat = social.tau.hat, cf = social.cf)

# * 4.4 Outcome 4: Altruism ------------------------
school_level_heterogeneity(var_list = altruism_list, 
                           covariates = c('refugee', 'braven_sd', 'beyes_sd', 'male'), 
                           tau.hat = altruism.tau.hat, cf = altruism.cf)

# 5. Identify the role of cluster-robustness as in Wager & Athey (2019; p. 10) -------------
# Note: in the paper they add w.hat and y.hat estimates using the clusters, not sure why
# and also tune.parameters = "all"

# * 5.1 Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior -------
violence.X.adj <- violence_list$X[, !colnames(violence_list$X) %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')]
violence.cf.noclust <- 
  causal_forest(X = violence.X.adj, 
                Y = violence_list$Y, W = violence_list$W)

violence.ATE.noclust <- average_treatment_effect(violence.cf.noclust)
paste("95% CI for the ATE:", round(violence.ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * violence.ATE.noclust[2], 3))

best_linear_projection(violence.cf.noclust, violence.X.adj)

test_calibration(violence.cf.noclust)

# * 5.1 Outcome 2: Social Exclusion ------------------------
social.X.adj <- social_list$X[, !colnames(social_list$X) %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')]
social.cf.noclust <- 
  causal_forest(X = social.X.adj, 
                Y = social_list$Y, W = social_list$W)

social.ATE.noclust <- average_treatment_effect(social.cf.noclust)
paste("95% CI for the ATE:", round(social.ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * social.ATE.noclust[2], 3))

best_linear_projection(social.cf.noclust, social.X.adj)

test_calibration(social.cf.noclust)

# * 5.4 Outcome 4: Altruism ---------------------------------------------
# Note: remove school-level parameters from X matrix due to lack of overlap
altruism.cf.noclust <- 
  causal_forest(X = altruism_list$X[, !colnames(altruism_list$X) %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')], 
                Y = altruism_list$Y, W = altruism_list$W)

altruism.ATE.noclust <- average_treatment_effect(altruism.cf.noclust)
paste("95% CI for the ATE:", round(altruism.ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * altruism.ATE.noclust[2], 3))

test_calibration(altruism.cf.noclust)

