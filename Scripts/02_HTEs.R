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
source('Scripts/00_Functions.R')

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

# Add original paper HTEs
HTE_refugee_comparison <- data.table(main_outcome=character(),outcome=character(), type=character(), estimate=numeric(),stderr=numeric())
HTE_refugee_comparison <- 
  rbind(HTE_refugee_comparison, 
        list('Violence and Antisocial Behavior', 'In-class \nbullying', 'Alan et al.', -0.062, 0.03)) # p. 41
#HTE_refugee_comparison <- 
#  rbind(HTE_refugee_comparison, 
#        list('Violence and Antisocial Behavior', 'Teacher Reported Behavioral Conduct', 'Alan et al.', 0.006, 0.09)) # p. 41
HTE_refugee_comparison <- 
  rbind(HTE_refugee_comparison, 
        list('Social Exclusion', 'Host Emotional \nSupport', 'Alan et al.', 0.080, 0.05)) # p. 42
HTE_refugee_comparison <- 
  rbind(HTE_refugee_comparison, list('Outcome 3: Prosocial Behavior', 'Trust', 'Alan et al.', 0.079, 0.11)) # p. 43
HTE_refugee_comparison <- 
  rbind(HTE_refugee_comparison, list('Altruism', 'Willingness to donate', 'Alan et al.', 0.043, 0.04)) # p. 44
HTE_refugee_comparison <- 
  rbind(HTE_refugee_comparison, list('Academic Achievement', 'Turkish', 'Alan et al.', 0.130, 0.05)) # p. 44


# 1. Load processed data ------------------------------------
SC_Data <- haven::read_dta('Data/Processed_Data/JS_Stata_Processed.dta')
SC_Data <- as.data.table(SC_Data)
SC_Data <- SC_Data[, b_schoolid := factor(b_schoolid)]
SC_Data <- SC_Data[, bstrata := factor(bstrata)]

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

m.table10.3 <- lm(formula = paste0(
  'fhostsupportself ~ refugee*treatment + bhostsupportself  + factor(bstrata) + factor(b_districtid) +', controls),
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
                             W = violence_list$W, clusters = violence_list$C, W.hat = .5)

# CATE histogram 
violence.tau.hat <- predict(violence.cf)$predictions
hist(violence.tau.hat, main="Violence outcome: CATE estimates", freq=F)

# Variable importance
violence.var_imp <- c(variable_importance(violence.cf))
names(violence.var_imp) <- violence.covariates
violence.sorted_var_imp <- sort(violence.var_imp, decreasing = TRUE)

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

# Regress AIPW scores on group membership
violence.aipw <- get_AIPW_scores(violence_list, violence.cf)

violence.aipw.ols <- lm(formula = 'violence.aipw ~ refugee', 
                      data = transform(violence_list$X, aipw.scores = violence.aipw))
violence.ols.res <- coeftest(violence.aipw.ols, vcov = vcovHC(violence.aipw.ols, "HC2"))

HTE_refugee_comparison <- rbind(HTE_refugee_comparison, 
                                list('Violence and Antisocial Behavior', 'In-class \nbullying', 'AIPW', -0.066827, 0.026044))

# * 3.2.2 Outcome 2: Social Exclusion ------------------------------
# Fit causal tree
social_list <- generate_X_Y_W_C(outcome = social.outcome, covariates = social.covariates)
social.n <- dim(social_list$X)[1]
social.cf <- causal_forest(X = social_list$X, Y = social_list$Y, 
                             W = social_list$W, clusters = social_list$C, W.hat = .5)

# CATE histogram 
social.tau.hat <- predict(social.cf)$predictions
hist(social.tau.hat, main="Social Exclusion outcome: CATE estimates", freq=F)

# Variable importance
social.var_imp <- c(variable_importance(social.cf))
names(social.var_imp) <- colnames(social_list$X)
social.sorted_var_imp <- sort(social.var_imp, decreasing = TRUE)

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
                          covariates = colnames(social_list$X)[2:length(colnames(social_list$X))], 
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

HTE_refugee_comparison <- rbind(HTE_refugee_comparison, 
                                list('Social Exclusion', 'Host Emotional \nSupport', 'AIPW', 0.0687247, 0.0311875))


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
                           W = prosocial_list$W, clusters = prosocial_list$C, W.hat = .5)

# CATE histogram 
prosocial.tau.hat <- predict(prosocial.cf)$predictions
hist(prosocial.tau.hat, main="Prosocial Behavior outcome: CATE estimates", freq=F)

# Variable importance
prosocial.var_imp <- c(variable_importance(prosocial.cf))
names(prosocial.var_imp) <- prosocial.covariates
prosocial.sorted_var_imp <- sort(prosocial.var_imp, decreasing = TRUE)

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

# Regress AIPW scores on group membership
prosocial.aipw <- get_AIPW_scores(prosocial_list, prosocial.cf)

prosocial.aipw.ols <- lm(formula = 'prosocial.aipw ~ refugee', 
                        data = transform(prosocial_list$X, aipw.scores = prosocial.aipw))
prosocial.ols.res <- coeftest(prosocial.aipw.ols, vcov = vcovHC(prosocial.aipw.ols, "HC2"))

HTE_refugee_comparison <- rbind(HTE_refugee_comparison, 
                                list('Outcome 3: Prosocial Behavior', 'Trust', 'AIPW', 0.0018356, 0.0908848))


# * 3.2.4 Outcome 4: Altruism -------------------------------
# * fdonate outcome
# Fit causal tree
altruism_list <- generate_X_Y_W_C(outcome = 'fdonate', covariates = altruism.covariates)
altruism.n <- dim(altruism_list$X)[1]
altruism.cf <- causal_forest(X = altruism_list$X, Y = altruism_list$Y, 
                             W = altruism_list$W, clusters = altruism_list$C, W.hat = .5)

# CATE histogram 
altruism.tau.hat <- predict(altruism.cf)$predictions
hist(altruism.tau.hat, main="Altruism outcome: CATE estimates", freq=F)

# Variable importance
altruism.var_imp <- c(variable_importance(altruism.cf))
names(altruism.var_imp) <- altruism.covariates
altruism.sorted_var_imp <- sort(altruism.var_imp, decreasing = TRUE)

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

# Regress AIPW scores on group membership
altruism.aipw <- get_AIPW_scores(altruism_list, altruism.cf)

altruism.aipw.ols <- lm(formula = 'altruism.aipw ~ refugee', 
                        data = transform(altruism_list$X, aipw.scores = altruism.aipw))
altruism.ols.res <- coeftest(altruism.aipw.ols, vcov = vcovHC(altruism.aipw.ols, "HC2"))

HTE_refugee_comparison <- rbind(HTE_refugee_comparison, 
                                list('Altruism', 'Willingness to donate', 'AIPW', -0.033102 , 0.025657))


# * 3.2.5 Outcome 5: Achievement Tests --------------------------------
# Fit causal tree
achievement_list <- generate_X_Y_W_C(outcome = 'fturk_sd', covariates = achievement.covariates)
achievement.n <- dim(achievement_list$X)[1]
achievement.cf <- causal_forest(X = achievement_list$X, Y = achievement_list$Y, 
                             W = achievement_list$W, clusters = achievement_list$C, W.hat = .5)

# CATE histogram 
achievement.tau.hat <- predict(achievement.cf)$predictions
hist(achievement.tau.hat, main="Achievement outcome: CATE estimates", freq=F)

# Variable importance
achievement.var_imp <- c(variable_importance(achievement.cf))
names(achievement.var_imp) <- achievement.covariates
achievement.sorted_var_imp <- sort(achievement.var_imp, decreasing = TRUE)

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

# Regress AIPW scores on group membership
achievement.aipw <- get_AIPW_scores(achievement_list, achievement.cf)

achievement.aipw.ols <- lm(formula = 'achievement.aipw ~ refugee', 
                        data = transform(achievement_list$X, aipw.scores = achievement.aipw))
achievement.ols.res <- coeftest(achievement.aipw.ols, vcov = vcovHC(achievement.aipw.ols, "HC2"))

HTE_refugee_comparison <- rbind(HTE_refugee_comparison, 
                                list('Academic Achievement', 'Turkish', 'AIPW', 0.063745, 0.055048))


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
                           covariates = c('refugee', 'braven_sd', 'beyes_sd', 'male'), 
                           tau.hat = social.tau.hat, cf = social.cf)

# * 4.4 Outcome 4: Altruism ------------------------
school_level_heterogeneity(var_list = altruism_list, 
                           covariates = c('refugee', 'braven_sd', 'beyes_sd', 'male'), 
                           tau.hat = altruism.tau.hat, cf = altruism.cf)

# 5. Identify the role of cluster-robustness as in Wager & Athey (2019; p. 10) -------------

# * 5.1 Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior -------
violence.X.adj <- violence_list$X[, !colnames(violence_list$X) %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')]
violence.cf.noclust <- 
  causal_forest(X = violence.X.adj, 
                Y = violence_list$Y, W = violence_list$W, W.hat = .5)

violence.ATE.noclust <- average_treatment_effect(violence.cf.noclust)
paste("95% CI for the ATE:", round(violence.ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * violence.ATE.noclust[2], 3))

best_linear_projection(violence.cf.noclust, violence.X.adj)

test_calibration(violence.cf.noclust)

# * 5.1 Outcome 2: Social Exclusion ------------------------
social.X.adj <- social_list$X[, !colnames(social_list$X) %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')]
social.cf.noclust <- 
  causal_forest(X = social.X.adj, 
                Y = social_list$Y, W = social_list$W, W.hat = .5)

social.ATE.noclust <- average_treatment_effect(social.cf.noclust)
paste("95% CI for the ATE:", round(social.ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * social.ATE.noclust[2], 3))

best_linear_projection(social.cf.noclust, social.X.adj)

test_calibration(social.cf.noclust)

partial_dependence_single(selected.covariate = 'refugee', 
                          covariates = social.covariates[!social.covariates %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')], 
                          type = 'binary', X = social.X.adj,
                          causal.forest = social.cf.noclust)

social.aipw.noclust <- get_AIPW_scores(
  list(X = social.X.adj, Y = social_list$Y, W = social_list$W, C = social_list$C), 
  social.cf.noclust)

social.aipw.ols.noclust <- lm(formula = 'social.aipw.noclust ~ refugee', 
                      data = transform(social.X.adj, aipw.scores = social.aipw.noclust))
coeftest(social.aipw.ols.noclust, vcov = vcovHC(social.aipw.ols.noclust, "HC2"))

# Using folds (Wager & Athey 2019)
nfold = 5
school.levels = unique(school.id)
cluster.folds = sample.int(nfold, length(school.levels), replace = TRUE)

tau.hat.crossfold = rep(NA, length(Y))
for (foldid in 1:nfold) {
  print(foldid)
  infold = school.id %in% school.levels[cluster.folds == foldid]
  cf.fold = causal_forest(X[!infold, selected.idx], Y[!infold], W[!infold],
                          Y.hat = Y.hat[!infold], W.hat = W.hat[!infold],
                          tune.parameters = "all")
  pred.fold = predict(cf.fold, X[infold, selected.idx])$predictions
  tau.hat.crossfold[infold] = pred.fold
}

cf.noclust.cpy = cf.noclust
cf.noclust.cpy$predictions = tau.hat.crossfold
cf.noclust.cpy$clusters = school.id
test_calibration(cf.noclust.cpy)

Rloss = mean(((Y - Y.hat) - tau.hat * (W - W.hat))^2)
Rloss.noclust = mean(((Y - Y.hat) - tau.hat.noclust * (W - W.hat))^2)
Rloss.crossfold = mean(((Y - Y.hat) - tau.hat.crossfold * (W - W.hat))^2)

c(Rloss.noclust - Rloss, Rloss.crossfold - Rloss)


# * 5.4 Outcome 4: Altruism ---------------------------------------------
# Note: remove school-level parameters from X matrix due to lack of overlap
altruism.cf.noclust <- 
  causal_forest(X = altruism_list$X[, !colnames(altruism_list$X) %in% c("b_schoolsize", 'bstrata', 'b_districtid', 'f_csize')], 
                Y = altruism_list$Y, W = altruism_list$W, W.hat = .5)

altruism.ATE.noclust <- average_treatment_effect(altruism.cf.noclust)
paste("95% CI for the ATE:", round(altruism.ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * altruism.ATE.noclust[2], 3))

test_calibration(altruism.cf.noclust)

# Plot AIPW comparison
ggplot(HTE_refugee_comparison, aes(x= outcome)) +
  geom_errorbar(aes(ymin= estimate - stderr, ymax= estimate+stderr, color = type, group = type), 
                position=position_dodge(), width =0.3) +
  theme_classic() +
  geom_hline(yintercept = 0) + 
  labs(x = "Outcome") +
  scale_color_manual(values = c('steelblue4', 'skyblue2'), name = '') + 
  theme(legend.position = 'bottom')
ggsave(filename = 'Outputs/AIPW_Refugee_comp.png', width = 7.5, height = 2.5, units = "in",
       scale = 1.2)

