## Script: 02_HTEs.R ------------------------------------


# Working setup ---------------------------------
library(data.table)
library(ggplot2)
library(grf)
library(haven) # read .dta files
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
  W <- W[[1]]
  Y <- Y[[1]]
  C <- C[[1]]
  
  list(X = X,Y = Y, W = W, C = C)
}

get_AIPW_scores <- function(outcome, covariates) {
  # TODO Jake
}

# 0. Parameters ------------------------------------

# Global controls defined by authors at the student-level
# Please write below formula without spaces
controls <- 'ageinm+male+refugee+astudent+b_schoolsize+braven_sd+beyes_sd+f_csize'
controls_vec <- strsplit(controls, split='\\+')[[1]]

# Define controls used for each outcome (this adds any controls that are specific to an outcome)
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism
altruism.covariates <- c('bdonation_sd', controls_vec)

# Outcome 5: Achievement Tests
achievement.covariates <- c(controls_vec)

# 1. Load processed data ------------------------------------
SC_Data <- haven::read_dta('Data/Processed_Data/JS_Stata_Processed.dta')
SC_Data <- as.data.table(SC_Data)

# 2. Pre-specified hypotheses ----------------------
# Social cohesion paper posits 3 subgroups: refugee status, gender and emotional intelligence

# 2.1 Adding interactions to regressions + p-value corrections -----
# Note: this is the method used in the original paper

# They only perform HTE analysis for the following outcomes (Tables 9-13)
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

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
 
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism

# Outcome 5: Achievement Tests

# 3.2 Causal forests -------------------------------------

# * 3.2.1 Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior -------

# * 3.2.2 Outcome 2: Social Exclusion ------------------------------

# * 3.2.3 Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation ----------------

# * 3.2.4 Outcome 4: Altruism -------------------------------
# Fit causal tree
altruism_list <- 
  generate_X_Y_W_C(outcome = 'fdonate', covariates = altruism.covariates)
altruism.cf <- 
  causal_forest(X = altruism_list$X, Y = altruism_list$Y, 
                W = altruism_list$W, clusters = altruism_list$C)

# CATE histogram 
altruism.tau.hat <- predict(altruism.cf)$predictions
hist(altruism.tau.hat, main="Altruism outcome: CATE estimates", freq=F)

# Variable importance
altruism.var_imp <- c(variable_importance(altruism.cf))
names(altruism.var_imp) <- altruism.covariates # TODO What IS NA? The clusters?
altruism.sorted_var_imp <- sort(altruism.var_imp, decreasing = TRUE)

# Best linear projection
best_linear_projection(altruism.cf, altruism_list$X)

# Calibration
test_calibration(altruism.cf)

# Partial dependence
altruism.selected.covariate <- 'refugee'
altruism.other.covariates <- 
  altruism.covariates[which(altruism.covariates != altruism.selected.covariate)]

altruism.covariate.grid <- c(0, 1)
# TODO pending partial dependence

# * 3.2.5 Outcome 5: Achievement Tests --------------------------------

# 4. Comparison of heterogeneity at the school vs. student levels --------

# 5. Identify the role of cluster-robustness as in Wager & Athey (2019; p. 8) -------------


