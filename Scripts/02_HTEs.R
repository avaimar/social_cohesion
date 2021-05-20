## Script: 02_HTEs.R ------------------------------------


# 0. Working setup ---------------------------------
library(data.table)
library(ggplot2)
library(grf)
library(statar)

# Parameters
controls <- 'ageinm + male + refugee + astudent + b_schoolsize'

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

# 1. Load processed data ------------------------------------
SC_Data <- fread('Data/Processed_Data/ABGK_recreated_variables.csv')


# 2. Pre-specified hypotheses ----------------------
# Social cohesion paper posits 3 subgroups: refugee status, gender and emotional intelligence

# 2.1 Adding interactions to regressions + p-value corrections -----
# Note: this is the method used in the original paper

# They only perform HTE analysis for the following outcomes (Tables 9-13)
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism
m.table12.1 <- 
  lm(formula = paste0('fdonate ~ a2*treatment + factor(bstrata) + factor(b_districtid) +', 
                      controls),
     data = SC_Data, subset = SC_Data$refugee == 0)

m.table12.2 <- 
  lm(formula = paste0('fdonate ~ a2*treatment + factor(bstrata) + factor(b_districtid) +',
                      controls),
     data = SC_Data, subset = SC_Data$refugee == 1)

m.table12.3 <- 
  lm(formula = paste0('fdonation_perc ~ a2*treatment + factor(bstrata) + factor(b_districtid) +', 
                      controls),
     data = SC_Data, subset = SC_Data$refugee == 0)

m.table12.4 <- 
  lm(formula = paste0('fdonation_perc ~ a2*treatment + factor(bstrata) + factor(b_districtid) +', 
                      controls),
     data = SC_Data, subset = SC_Data$refugee == 1)

# Outcome 5: Achievement Tests


# 2.2 AIPW scores and regressing on subgroup membership ------------
# TODO Check how we correct for clusters, this is done in Wager & Athey (2019)


# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism
#fmla <- paste0('fdonate ~ ')
#causal_forest( clusters = SC_Data$b_schoolid)

# Outcome 5: Achievement Tests

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
# Define covariates
altruism.covariates <- c('ageinm', 'male', 'refugee', 'astudent', 'b_schoolsize')

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



