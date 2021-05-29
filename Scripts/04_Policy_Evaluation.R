## Script: 04_Policy_Evaluation.R ------------------------------------


# Working setup ---------------------------------
library(data.table)
library(grf)
library(glmnet)
library(splines)
library(ggplot2)

# Helper functions
source('Scripts/00_Functions.R')

# Seed
set.seed(42)

# 0. Parameters ---------------------------------------------
# Cost parameters
cost_per_student <- (20 + 7 + 8)/ 6.5 # In USD (p. 28 of the paper)
experiment_budget <- (20 + 7 + 8) * 1000 # USD

# Global student-level controls defined by authors
# Please write below formula without spaces
controls <- 'ageinm+male+refugee+astudent+b_schoolsize+braven_sd+beyes_sd+f_csize'
controls_vec <- strsplit(controls, split='\\+')[[1]]

# 1. Load processed data ------------------------------------
SC_Data <- haven::read_dta('Data/Processed_Data/JS_Stata_Processed.dta')
SC_Data <- as.data.table(SC_Data)
SC_Data <- SC_Data[, b_schoolid := factor(b_schoolid)]

# 2. Aggregate at the school level --------------------------
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
  bstrata = max(bstrata),
  refugee = sum(refugee),
  fhostsupportself = mean(fhostsupportself, na.rm = TRUE),
  bhostsupportself = mean(bhostsupportself, na.rm = TRUE)
), by = .(b_schoolid)]

# School size and Syrian/Refugee percentages
SC_Data_schools <- SC_Data_schools[, b_schoolsize := bstudentnum_2 + bstudentnum_3]
SC_Data_schools <- SC_Data_schools[, srefshare := (bactive_syrian_2 + bactive_syrian_3) / 
                                     b_schoolsize]
SC_Data_schools <- SC_Data_schools[, refugee_share := refugee / b_schoolsize]
# Note: srefshare and refugee are computed with respect to the 3rd and 4th grade. We assume
# they are representative percentages at the school level

# 3. Policy Evaluation (sample means) ----------------------------

# Selected outcome: Social Exclusion (Host Emotional support)
social.outcome <- 'fhostsupportself'
social.covariates <- c('bhostsupportself', 'bstrata', 'b_districtid', controls_vec)
social_list <- generate_X_Y_W_C(outcome = social.outcome, covariates = social.covariates)
social.n <- dim(social_list$X)[1]
social.cf <- causal_forest(X = social_list$X, Y = social_list$Y, 
                           W = social_list$W, clusters = social_list$C, W.hat = 5)
social.tau.hat <- predict(social.cf)$predictions
social.aipw <- get_AIPW_scores(social_list, social.cf)

# ATE (school-level)
m.school_level <- lm(formula = paste0(
  'fhostsupportself ~ treatment *refugee + bhostsupportself + b_schoolsize'),
  data = SC_Data)

# ATE (individual-level)
m.table10.3.ATE <- lm(formula = paste0(
  'fhostsupportself ~ treatment + bhostsupportself  + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data)

# Original paper reported HTE
m.table10.3 <- lm(formula = paste0(
  'fhostsupportself ~ refugee*treatment + bhostsupportself  + factor(bstrata) + factor(b_districtid) +', controls),
  data = SC_Data)

# HTEs. From social.ols.res in 02_HTEs.R we find the following
refugee_HTE <- 0.0708095 # Compared to table 10.3 we get a somewhat higher baseline effect
base_HTE <- 0.0811981 # Notice from table 10.3 we get almost the exact same HTE for refugees

# 4. Policy Evaluation (AIPW scores) -----------------------------
# Define the policy: treats every student in a school if the school has a refugee share > 0.1
set.seed(42)

# Build our data table 
selected_cols <- c(social.outcome, social.covariates, 'treatment', 'b_schoolid')
social.data <- SC_Data[, selected_cols, with=FALSE]
social.data <- social.data[complete.cases(social.data)]

# Add school-level refugee share information and define treatment as per policy
social.data <- merge(social.data, SC_Data_schools[, .(b_schoolid, refugee_share)], all.x=TRUE)
social.data <- social.data[, pi := ifelse(refugee_share >= 0.1, 1, 0)]
pi <- social.data$pi

# Compute AIPW scores
mu.hat.1 <- social.cf$Y.hat + (1 - social.cf$W.hat) * social.tau.hat
mu.hat.0 <- social.cf$Y.hat - social.cf$W.hat * social.tau.hat

gamma.hat.1 <- mu.hat.1 + social_list$W/social.cf$W.hat * (social_list$Y - mu.hat.1)
gamma.hat.0 <- mu.hat.0 + (1-social_list$W)/(1-social.cf$W.hat) * (social_list$Y - mu.hat.0)

# Compute policy value
gamma.hat.pi <- pi * gamma.hat.1 + (1 - pi) * gamma.hat.0
value.estimate <- mean(gamma.hat.pi)
value.stderr <- sd(gamma.hat.pi) / sqrt(length(gamma.hat.pi))

# Random policy that treats with 45% probability
random_prob <- 0.25

# Randomize at the school level first and then assign to student level
pi.school.random <- copy(SC_Data_schools[, .(b_schoolid)])
pi.school.random <- pi.school.random[, pi_school_random := rbinom(n=80, size =1, prob = random_prob)]
social.data <- merge(social.data, pi.school.random, by = 'b_schoolid')
pi.random <- social.data$pi_school_random

# Compute random policy value
gamma.hat.pi.random <-  pi.random * gamma.hat.1 + (1 - pi.random) * gamma.hat.0
value.estimate.random <- mean(gamma.hat.pi.random)
value.stderr.random <- sd(gamma.hat.pi.random) / sqrt(length(gamma.hat.pi.random))

# To estimate the difference in value between to policies we subtract the scores
diff.scores <- gamma.hat.pi - gamma.hat.pi.random 
diff.estimate <- mean(diff.scores)
diff.stderr <- sd(diff.scores) / sqrt(length(diff.scores))

# Cost difference
pi_cost <- cost_per_student * sum(pi)
pi_random_cost <- cost_per_student * sum(pi.random)

# Results
print(paste("Refugee-share-based policy value estimate:", round(value.estimate, 3),
            "Std. Error:", round(value.stderr, 3)))
print(paste("Random policy value estimate:", round(value.estimate.random, 3),
            "Std. Error:", round(value.stderr.random, 3)))
print(paste("Difference in policy value estimate:", round(diff.estimate, 3), 
            "Std. Error:", round(diff.stderr, 3)))
print("\n")
print(paste0("Refugee-share-based policy cost: ", round(pi_cost, 2), " USD"))
print(paste0("Random policy cost: ", round(pi_random_cost, 2), " USD"))
