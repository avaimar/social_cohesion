## Script: 02_HTEs.R ------------------------------------


# 0. Working setup ---------------------------------
library(data.table)
library(ggplot2)
library(statar)

# Parameters
controls <- 'ageinm + male + refugee + astudent + b_schoolsize'

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
# TODO pending 
causal_forest( clusters = SC_Data$b_schoolid)

# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism


# Outcome 5: Achievement Tests

# 3. Data-driven hypotheses ----------------------------
# 3.1 Causal trees with clustering as in Athey & Wager (2019) ------------
 
# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism


# Outcome 5: Achievement Tests

# 3.2 Causal forests -------------------------------------

# Outcome 1: Student and Teacher Reports of Violence and Antisocial Behavior

# Outcome 2: Social Exclusion

# Outcome 3: Prosocial Behavior: Trust, Reciprocity and Cooperation

# Outcome 4: Altruism


# Outcome 5: Achievement Tests

# 4. Comparison of heterogeneity at the school vs. student levels --------



