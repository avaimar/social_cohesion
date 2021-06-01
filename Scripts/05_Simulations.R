## Script: 05_Simulations.R ------------------------------------


# Working setup ---------------------------------
library(data.table)
library(grf)
library(glmnet)
library(splines)
library(ggplot2)

# Seed
set.seed(42)

# Helper functions
source('Scripts/00_Functions.R')

get_outcome <- function(base_data, W_column, effect_type, interference ) {
  # Note: interference = 0 is no interference. 
  
  # Base effect
  y <- .5*(base_data$V1 + 5)
  
  # Get treatment assignment
  W <- base_data[, W_column, with = FALSE][[1]]
  
  # Treatment effect
  if (effect_type == 'ATE') {
    tau <-  5 * W
  } else {
    #tau <-  (base_data$V2 + base_data$V3/2 + .1) * W
    stop('CATE not yet implemented.')
  }
  
  # Interference effect
  tau <- tau * (1 + interference * base_data$W_perc)
  
  y <- y + tau
  
  # Noise
  y <- y + .1 * rnorm(dim(base_data)[1])
  
  y
}

get_aipw <- function(outcome, treatment, data, prob) {
  fmla <- formula(paste0(outcome, '~', paste0('V' ,c(1:p), collapse='+')))
  if (treatment == 'W_ind') {
    forest <- causal_forest(
      X = model.matrix(fmla, data), W = data[, treatment, with=FALSE][[1]],
      Y = data[, outcome, with=FALSE][[1]], W.hat = prob)
  } else {
    forest <- causal_forest(
      X = model.matrix(fmla, data), W = data[, treatment, with=FALSE][[1]],
      Y = data[, outcome, with=FALSE][[1]], W.hat = prob,
      clusters = data[, 'school_id', with=FALSE][[1]]
    )
  }
  average_treatment_effect(forest, target.sample="overlap")
}


# 1. Load processed data ------------------------------------
SC_Data <- haven::read_dta('Data/Processed_Data/JS_Stata_Processed.dta')
SC_Data <- as.data.table(SC_Data)
SC_Data <- SC_Data[, b_schoolid := factor(b_schoolid)]
SC_Data <- SC_Data[, bstrata := factor(bstrata)]

# 2. Aggregate data at school and class levels ----------------------------
# School
SC_Data_schools <- SC_Data[, .(
  bstudentnum_2 = max(bstudentnum_2, na.rm = TRUE),
  bstudentnum_3 = max(bstudentnum_3, na.rm = TRUE)
), by = .(b_schoolid)]
SC_Data_schools <- SC_Data_schools[, b_schoolsize := bstudentnum_2 + bstudentnum_3]

# Class
SC_Data_classes <- SC_Data[, .(
  class_size = max(f_csize, na.rm = TRUE)
), by = .(b_schoolid, b_classid)]

# 3. Base parameters ----------------------------------
N_s <- 80
p <- 3

# School sizes (heterogeneous / homogeneous)
N_si_het <- SC_Data_schools$b_schoolsize
N_si_hom <- floor(mean(N_si_het))
N_si <- N_si_het

# Class sizes (heterogeneous / homogeneous)
N_ci_het <- SC_Data_classes$class_size
N_c <- length(N_ci_het)
N_ci_hom <- rep(floor(mean(N_ci_het)), times = N_c)
N_ci <- N_ci_het

# 4. Simulation setup ---------------------------------
# Our main comparison of interest is school vs. individual randomization

# Build dataset
base_data <- data.table(class_id = SC_Data$b_classid, school_id = SC_Data$b_schoolid)
base_data <- cbind(base_data, matrix(runif( dim(base_data)[1] * p, min = 0, max = 100),
                                     dim(base_data)[1], p))
N <- dim(base_data)[1]
base_data <- merge(base_data, SC_Data_schools[, .(b_schoolid, b_schoolsize)], all.x = TRUE, 
                   by.x = 'school_id', by.y = 'b_schoolid')

# Add school-level randomization treatment assignment
SC_Data_schools <- SC_Data_schools[, W_sch := rbinom(dim(SC_Data_schools)[1], 1, 0.5)]
base_data <- merge(base_data, SC_Data_schools[, .(b_schoolid, W_sch)], all.x = TRUE, 
                  by.x = 'school_id', by.y = 'b_schoolid')

# Add individual-level randomization treatment assignment
base_data <- base_data[, W_ind := rbinom(N, 1, .5)]

# Compute number of treated classmates 
class_data <- base_data[, .(W_class = sum(W_ind), N_class = .N), by = .(class_id)]
base_data <- merge(base_data, class_data, all.x = TRUE, by = 'class_id')
base_data <- base_data[, W_perc := W_class / N_class]

# 5. Experiment 1: Interference treatment effect proportion ----------------------
# Note: define interference as a function of the number of treated students in the classroom

exp1_data <- copy(base_data)

# Define outcome
exp1_data <- exp1_data[, Y_ind_i0 := get_outcome(exp1_data, W_column = 'W_ind',
                                               effect_type = 'ATE', interference = 0)]

exp1_data <- exp1_data[, Y_sch_i0 := get_outcome(exp1_data, W_column = 'W_sch',
                                                 effect_type = 'ATE', interference = 0)]

exp1_data <- exp1_data[, Y_ind_i1 := get_outcome(exp1_data, W_column = 'W_ind',
                                                 effect_type = 'ATE', interference = 1)]

exp1_data <- exp1_data[, Y_sch_i1 := get_outcome(exp1_data, W_column = 'W_sch',
                                                 effect_type = 'ATE', interference = 1)]

exp1_data <- exp1_data[, Y_ind_im1 := get_outcome(exp1_data, W_column = 'W_ind',
                                                 effect_type = 'ATE', interference = -1)]

exp1_data <- exp1_data[, Y_sch_im1 := get_outcome(exp1_data, W_column = 'W_sch',
                                                 effect_type = 'ATE', interference = -1)]

# Compare confidence intervals
exp1_intervals <- data.table(outcome = character(), treatment = character(),
                             estimate=numeric(), std.err=numeric())

for (base_outcome in c('i0', 'i1', 'im1')) {
  for (base_treatment in c('ind', 'sch')) {
    # Define outcome and treatment
    outcome <- paste0('Y_', base_treatment, '_', base_outcome)
    treatment <- paste0('W_', base_treatment)
    
    # Get AIPW
    aipw <- get_aipw(outcome = outcome, treatment = treatment, data = exp1_data, prob = 0.5)
    
    # Add to table
    exp1_intervals <- rbind(exp1_intervals, list(outcome, treatment, aipw[1], aipw[2]))
  }
}

ggplot(data = exp1_intervals, aes())
