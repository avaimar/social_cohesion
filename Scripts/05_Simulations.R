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

# 1. Load processed data ------------------------------------
SC_Data <- haven::read_dta('Data/Processed_Data/JS_Stata_Processed.dta')
SC_Data <- as.data.table(SC_Data)
SC_Data <- SC_Data[, b_schoolid := factor(b_schoolid)]

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
p <- 10

# School sizes (heterogeneous / homogeneous)
N_si_het <- SC_Data_schools$b_schoolsize
N_si_hom <- floor(mean(N_si_het))
N_si <- N_si_het

# Class sizes (heterogeneous / homogeneous)
N_ci_het <- SC_Data_classes$class_size
N_c <- length(N_ci_het)
N_ci_hom <- rep(floor(mean(N_ci_het)), times = N_c)
N_ci <- N_ci_het

# 4. Base case: school-level randomization ---------------------
# Build dataset
base_data <- data.table(class_id = SC_Data$b_classid, school_id = SC_Data$b_schoolid)
base_data <- cbind(base_data, matrix(runif( dim(base_data)[1] * p), dim(base_data)[1], p))
N <- dim(base_data)[1]

# Define treatment
SC_Data_schools <- SC_Data_schools[, W := rbinom(dim(SC_Data_schools)[1], 1, 0.5)]
ind_data <- merge(base_data, SC_Data_schools[, .(b_schoolid, W)], all.x = TRUE, 
                  by.x = 'school_id', by.y = 'b_schoolid')

# Define outcome
ind_data <- ind_data[, Y := .5*(V1 - .5) # base effect
                     + (V2 - .5)*W # treatment effect
                     + .1 * rnorm(N) ] # noise

m.base <- lm(formula = paste0('Y ~ W + ', paste0('V' ,c(1:10), collapse='+')),
             data = ind_data)

# 4. Student-randomization with heterogeneous class sizes ---------------------
# With interference (as a function of the number of treated students in the classroom)

# Define treatment
ind_data <- copy(base_data[, W := rbinom(N, 1, .5)])

# Compute number of treated classmates 
class_data <- ind_data[, .(W_class = sum(W), N_class = .N), by = .(class_id)]
ind_data <- merge(ind_data, class_data, all.x = TRUE)
ind_data <- ind_data[, W_perc := W_class / N_class]

# Define outcome
ind_data <- ind_data[, Y_int := .5*(V1 - .5) # base effect
                     + (V2 - .5)*W * W_perc # treatment + interference effect
                     + .1 * rnorm(N) ] # noise

# Without interference
ind_data <- ind_data[, Y_nint := .5*(V1 - .5) # base effect
                     + (V2 - .5)*W  # treatment effect
                     + .1 * rnorm(N) ] # noise




# 5. Student-randomization with homogeneous class sizes ---------------------
#ind_data <- data.table(class_id = rep(1:N_c, times = N_ci), school_id = )


# Play with intereference at class/school level
