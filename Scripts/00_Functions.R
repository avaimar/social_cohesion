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
                                 
  generate_X_Y_W_C_df <- function(outcome, covariates, df, treatment) {
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
  SC_table <- as.data.table(df)
  selected_cols <- c(outcome, covariates, 'treatment')
  data <- SC_table[, selected_cols, with=FALSE]
  data <- na.omit(data)
  write.csv(data,"unfactored.csv")
  data$b_schoolid = factor(data$b_schoolid)
  data$bstrata = factor(data$bstrata)
  
  # Setup formula
  fmla <- formula(paste0(outcome, '~', paste(covariates, collapse='+')))
  X <- model.matrix(fmla, data)
  W <- data[, 'treatment']
  #Y <- data[, outcome]
  Y <- data[, outcome, with=FALSE]
  C <- data[, 'b_schoolid']
  
  # Format Y, W, C as numeric vectors
  W <- as.numeric(W[[1]])
  Y <- as.numeric(Y[[1]])
  C <- as.numeric(C[[1]])
  list(X = X,Y = Y, W = W, C = C)
}

run_AIPW <- function(outcome,income,covariates,treatment,df) {
  # _______________________________________________________
  # Runs AIPW based on grf on a dataframe.
  # Inputs:
  # - outcome: (str) outcome column name in SC_Data
  # - income: (str) baseline var name in SC_Data
  # - covariates: (vector of str) covariate column names
  # - treatment (str) treatment var name
  # - df: dataframe
  # Returns:
  # AIPW estimate and std.err
  # _______________________________________________________
  covariates <- c(covariates,income)
  list_data <- generate_X_Y_W_C_df(outcome,covariates,df,treatment)
  forest <- causal_forest(
    X=list_data$X,  
    W=list_data$W,
    Y=list_data$Y,
    clusters = list_data$C,
    W.hat=.5,  # In randomized settings, set W.hat to the (known) probability of assignment
    num.trees = 100)
  forest.ate <- average_treatment_effect(forest,target.sample="overlap")
  forest.ate
}
