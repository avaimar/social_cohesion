
cohesion_data <- cohesion_data #swap out your name of dataframe
final_var <- 'ffriend' #Name of Endline variable you want to look at
baseline_var <-'bfriend' #Name of Baseline variable you want to look at
cohesion_columns<-cohesion_data[,c('treatment',final_var,baseline_var, 'ageinm' , 'male' , 
                                   'refugee' , 'astudent' , 'braven_sd' , 'beyes_sd' , 'b_schoolsize', 
                                   'f_csize','b_schoolid')]
final_cohesion<-na.omit(cohesion_columns)
covariates <- c(baseline_var, 'ageinm' , 'male' , 
                'refugee' , 'astudent' , 'braven_sd' , 'beyes_sd' , 'b_schoolsize', 
                'f_csize','b_schoolid')
treatment <- 'treatment'
outcome <- final_var
fmla <- as.formula(paste0("~", paste0("bs(", covariates, ", df=3)", collapse="+")))
W <- unlist(final_cohesion[,treatment])
Y <- final_cohesion[,outcome]
XX2 <- model.matrix(fmla, final_cohesion)
typeof(XX)
logit <- cv.glmnet(x=XX2, y=W, family="binomial")
e.hat <- predict(logit, XX2, s = "lambda.min", type="response")
e.hat.1 <- e.hat[W==1]
e.hat.0 <-e.hat[W==0]
#hg1 <- hist(e.hat.1,plot=FALSE)
#hg0 <- hist(e.hat.0,plot=FALSE)
#plot(hg0,col="red",main="e.hat, treated in blue, controls in red")
#plot(hg1,col="blue",add=TRUE)
covariates <- c(baseline_var,'ageinm' , 'male' , 
                'refugee' , 'astudent' , 'braven_sd' , 'beyes_sd' , 'b_schoolsize', 
                'f_csize')
treatment <- 'treatment'
outcome <- final_var

# Input covariates need to be numeric. 
XX <- model.matrix(formula(paste0("~", paste0(covariates, collapse="+"))), data=final_cohesion)

# Estimate a causal forest.
Y<-unlist(final_cohesion[,outcome])
W<-unlist(final_cohesion[,treatment])
cluster<-unlist(final_cohesion[,'b_schoolid'])
forest <- causal_forest(
  X=XX,  
  W=W,
  Y=Y,
  clusters = cluster,
  #W.hat=...,  # In randomized settings, set W.hat to the (known) probability of assignment
  num.trees = 100)
forest.ate <- average_treatment_effect(forest,target.sample="overlap")
print(forest.ate)