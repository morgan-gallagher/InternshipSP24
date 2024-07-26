install.packages("BART")
library(BART)
library(pROC)
library(caret)

#make grid of parameters to search over
param_grid <- expand.grid(
  ntree = c(100, 200),
  nskip = c(100),
  ndpost = c(1000),
  theta = c(0, 0.5, 1),
  rho = c(0.5 * 956, 0.75 * 956, 956),
  a = c(0.5, 1),
  b = c(0.5, 1, 2, 3)
)

#empty dataframe to store results
results_crc <- data.frame(ntree = integer(),
                           nskip = integer(),
                           ndpost = integer(),
                           theta = numeric(),
                           rho = numeric(),
                           a = numeric(),
                           b = numeric(),
                           test_auc = numeric())

#compute auc for each parameter combination
compute_average_test_auc <- function(param_set) {
  test_aucs <- numeric()
  
  for (i in 1:2) {
    #split data
    trainIndex <- createDataPartition(y.train, p = .8, list = FALSE, times = 1)
    df_train <- train_df[trainIndex, ]
    df_test <- train_df[-trainIndex, ]
    ytrain <- y.train[trainIndex]
    ytest <- y.train[-trainIndex]
    
    #train the model
    binary <- mc.pbart(
      x.train = df_train, y.train = ytrain, x.test = df_test, 
      ntree = param_set$ntree, ndpost = param_set$ndpost, 
      nskip = param_set$nskip, sparse = TRUE, 
      theta = param_set$theta, a = param_set$a, b = param_set$b, 
      rho = param_set$rho, seed = round(runif(1, min = 1, max = 1000000)),
      mc.cores = 4
    )
    
    #compute test AUC
    roc_curve_test <- roc(ytest, binary$prob.test.mean)
    test_aucs <- c(test_aucs, auc(roc_curve_test))
    #print("SUCCESS!")
  }
  
  #return avg auc
  return(mean(test_aucs))
}


#fill in result dataframe
for (i in 1:nrow(param_grid)) {
  #print(paste("ON ROW:", i, sep = " "))
  param_set <- param_grid[i, ]
  avg_test_auc <- compute_average_test_auc(param_set)
  
  results_crc <- rbind(results_crc, data.frame(
    ntree = param_set$ntree,
    nskip = param_set$nskip,
    ndpost = param_set$ndpost,
    theta = param_set$theta,
    rho = param_set$rho,
    a = param_set$a,
    b = param_set$b,
    test_auc = avg_test_auc
  ))
}
