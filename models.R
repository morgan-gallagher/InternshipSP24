#lst to record aucs
train_crc_aucs <- numeric()


### training ###

#use cv to validate model on training data 
for (i in 1:100) {
  #print(paste("STARTING ITERATION:", i, sep = " "))
  #80/20 train test split
  trainIndex <- createDataPartition(y.train, p = .8, list = FALSE, times = 1)
  df_train <- train_df[trainIndex, ]
  df_test <- train_df[-trainIndex, ]
  ytrain <- y.train[trainIndex]
  ytest <- y.train[-trainIndex]
  
  #build model using best performing params
  train_post <- mc.pbart(
    x.train = df_train, y.train = ytrain, x.test = df_test, 
    ntree = 100, ndpost = 1000, 
    nskip = 100, sparse = TRUE, 
    theta = 0, a = 0.5, b = 3, 
    rho = 478, seed = round(runif(1, min = 1, max = 1000000)),
    mc.cores = 8
  )
  
  #record aucs
  roc_curve_train <- roc(ytest, train_post$prob.test.mean)
  train_crc_aucs <- c(train_crc_aucs, auc(roc_curve_train))
  
}

### testing ###
test = predict(train_post, test_df)
