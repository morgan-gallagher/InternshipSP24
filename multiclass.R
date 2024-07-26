
### BART ### 
bart_multi_misclassification_lst_test <- c()
bart_multi_misclassification_lst_train <- c()
bart_confusion_matrix_lst <- list()

start_time <- Sys.time()
for (i in 1:100) {
  print(paste("STARTING ITERATION:", i, sep = " "))
  set.seed(i)
  x.train <- as.matrix(subset(train_df, select = -status))
  x.test <- as.matrix(subset(test_df, select = -status))
  y.train <- as.numeric(factor(train_df$status))
  y.test <- as.numeric(factor(test_df$status))
  
  #run BART model
  post_colscreen <- mbart2(x.train = x.train, y.train = y.train,
                           x.test = x.test, ntree = 100, keepevery = 5, 
                           base = 0.95, power = 2, k = 3, ndpost = 1000, 
                           nskip = 100)
  
  #for convergence testing
  #ts.plot(rowSums(post_colscreen$yhat.train))
  
  predictions_test <- make_predictions(post_colscreen, test = TRUE)
  misclassification_test <- sum(predictions_test != y.test) / length(y.test)
  bart_multi_misclassification_lst_test <- append(bart_multi_misclassification_lst_test, misclassification_test)
  
  confusion_matrix <- empty_confusion_matrix(K)
  confusion_matrix <- fill_confusion_matrix(confusion_matrix, predictions_test, test = TRUE)
  bart_confusion_matrix_lst[[i]] <- confusion_matrix
  
  predictions_train <- make_predictions(post_colscreen, test = FALSE)
  misclassification_train <- sum(predictions_train != y.train) / length(y.train)
  bart_multi_misclassification_lst_train <- append(bart_multi_misclassification_lst_train, misclassification_train)
}

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)

confusion_array <- array(unlist(bart_confusion_matrix_lst), dim = c(K, K, length(bart_confusion_matrix_lst)))
avg_confusion_matrix <- apply(confusion_array, c(1,2), mean)

bart_multi_mean_misclassification <- mean(bart_multi_misclassification_lst)
hist(bart_multi_misclassification_lst, xlab = "misclassifiation", ylab = "frequency", main = paste("bart misclass rates (Mean =", bart_multi_mean_misclassification, ")"))


### Random Forest ###
library(randomForest)

rf_misclassification_lst <- c()
for (i in 1:100) {
  set.seed(i)
  train_index <- createDataPartition(response, p = 0.8, list = FALSE)
  x.train <- as.matrix(colscreen.prop[train_index, ])
  x.test <- as.matrix(colscreen.prop[-train_index, ])
  y.train <- response[train_index]
  y.test <- response[-train_index]
  
  y.train_rf = as.factor(y.train)
  y.test_rf = as.factor(y.test)
  #y.test_rf = factor(y.test_rf, levels = c(levels(y.test_rf), 1))
  
  classifier_RF = randomForest(x = x.train, y = y.train_rf, ntree = 200, maxnodes = 20) 
  
  predictions <- predict(classifier_RF, newdata = x.test)
  
  misclassification_rf <- sum(predictions != y.test_rf)/length(y.test_rf)
  
  rf_misclassification_lst <- append(rf_misclassification_lst, misclassification_rf)
}

rf_mean_misclass <- mean(rf_misclassification_lst)
hist(rf_misclassification_lst, xlab = "misclassifiation", ylab = "frequency", main = paste("rand forest misclass rates (Mean =", rf_mean_misclass, ")"))


### glmnet ###
library(glmnet)

glmnet_multi_misclass_lst <- c()
for (i in 1:100) {
  set.seed(i)
  train_index <- createDataPartition(response, p = 0.8, list = FALSE)
  x.train <- as.matrix(colscreen.prop[train_index, ])
  x.test <- as.matrix(colscreen.prop[-train_index, ])
  y.train <- response[train_index]
  y.test <- response[-train_index]
  
  cvfit <- cv.glmnet(x.train, y.train, family = "multinomial", alpha = 1)
  glmnet_predict <- predict(cvfit, newx = x.test, s = "lambda.min", type = "class")
  
  misclassification <- sum(glmnet_predict != y.test) / length(y.test)
  glmnet_multi_misclass_lst <- append(glmnet_multi_misclass_lst, misclassification)
}


glm_multi_mean_misclass <- mean(glmnet_multi_misclass_lst)
hist(glmnet_multi_misclass_lst, xlab = "misclassifiation", ylab = "frequency", main = paste("glm misclass rates (Mean =", glm_multi_mean_misclass, ")"))


#save results
multiclass_df <- data.frame(
  glm_misclass = glmnet_multi_misclass_lst,
  bart_misclass = bart_multi_misclassification_lst,
  rf_misclass = rf_misclassification_lst
)

colnames(multiclass_df) <- c("LASSO_Misclassification", "BART_Misclassification", "RF_Misclassification")

#plot results
multiclass_longer <- multiclass_df %>%
  pivot_longer(
    cols = everything(),
    names_to = c("method", "metric"),
    names_sep = "_",
    values_to = "value"
  )

multiclass_longer <- multiclass_longer %>%
  mutate(
    method = factor(method, levels = c("LASSO", "BART", "RF")),
    metric = factor(metric, levels = c("Misclassification"))
  )


multiclass_longer <- subset(multiclass_longer, select = -metric)

multiclass_plot <- ggplot(multiclass_longer, aes(y = value, fill = method)) +
  geom_boxplot(width = 0.5) +
  labs(y = "Misclassification", title = "Multiclass (CRC, Adenoma, Controls)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

multiclass_plot <- multiclass_plot + labs(fill = "Method")


#helper function
make_predictions <- function(post, test = TRUE) {
  
  predictions = c()
  
  if (test == TRUE) {
    for (i in 0:((length(post$prob.test.mean)/K) - 1)) {
      index = which.max(post$prob.test.mean[(i * K + 1) : (i * K + K)])
      predictions <- append(predictions, index, after = length(predictions))
    }
  }
  else if (test == FALSE) {
    for (i in 0:((length(post$prob.train.mean)/K) - 1)) {
      index = which.max(post$prob.train.mean[(i * K + 1) : (i * K + K)])
      predictions <- append(predictions, index, after = length(predictions))
    }
  }
  
  return(predictions)
}


