library(ggplot2)
library(pROC)

#create roc object with inference
roc_curve_test <- roc(y.test, test$prob.test.mean)
#print(roc_curve_test)

# obtain predictions
predictions <- ifelse(test$prob.test.mean > 0.53, 1, 0)

#calculate sensitivity and specificity
sens_spec <- calculate_metrics(predictions, y.test)

#construct confusion matrix
confusion_matrix_empty <- empty_confusion_matrix(2)
confusion_matrix <- fill_confusion_matrix(confusion_matrix_empty, predictions, labels = y.test)


### graphs ###

#plot roc curve
pROC::plot.roc(roc_curve_test)
plot(roc_curve_test, col = 'red', main = "CRC Predictive Model", xlim = c(1,0))
text(x = 0.25, y = 0.25, "AUC: 0.77")

#density plots
density_df <- data.frame(predicted_probabilities = test$prob.test.mean, true_labels = as.factor(y.test))

ggplot(density_df, aes(x = predicted_probabilities, fill = true_labels)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Healthy", "CRC")) +
  labs(x = "Predicted Probability", y = "Density", fill = "Class") +
  xlim(0, 1) + 
  theme_minimal()


