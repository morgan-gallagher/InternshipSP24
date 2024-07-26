
#construct empty confusion matrix
empty_confusion_matrix <- function(num_cat){
  confusion_matrix <- matrix(0, nrow = num_cat, ncol = num_cat)
  rownames(confusion_matrix) <- 1:num_cat
  colnames(confusion_matrix) <- 1:num_cat
  return (confusion_matrix)
}

#fill confusion matrix
fill_confusion_matrix <- function(confusion_matrix, predictions, labels, test = TRUE) {
  for (i in 1:length(predictions)) {
    row <- predictions[i] + 1
    column <- labels[i] + 1
    
    confusion_matrix[row, column] <- confusion_matrix[row, column] + 1
  }
  return(confusion_matrix)
}

#calculate sensitivity and specificity 
calculate_metrics <- function(predicted, actual) {
  true_positive <- sum(predicted == 1 & actual == 1)
  true_negative <- sum(predicted == 0 & actual == 0)
  false_positive <- sum(predicted == 1 & actual == 0)
  false_negative <- sum(predicted == 0 & actual == 1)
  
  sensitivity <- true_positive / (true_positive + false_negative)
  specificity <- true_negative / (true_negative + false_positive)
  
  return(list(sensitivity = sensitivity, specificity = specificity))
}


#fill in null distribution df
make_distributions_binary <- function(trial_num, x.train, response, null_dists) {
  
  #fill in null distribution for features
  for (trial in 1:trial_num) {
    
    print(paste("STARTING ITERATION:", trial, sep = " "))
    
    #shuffle response vector
    y.train <- sample(response)
    
    #build model
    binary <- mc.pbart(x.train = x.train, y.train = y.train, ntree = 100, ndpost = 1000,
                       nskip = 100, sparse = TRUE, theta = 0, a = 0.5, b= 3.0, rho = 478,
                       seed = round(runif(1, min = 1, max = 1000000)),
                       mc.cores = 4)
    
    for (bacteria in 1:length(binary$varcount.mean)) {
      null_dists[[1]][trial, bacteria] <- binary$varcount.mean[bacteria]
    }
    
  }
  return(null_dists)
  
}

#make empty null distribution df
create_null_dist <- function(x.train, nrow, K) {
  
  #num of cols with same values/bacteria counts in the cols, all 0's/1's for counts
  cols_with_same <- sum(sapply(x.train, function(x) length(unique(x)) == 1))
  
  #all col names from x.train
  all_col_names <- names(x.train)
  
  #subtract col names that have the repeated values
  selected_col_names <- all_col_names[!(sapply(x.train, function(x) length(unique(x)) == 1))]
  
  #list for null distrbutions
  null_dists <- list()
  
  for (i in 1:K) {
    #create empty dataframe to store values
    df <- data.frame(matrix(0, nrow = nrow, ncol = length(selected_col_names)))
    #add selected col names to null dataframe
    colnames(df) <- selected_col_names 
    #name the dataframe
    assign(paste0('null_dist', i), df)
    #add the dataframe to the list
    null_dists[[i]] <- get(paste0('null_dist', i))
  }
  
  return(null_dists)
  
}


#variable selection function, return indices of selected vars
variable_selection_binary <- function (post, quantile_lst) {
  #list to store important bacteria per K
  important_var_lst <- vector("list", 1)
  
  post_df = as.data.frame(post$varcount.mean)
  
  for (i in 1:1) {
    #get varcount for K(ith) category
    cat_row <- post_df[, i]
    #get quantiles for K(ith) category
    quantile_col <- quantile_lst[[i]][, i]
    #calculate differences
    difference <- cat_row - quantile_col
    #positive differences
    selected_bacteria <- which(difference > 0)
    
    #return(selected_bacteria)
    
    important_var_lst[[i]] <- selected_bacteria
    
    #only to order differences
    #ordered_selected_bacteria <- selected_bacteria[order(difference[selected_bacteria, ], decreasing = TRUE)]
    #top 10 ordered bacteria
    #top_10 <- ordered_selected_bacteria[1:10]
    #ordered bacteria names
    #ordered_bacteria_names <- bacteria_names[ordered_selected_bacteria]
    #append ordered bacteria names to list
    #important_var_lst[[i]] <- ordered_bacteria_names
    
  }
  
  return(important_var_lst)
}


#compute quantiles from null distribution
compute_quantiles <- function (alpha, null_dists, K) {
  
  quantile_lst <- vector("list", K)
  
  for (i in 1:K) {
    quantiles <- as.data.frame(
      sapply(null_dists[[i]], function(col) quantile(col, 1-alpha))
    )
    
    quantile_lst[[i]] <- quantiles
  }
  
  return(quantile_lst)
}

