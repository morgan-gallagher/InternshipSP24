
### create null distributions ###
#empty df for null distribution
null_dists_crc <- create_null_dist(train_df, 100, 1)

#fill in null_distributions for variables
null_dists_crc <- make_distributions_binary(100, train_df, y.train, null_dists_crc)

### perform variable selection ###

#compute 95th quantile of null distributions
quantile_lsts <- compute_quantiles(0.05, null_dists_crc, 1)

#build model with selected hyperparameters
binary <- mc.pbart(x.train = train_df, y.train = y.train, ntree = 100, ndpost = 1000,
                   nskip = 100, sparse = TRUE, theta = 0, a = 0.5, b= 3.0, rho = 478,
                   seed = round(runif(1, min = 1, max = 1000000)),
                   mc.cores = 8)


#get selected variable indices
variables_selected_idx <- variable_selection_binary(binary, quantile_lsts)

#construct train and test df from selected vars
train_df_selected_vars <- train_df[, variables_selected_idx[[1]]]
test_df_selected_vars <- test_df[, variables_selected_idx[[1]]]

#training loop
train_crc_aucs <- numeric()
for (i in 1:100) {
#print(paste("STARTING ITERATION:", i, sep = " "))
trainIndex <- createDataPartition(y.train, p = .8, list = FALSE, times = 1)
df_train <- train_df_selected_vars[trainIndex, ]
df_test <- train_df_selected_vars[-trainIndex, ]
ytrain <- y.train[trainIndex]
ytest <- y.train[-trainIndex]
train_post <- mc.pbart(
x.train = df_train, y.train = ytrain, x.test = df_test, ntree = 100, ndpost = 1000,
nskip = 100, sparse = TRUE,
theta = 0, a = 0.5, b = 3,
rho = 478, seed = round(runif(1, min = 1, max = 1000000)),
mc.cores = 8
)
roc_curve_train <- roc(ytest, train_post$prob.test.mean)
train_crc_aucs <- c(train_crc_aucs, auc(roc_curve_train))
}

#test model
binary <- mc.pbart(x.train = train_df_selected_vars, y.train = y.train, 
                   ntree = 100, ndpost = 1000, 
                   nskip = 100, sparse = TRUE, theta = 0, a = 0.5, b= 3.0, 
                   rho = 478, seed = round(runif(1, min = 1, max = 1000000)), 
                   mc.cores = 4)

pred_test = predict(binary, test_df_selected_vars)
roc_curve_test <- roc(y.test, pred_test$prob.test.mean)
print(roc_curve_test)



### get species, genus, and family information ###
#Note ugg_taxonomy provided by Ob́on-Santacana

#bacteria species names
sel_bact_species_names <- colnames(train_df[variables_selected_idx[[1]]])
select_bact_df <- data.frame(species = sel_bact_species_names)

#get genus and family 
genus_lst <- c()
family_lst <- c()
for (i in 1:nrow(select_bact_df)) {
  select_bact <- select_bact_df[i, "species"]
  idx <- which(uhgg_taxonomy$species == select_bact)
  #if no info for genus/family, add NA
  if (length(idx) == 0) {
    genus <- NA
    family <- NA
    
  } else {
    genus <- uhgg_taxonomy$genus[min(idx)]
    family <- uhgg_taxonomy$family[min(idx)] 
  }
  genus_lst <- append(genus_lst, genus)
  family_lst <- append(family_lst, family)
}

#add genus/family to dataframe
select_bact_df$genus <- genus_lst
select_bact_df$family <- family_lst


### filter selected bacteria with full information ###

#genus
filtered_df_genus <- select_bact_df[!is.na(select_bact_df$genus), ]
genus_counts <- as.data.frame(table(filtered_df_genus$genus))
names(genus_counts) <- c("genus", "count")
top_genus_counts <- genus_counts[order(-genus_counts$count), ][1:15, ]

#family
filtered_df_family <- select_bact_df[!is.na(select_bact_df$family), ]
family_counts <- as.data.frame(table(filtered_df_family$family))
names(family_counts) <- c("family", "count")
top_family_counts <- family_counts[order(-family_counts$count), ][1:15, ]


### graphs ###

#for genus
ggplot(top_genus_counts, aes(x = reorder(genus, count), y = count)) +
  geom_bar(stat = "identity", fill = '#7a81ff') +
  coord_flip() +  
  xlab("Genus") +
  ylab("Frequency") +
  ggtitle("Selected Bacteria Profile") +
  theme_minimal()

#for family
ggplot(top_family_counts, aes(x = reorder(family, count), y = count)) +
  geom_bar(stat = "identity", fill = '#7a81ff') +
  coord_flip() +  
  xlab("Family") +
  ylab("Frequency") +
  ggtitle("Selected Bacteria Profile") +
  theme_minimal()
