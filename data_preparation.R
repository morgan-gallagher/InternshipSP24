install.packages("zCompositions")
install.packages("compositions")
library(zCompositions)
library(compositions)

#NOTE:RNA seq pre-processing code was given to us by by Ob́on-Santacana, not 
#included here

### get necessary metadata ###

#cancer and control metadata
metadata_cancer_neg <- metadata_all |>
  filter(status %in% c("neg", "cancer"))

#adenoma and control metadata
metadata_adenoma_neg <- metadata_all |>
  filter(status %in% c("neg", "adenoma"))


### make train and test dataframes ###

#subset all species data to be only neg/cancer
species_cancer_neg_normalized <- species_all_normalized[rownames(metadata_cancer_neg), ]

#split normalized data by study
species_ours_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "ours"), ]
species_feng_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "feng"), ]
species_gupta_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "gupta"), ]
species_thomas_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "thomas"), ]
species_vogtmann_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "vogtmann"), ]
species_wirbel_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "wirbel"), ]
species_yachida_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "yachida"), ]
species_yu_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "yu"), ]
species_zeller_normalized <- species_cancer_neg_normalized[which(metadata_cancer_neg$study == "zeller"), ]

#replacement of zeros by study
species_ours_normalized_nozeros = cmultRepl(species_ours_normalized, method = "SQ", output = "prop", z.warning = 1)
species_feng_normalized_nozeros = cmultRepl(species_feng_normalized, method = "SQ", output = "prop", z.warning = 1)
species_gupta_normalized_nozeros = cmultRepl(species_gupta_normalized, method = "SQ", output = "prop", z.warning = 1)
species_thomas_normalized_nozeros = cmultRepl(species_thomas_normalized, method = "SQ", output = "prop", z.warning = 1)
species_vogtmann_normalized_nozeros = cmultRepl(species_vogtmann_normalized, method = "SQ", output = "prop", z.warning = 1)
species_wirbel_normalized_nozeros = cmultRepl(species_wirbel_normalized, method = "SQ", output = "prop", z.warning = 1)
species_yachida_normalized_nozeros = cmultRepl(species_yachida_normalized, method = "SQ", output = "prop", z.warning = 1)
species_yu_normalized_nozeros = cmultRepl(species_yu_normalized, method = "SQ", output = "prop", z.warning = 1)
species_zeller_normalized_nozeros = cmultRepl(species_zeller_normalized, method = "SQ", output = "prop", z.warning = 1)


#test dataset
test_df <- species_ours_normalized_nozeros

#train dataset
train_df <- bind_rows(species_feng_normalized_nozeros,
                      species_gupta_normalized_nozeros,
                      species_thomas_normalized_nozeros,
                      species_vogtmann_normalized_nozeros,
                      species_wirbel_normalized_nozeros,
                      species_yachida_normalized_nozeros,
                      species_yu_normalized_nozeros,
                      species_zeller_normalized_nozeros)


#center log ratio transformation
test_df <- as.data.frame(clr(test_df))
train_df <- as.data.frame(clr(train_df))


### make response variables ###

train_patient_ids <- rownames(train_df) 
y.train <- as.numeric(metadata_cancer_neg[train_patient_ids, "status"])
y.train <- ifelse(y.train == 3, 1, 0)

test_patient_ids <- rownames(test_df) 
y.test <- as.numeric(metadata_cancer_neg[test_patient_ids, "status"])
y.test <- ifelse(y.test == 3, 1, 0)
