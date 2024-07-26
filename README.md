# InternshipSP24
This repository contains the code I used to complete MEIN40400 at UCD. The files should be run in the following order:

1. **data_preparation.R**  
   Processes normalized count data obtained from Obón-Santacana et al. (2022).
2. **grid_search.R**  
   Performs grid search over parameters and records associated AUCs.
3. **models.R**  
   Builds BART classifiers using best performing hyperparameters.
4. **evaluation.R**  
   Evaluates the BART classifiers and produces ROC curves and density plots.
5. **var_selection.R**  
   Performs variable selection and evaluates BART model using selected variables.


The script **functions.R** contains helper functions to run the above scripts. 

We also evaluated the performance of BART in a multicategorical setting in **multiclass.R**.

The profiles of the selected bacteria for the CRC and adenoma predictive models are viewable in R. 
