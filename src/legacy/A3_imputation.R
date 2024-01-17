library(dplyr)
library(miceRanger)
library(readr)
library(data.table)
library(gridExtra)


source("../src/functions.R")

path <- setPath("p1402-mauricim")
# print r session info
#sessionInfo()

set.seed(42)

# provide split id to be imputed
args <- commandArgs(trailingOnly=TRUE)
bootstrap_id <- as.integer(args[[1]])

# multiple imputation parameters
number_of_imputations = 5 # default: 5
number_of_iterations = 20 # default: 5, 30 is enough
number_of_trees = as.integer(100) # default: 500

bootstrap_samples <- readRDS(paste0(path, "data/bootstrap_samples.rds"))

df <- bootstrap_samples %>% filter(bs_id==bootstrap_id)

variables_file <- 'data/variables_w_missing.csv'

variables <- read.csv(paste0(path, variables_file))
variables <- variables %>% filter(!grepl("summary", variable))

# filter patients alive at prediction time
df <- df %>% filter(hn4_dv_status == "1 - Alive")

# make subset with only variables that will be used in imputation
df_imp <- df[, variables$variable]

# sort variables from low to high missingness, that way they will be imputed in that order
variables_sorted <- variables[order(variables$missing),] 

# make a vector containing the names of all variables that need to be imputed
df_vars_to_impute <- variables_sorted %>% filter(missing>0)
vars_to_impute <- df_vars_to_impute$variable #variable names

# update so that vars_to_impute does not include variables with no missingness for this specific subset of data
vars_to_impute <- vars_to_impute [! vars_to_impute %in% names(which(colSums(is.na(df_imp))==0))]


df_imp_variables <- data.frame(vars_to_impute)
df_imp_variables <- df_imp_variables %>% subset(vars_to_impute != "Studyid_hn057")  #remove these variables as possible predictors


# make a list of variables to impute
vars_mice <- setNames(as.list(vars_to_impute),vars_to_impute)

# Give to each list component (i.e. each variable to impute) a vector containing the variables that will be used to impute that specific component
for(i in 1:length(vars_mice)){

  # use all variables, except the variable being imputed itself, to impute each predictor   
   
    pred_temp <- as.vector(df_imp_variables$vars_to_impute)
    vars_mice[[i]] <- pred_temp[!pred_temp %in% c(names(vars_mice[i]))]
  
  }



# specify if (predictive) meanMatching or (predicted) value will be imputed.
## ## meanMatch best if integer, bimodal or skewed data, else predictor value usually good. 
## Categorical variables should always be "value"
## Here: have the more continues variables (i.e. all summaries) & the unordered factors as value , 
## while the rest are "meanMatch" (including integer ordinal data)

vars_unordered_factors<- variables_sorted %>% filter(dtype=="category") # contains only unordered factors 
value_selector<-c(rep(NA,length(names(vars_mice))))
for(i in 1:length(value_selector)){
  value_selector[i] <- ifelse( names(vars_mice)[i] %in% vars_unordered_factors$variable | grepl("summary", names(vars_mice[i])),
                               yes="value", no="meanMatch")
}
names(value_selector) <- names(vars_mice)
#print("Below is a list showing the method used for each variable being imputed")
#value_selector


print("Starting imputation")
Sys.time()

#df_imp <- df_imp %>% select(-c(Studyid_hn057))

#seed <- 42
#number_of_imputations <- 5
#number_of_trees <- 100

imp_all <- miceRanger(df_imp, 
                      m = number_of_imputations, 
                      maxiter = number_of_iterations, 
                      num.trees=number_of_trees,
                      returnModels = FALSE, 
                      verbose=TRUE, 
                      parallel = FALSE, 
                      respect.unordered.factors = "partition",
                      vars = vars_mice, 
                      valueSelector = value_selector)

saveRDS(imp_all, paste0(path, "data/imputed_bs", bootstrap_id,".rds"))





