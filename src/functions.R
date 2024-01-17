# functions

# user defined functions
aliveAtTimepoint <- function(timepoint, timeline){
  if(as.numeric(timeline) >= as.numeric(timepoint)){
    return(paste("1 - Alive"))
  }
  else{
    return(paste("2 - Dead"))
  }
}
aliveAtTimepoint <- Vectorize(aliveAtTimepoint) # vectorization allows to use the function with dplyr

ifNever <- function(x, conditionVariable, conditionValue){
  # the variables: 
  # hn1_a9_ay_former_tobac_user - If you are a former tobacco user, how long ago did you stop using tobacco?
  # hn1_a10_ay_begin_tobac_use - At what age did you begin to use tobacco products?
  # contains missing values associated to people that never smoked
  # we will encode these with -100000 value and replace only the real missing values with NAs afterwards
  # this function will do the same as:
  # mutate( hn1_a9_ay_former_tobac_user = ifelse((hn1_a8_ay_tobacco == "3 - Never") & (hn1_a9_ay_former_tobac_user==".a - Missing"), 
  #                                              -100000, # true
  #                                              hn1_a9_ay_former_tobac_user)) %>% # false
  if((conditionVariable == conditionValue ) & (x == ".a - Missing")){
    return(-100000)
  } 
  else{
    return(x)
  }
}
ifNever <- Vectorize(ifNever)

countMiss <- function(df){
    # this function counts missing values for the HN5000 data - either NAs and Missing
    # returns data.frame with two columns with the variable name and % of missing values
  
    missingness <- data.frame(matrix(ncol=2, nrow=ncol(df)))
    colnames(missingness) <- c("variable", "missing")
    missingness$variable <- names(df)
    
    
    n <- 1
    for ( column in names(df)){
      missingness[['missing']][n] <- 100.0*(sum(grepl("Missing",df[[column]])) + 
                                          sum(grepl("Unknown",df[[column]])) + 
                                          sum(is.na(df[[column]])))/nrow(df)
        
        n <- n + 1
    }
    
    #missingness <- missingness %>% filter(missing>0|missing==100)
    return(missingness)
}

checkLevel <- function(var_level){
  # this function receives a level from a categorical variable
  # and converts it to an integer
  # this encoding is used for natural ordered variables

  for (int_level in 0:16){

    if(grepl(paste0("^", as.character(int_level),"."), var_level)){
      return(int_level)
    } 

  }

  if(var_level == ".a - Missing" | is.na(var_level)){
      return(NA)
  }
  else{
      return(var_level)
  }

}

encodeAllLevels <- function(df, variable_list){
  # receives a data.frame object df and a variable list as input
  # and encode all levels as integers for the variable_list provided
  # then return the encoded data.frame

  for (variable in variable_list){
  
    var_levels <- unique(df[, variable])
    for (var_level in var_levels){
    df[, variable][df[, variable] == var_level] = checkLevel(var_level)
    }
  }

  return(df)
}

encode_treatment <- function(df){
    # encode hn1_treatgroup as three new binary variables hn1_surgery, hn1_chemotherapy and hn1_radiotherapy
    df <- df %>% mutate(hn1_surgery = if_else(grepl("surgery", hn1_treatgroup) | grepl("primary", hn1_treatgroup) , 1, 0))
    df <- df %>% mutate(hn1_chemotherapy = if_else(grepl("chemo",hn1_treatgroup) , 1, 0)) 
    df <- df %>% mutate(hn1_radiotherapy = if_else(grepl("radiotherapy",hn1_treatgroup) , 1, 0))
  
  return(df)
  }

missingPerObservation <- function(df){
  # This function checks how many quality of life variables are missing
  # for the EORTC-QLQ-30 and EORTC-QLQ-HN35 subscales that will be
  # used for imputing the outcome

  qolVars <- df %>% dplyr::select(contains(c("hn1_dv_c30_", "hn2_dv_c30_", "hn3_dv_c30_", 
                                      "hn1_dv_hn35_", "hn2_dv_hn35_", "hn3_dv_hn35_"))) %>% colnames(.)
  
  df <- df %>% mutate(notMissingVars = -1)
  
  for (obs in 1:nrow(df)){
    n <- 0
    for (qolvar in qolVars){
      
      if(!is.na(df[obs, qolvar]) & df[obs, qolvar]!= ".a - Missing"){
        n <- n + 1
      }
    
    }
     df[obs, "notMissingVars"] <- n 
  }
  return(df)
}

check_observations_per_level <- function(df, threshold=0.05){
    # this function reads a data.frame df and returns a list of categorical variables
    # which present low number of observations in at least one level
    # according with the treshold provided
    factor_variables <- df %>% dplyr::select(where(is.factor))
    warning_list <- data.frame(variable = c(), counts=c(), level=c())
    for (variable in colnames(factor_variables)){
      level_proportions <- prop.table(summary(df[, variable]))
   
      if (any(level_proportions < threshold)){
        #var_counts <- paste0(as.numeric(summary(df[, variable])), collapse=",")
        warning_list[nrow(warning_list) + 1, "variable"] <- variable
        warning_list[nrow(warning_list), "counts"] <-  min(summary(df[, variable])) #var_counts
        warning_list[nrow(warning_list), "level"] <- which.min(summary(df[, variable]))
      }
    }
    cat(paste0("warning: some variables present low observations number in at least one of their levels.\n"))
    
    return(warning_list)
}


miceRanger.completeAll <- function(miceRangerObj, include=TRUE){
  # this function takes a miceRanger imputation object as input
  # then stack all the datasets in a single data.frame
  # if include=TRUE the original data is also included

  # extracts ALL imputed datasets into a list
  imputed_list <- miceRanger::completeData(miceRangerObj, 
                                           datasets = c(1:length(miceRangerObj$finalImps))) 
  if(!include){
    df <- data.frame() # init empty data.frame to stack the imputed datasets 
  }
  else {
    df <- miceRangerObj$data # copy original dataset with missing values
    id <- c(1:nrow(df)) # add the observation id
    imp <- 0 # imputation number
    df <- cbind(id, df)
    df <- cbind(imp, df)
  }
  
  imp <- 1
  for (dataSet in imputed_list){
    id <- c(1:nrow(dataSet))
    dataSet <- cbind(id, dataSet)
    dataSet <- cbind(imp, dataSet)
    df <- rbind(df, dataSet) # stack the imputed datasets
    imp <- imp + 1
  }
  
  return(df)
  
}


setPath <- function(username){
      if(Sys.info()[['sysname']]=="Windows"){
        path <- paste0("M:/", username, "/analysis/")
    } else{
        path  <- paste0("/tsd/p1402/home/",username, "/analysis/")
    }

    return(path)
}

load_survival_data <- function(args){
    
    path <- setPath("p1402-mauricim")

    train_file <- paste0(path, 'data/', args[1])
    test_file <- paste0(path, 'data/', args[2])
    
    variables_file <- paste0(path,'data/variables_model.csv')
    
    variables <- load_variable_list(variables_file, model="survival") 
                        
    varClasses <- variables$rtype
    names(varClasses) <- variables$variable

    df_train <- readr::read_csv(train_file, col_type = varClasses)
    df_train <- as.data.frame(df_train) %>% dplyr::select(all_of(variables$variable))
    #df_train <- df_train %>% mutate(outcome = hn4_dv_status) %>% dplyr::select(-hn4_dv_status)

    # specify and make ordered factors
    vars_ordered <- variables %>% 
        filter(dtype=="int64" & rtype=="factor") # contains only factors that should be ordered
    
    vars_unordered <-  variables %>% 
        filter(dtype=="category" & rtype=="factor")
    
    orderFactors <- function(x) { return(ordered(x, levels=sort(levels(x))))}
    
    df_train <- df_train %>% mutate_at(vars_ordered$variable, orderFactors)

    unordered_vars <- df_train %>% 
                      dplyr::select(where(is.factor)) %>%
                      dplyr::select(!where(is.ordered)) %>% 
                      dplyr::select(-hn4_dv_status) %>%
                      names()

    ordered_vars <- df_train %>% 
                      dplyr::select(where(is.factor)) %>%
                      dplyr::select(where(is.ordered)) %>% 
                      names()

    if(length(args)>1){
        df_test <- readr::read_csv(test_file, col_type = varClasses)
        df_test <- as.data.frame(df_test)
        df_test$outcome <- df_test$hn4_dv_status
        
        #df_test <- df_test  %>% 
        #            mutate_all(~na_if(., ".a - Missing")) %>% 
        #            mutate_all(~na_if(.,"5 - Unknown"))  %>%
        #            mutate_all(~na_if(., "NA"))

        for (column in colnames(df_train)){ 
          if(is.ordered(df_train[, column])){
            levels(df_test[, column]) <- levels(df_train[,column])
          }
        }

        df_test <- df_test %>% mutate_at(vars_ordered$variable, orderFactors)

    } else {
        df_test <- data.frame()
    }

    return(list(train=df_train, test=df_test, variables=variables, ord=ordered_vars, uord=unordered_vars))

}

loadTrainTestData <-  function(args, outcome_name ="hn4_dv_c30_ghs", 
                               baseline_outcome_name = "hn3_dv_c30_ghs", 
                               variables_list ="variables_model.csv", 
                               rm_na_outcome = TRUE){
  
  # this function loads the train and test set, select the predictors
  # and assign the correct types for each column
  # then returns df_train, df_test, formula_fit, predictors
  
  path <- setPath("p1402-mauricim")
  
  train_file <- paste0(path, 'data/', args[1])
  test_file <- paste0(path, 'data/', args[2])
  
  variables_file <- paste0(path,'data/', variables_list)
  
  variables <- load_variable_list(variables_file) 
  
  predictors <- variables %>% 
    filter(type!="outcome" & type!="excluded") %>% 
    dplyr::select(variable)
  
  varClasses <- variables$rtype
  names(varClasses) <- variables$variable
  
  df_train <- readr::read_csv(train_file, col_type = varClasses)
  
  df_miss <- as.data.frame(df_train) %>% 
    filter(imp==0) %>% 
    dplyr::select(all_of(predictors$variable) | as.character(outcome_name))
  
  df_train <- as.data.frame(df_train)  %>% 
    filter(imp>0)  %>% 
    droplevels()
  
  # specify and make ordered factors
  vars_ordered <- variables %>% 
    filter(dtype=="int64" & rtype=="factor") # contains only factors that should be ordered
  
  vars_unordered <-  variables %>% 
    filter(dtype=="category" & rtype=="factor")
  
  orderFactors <- function(x) { return(ordered(x, levels=sort(levels(x))))}
  
  df_train <- df_train %>% mutate_at(vars_ordered$variable, orderFactors)
  df_miss <- df_miss %>% mutate_at(vars_ordered$variable, orderFactors) 
  
  
  df_train <- df_train %>% 
    mutate(outcome = 
             case_when(get(outcome_name) - get(baseline_outcome_name) <= -10 ~ "Decline", 
                       get(outcome_name) - get(baseline_outcome_name) > -10 ~ "No change or improved")) %>%
    mutate(outcome = as.factor(outcome)) %>%
    dplyr::select(!contains("hn4")) 
  
  variables <- variables %>% filter(!grepl("hn4", variable))
  
  
  unordered_vars <- df_train %>% 
    dplyr::select(where(is.factor)) %>%
    dplyr::select(!where(is.ordered)) %>% 
    dplyr::select(-outcome) %>%
    names()
  
  ordered_vars <- df_train %>% 
    dplyr::select(where(is.factor)) %>%
    dplyr::select(where(is.ordered)) %>% 
    names()
  
  variables[nrow(variables)+1, ] <- c("outcome", "outcome", "categorical", "factor", 0)
  
  
  
  # in case we load only the training data length(args) == 1
  # so we add a place holder to df_test, just to keep consistency
  if(length(args)>1){
    df_test <- readr::read_csv(test_file, col_type = varClasses)
    
    df_test <- as.data.frame(df_test)
    
    if(rm_na_outcome){ 
      df_test <- df_test %>%  filter(!is.na(outcome_name))
    }
    
    df_test <- df_test %>%
      dplyr::select(all_of(predictors$variable), as.character(outcome_name)) %>%
      mutate_all(~na_if(., ".a - Missing")) %>% 
      mutate_all(~na_if(.,"5 - Unknown"))  %>%
      mutate_all(~na_if(., "NA")) %>%     
      droplevels()
    
    # some levels are present in the test set but not in the train set
    # hn3_a25a_ay_hlth_mobility, level 5 has only 2 observations
    for (column in colnames(df_train)){ 
      if(is.ordered(df_train[, column])){
        levels(df_test[, column]) <- levels(df_train[,column])
      }
    }
    
    df_test <- df_test %>% mutate_at(vars_ordered$variable, orderFactors)
    
    #df_test <- df_test %>% mutate(outcome = 
    #                        case_when( outcome_name - baseline_outcome_name<=-10 ~ 1,
    #                        outcome_name - baseline_outcome_name>-10 ~ 0)) %>% 
    #                        mutate(outcome = as.factor(outcome))
    
  } else {
    df_test <- data.frame() # place holder
  }
  
  return(list(train=df_train, test=df_test, vars=variables, ord=ordered_vars, uord=unordered_vars, miss=df_miss))
}


predict.pool <- function(models, new_data, class1=".pred_Decline", platt_calibration){
    # pooled predictions for binary outcome models
    # //todo: expand to multiclass problems 
    # //todo: make it able to accept only one model
    # models: list of model workflow objects from tidymodels package
    # new_data: data to be predicted with the provided workflow in models

    pooled_prediction <- 0*c(1:nrow(new_data))
    
    single_class_probabilities <- setNames( data.frame(matrix(ncol = length(models), 
                                                              nrow = nrow(new_data))), 
                                            c(paste0("p",1:length(models))))
    calibrated_probs <- setNames( data.frame(matrix(ncol = length(models), 
                                                              nrow = nrow(new_data))), 
                                            c(paste0("p",1:length(models))))    

    
    for (m in 1:length(models)){
        prediction <- as.data.frame(stats::predict(models[[m]], new_data, type="prob")) %>%
                      pull(class1)
        # removing individual calibration because it's irrelevant, we need to 
        # recalibrate after pooling the probabilities

        #calibrated_probabilities <- predict(platt_calibration, 
        #    data.frame(predicted = prediction), 
        #    type = "response") %>% as.numeric

        pooled_prediction <- pooled_prediction + prediction #calibrated_probabilities
        single_class_probabilities[, m] <- prediction
        #calibrated_probs[, m] <- 1 - calibrated_probabilities
    }
    pooled_prediction <- pooled_prediction/length(models)

    # calibrate pooled prediction
    pooled_prediction <- predict(platt_calibration, 
            data.frame(predicted = pooled_prediction), 
            type = "response") %>% as.numeric

    pooled_prediction <- 1 - pooled_prediction

    #ci <- transform(single_class_probabilities, 
    #                CI=apply(single_class_probabilities, 1,  sd, na.rm = TRUE))
    #ci <- as.data.frame(1.96 * ci$CI) # 95% confidence interval

#    if(!is.null(platt_calibration)){
#        calibrated_probabilities <- 0*c(1:nrow(new_data)))
#        for(m in length(models)){
#            calibrated_probabilities <- calibrated_probabilities + predict(platt_calibration, 
#            data.frame(predicted = single_class_probabilities[, m]), 
#            type = "response")
#        }
#    }

    results <- list(prediction = data.frame(.pred_Decline=pooled_prediction, 
                                `.pred_No change or improved`=1 - pooled_prediction), 
                    probabilities = single_class_probabilities, 
                    calibrated = calibrated_probs)

    return(results) 
}

load_variable_list <- function(file, model="qol", remove_subscales=FALSE){
  require(dplyr)

    variables <- read.csv(file)

    if(model=="survival"){
        variables[variables$variable=="hn4_dv_status", ]$type <- "outcome"
        variables[variables$variable=="hn4_dv_c30_ghs", ]$type <- "excluded"
    }
  
    variables <- variables %>% 
           filter(type!="index" &
                  type!="excluded" &
           !grepl("hn3_cb_recurrence", variable) )
    # remove hn3_nb16a_cb_recurrence, no recurrence at hn3
 
  if(remove_subscales){
    variables <- variables %>% filter(!grepl("_dv_c30", variable) & !grepl("_dv_hn35", variable))
  }

  return(variables)
}

brier <- function(truth, prediction){
  # computes Brier-Score for binary outcomes - equivalent to mean squared error
  # observed events must be assigned 1 and 0 for other cases
  # sum([observed] - [predicted probability])**2/N
  return(c(.metric="brier", .estimator="binary", .estimate=sum((truth-prediction)**2)/length(truth)))
}

brier_max <- function(p_mean){
  return(p_mean * (1 - p_mean)**2 + (1-p_mean)* p_mean**2 )
}

brier_scaled <- function(brier_score, p_mean){
  return(1-brier_score/brier_max(p_mean))
}

compute_outcome <- function(df){
  df <- df %>% mutate(outcome = case_when(hn4_dv_c30_ghs - hn3_dv_c30_ghs<=-10 ~ "Decline",
                                          hn4_dv_c30_ghs - hn3_dv_c30_ghs>-10 ~ "No change or improved") ) %>%
              mutate(outcome = as.factor(outcome))

  return(df)
}

get_model_performance <- function(fitted_model, 
                                    df, 
                                    split_id, 
                                    model_type, 
                                    plot_roc=FALSE, 
                                    threshold=0.5,  
                                    all_metrics=FALSE, 
                                    class1=".pred_Decline", 
                                    platt_calibration){
    # change predict to predict.pool for ensemble learning

    require(ggplot2)
    require(tidymodels)
    require(dplyr)

    if(class(fitted_model)=="list"){
        results <- predict.pool(fitted_model, new_data = df, class1, platt_calibration)
        predictions <- results$prediction
    } else{
        predictions <- stats::predict(fitted_model, new_data = df, type="prob")
    }
    

    if(plot_roc){
        y_test <- df %>% 
        dplyr::select(outcome) %>% 
        bind_cols(predictions)

        p <- y_test %>% roc_curve( outcome, .pred_Decline) %>% 
        ggplot(aes(x = 1 - specificity, y = sensitivity)) +
        geom_path() +
        geom_abline(lty = 3) +
        coord_equal() +
        theme_bw()

        plot(p)
  }

    truth_and_predictions <- df %>%  
                            dplyr::select(outcome) %>% 
                            bind_cols(predictions) 

    roc_curve_data <- truth_and_predictions %>% roc_curve(outcome, .pred_Decline) %>% mutate(split_id = split_id)
    pr_curve_data <- truth_and_predictions %>% pr_curve(outcome, .pred_Decline)  %>% mutate(split_id = split_id)

    auc <-  truth_and_predictions %>%        
            roc_auc(truth = outcome, .pred_Decline)

    auc_pr <-  truth_and_predictions %>%        
            pr_auc(truth = outcome, .pred_Decline)

    pred_ <- as.factor(ifelse(predictions %>% 
                      dplyr::select(.pred_Decline) >= threshold, 
                      "Decline", "No change or improved"))

    y_test <- df %>% 
                dplyr::select(outcome) %>% 
                bind_cols(pred_)


    if(all_metrics){
        performance <- as.data.frame(
                        rbind(
                            sens(y_test, truth = outcome, ...2),
                            accuracy(y_test, truth = outcome, ...2),
                            precision(y_test, truth = outcome, ...2),
                            specificity(y_test, truth = outcome, ...2),
                            j_index(y_test, truth = outcome, ...2),
                            f_meas(y_test, truth = outcome, ...2),
                            kap(y_test, truth = outcome, ...2),
                            auc,
                            auc_pr
                        )
                    )
    } else {
        performance <- as.data.frame(auc)   
    }

    colnames(performance) <- c("metric", "estimator", "estimate")
    performance$split <- split_id
    performance$method <- model_type
    #performance$threshold <- threshold

  return(list(performance=performance, 
              roc_curve_data=roc_curve_data, 
              pr_curve_data=pr_curve_data))
}



#platt_scaling(cal_class_truth, cal_predicted_probability, probabilities){
#    pred[pred == 0] <- 1e-08
#    pred[pred == 1] <- 1 - 1e-08
#    platt_model <- glm(truth ~ predicted_probability, family = "binomial")
#    calibrated <- predict(platt_model, data.frame(y = resp, x = pred), 
#        type = "response")
#}


plot_calibration_curve <- function(threshold_data){
    ggplot(threshold_data, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
    geom_line() +
    theme_minimal() +
    scale_color_viridis_d(end = 0.9) +
    scale_alpha_manual(values = c(.4, 1), guide = "none") +
    geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
    labs(
        x = "Threshold",
        y = "Metric Estimate",
        title = "Balancing performance by varying the threshold",
        subtitle = "Vertical line = Max J-Index"
    )
}


platt_scaling <- function(fitted_model, df){
    predicted <- predict(fitted_model, df, type="prob") %>% 
                 pull(.pred_Decline) 

    truth <- df %>% pull(outcome)
    calibration_data <- data.frame(predicted = predicted, truth = truth)

    platt_calibration <- glm(truth ~ predicted, calibration_data, family = "binomial")

    return(platt_calibration)
}


compute_feature_importances <- function(fitted_models, method){
  
    extracted_fit <- fitted_models$m1 %>% extract_fit_parsnip() 

    if(method=="glmnet"){
        variable_names <- extracted_fit$fit$beta  %>% row.names()
    } else if (method=="ranger"){
        variable_names <- extracted_fit$fit$variable.importance %>% names()
    }

   importances <- data.frame(matrix(data=vector(), nrow=0, ncol=length(variable_names)))

  
  colnames(importances) <- variable_names

  imp <- 1
  for (model in fitted_models){
   
    importances_temp <- model %>% 
                  extract_fit_parsnip() %>%
                  vi(num_features=length(variable_names)) %>%
                  mutate(
                      Importance = abs(Importance), # Variable = forcats::fct_reorder(Variable, Importance)
                      ) %>% 
                  dplyr::select(-Sign) %>% 
                  pivot_wider(names_from = Variable, values_from = Importance)
    
    importances <- rbind(importances, importances_temp) 
  }
  
  importances_mean <- importances %>% summarise_if(is.numeric, mean)  %>% pivot_longer(cols=colnames(.)) %>% as.data.frame
  importances_sd <-  importances %>% summarise_if(is.numeric, sd)  %>% pivot_longer(cols=colnames(.)) %>% as.data.frame

  importances_mean$sd <- importances_sd$value
  
  rownames(importances_mean) <- importances_mean$name
  
  return( importances_mean)
  
}


prepare_test_data <- function(df_test, df_train, imputation = step_impute_bag, outcome_name ="hn4_dv_c30_ghs", baseline_outcome_name="hn3_dv_c30_ghs"){
    require(recipes)
    # TODO: write all package dependencies explicitly e.g. yardstick::step_impute_bag
    # first we prapare a recipe to impute the predictors based on
    # the medians and modes estimated in the trainning set
    df_train <- df_train %>% dplyr::select(!contains("outcome")) #all_of(colnames(df_test)) & !contains("hn4"))


    rec_test_imputation <- recipe(~ ., data = df_train) 

    if(is.logical(all.equal(imputation, step_impute_median))){
        #print("imputing with median/mode")
        rec_test_imputation <- rec_test_imputation %>%
                            step_impute_median(all_numeric()) %>%
                            step_impute_mode(all_nominal())

    } else{
        #print(paste0("imputing with ", as.character(substitute(imputation))))
        rec_test_imputation <- rec_test_imputation %>%
                                imputation(all_predictors())
    }

    prep_models <- prep(rec_test_imputation, training = df_train, fresh=TRUE)

    baked_test <- bake(prep_models, new_data = df_test)
    # this variable was removed during baking since it was not available in df_train
    baked_test[,outcome_name] <- df_test[, outcome_name]

    # we write a second recipe to compute the binary outcome
    rec_test_outcome <- recipe( ~ ., data = baked_test) %>% 
                            step_mutate(outcome = 
                                case_when(get(outcome_name) - get(baseline_outcome_name)<=-10 ~ "Decline", 
                                get(outcome_name) - get(baseline_outcome_name)>-10 ~ "No change or improved") %>% 
                                as.factor, role="outcome") %>%
                            step_rm(outcome_name)
            
    prepared_test_outcome <- prep(rec_test_outcome, training = baked_test)

    juiced_test <- juice(prepared_test_outcome) 

    return(juiced_test)
}




get_model_performance_surv <- function(fitted_model, 
                                    df, 
                                    split_id, 
                                    model_type, 
                                    plot_roc=FALSE, 
                                    threshold=0.5     
                                    ){
    # change predict to predict.pool for ensemble learning

    require(ggplot2)
    require(tidymodels)
    require(dplyr)

 
    predictions <- stats::predict(fitted_model, new_data = df %>% dplyr::select(-hn4_dv_status), type="prob")

    

    if(plot_roc){
        y_test <- df %>% 
        dplyr::select(hn4_dv_status) %>% 
        bind_cols(predictions)

        p <- y_test %>% roc_curve( hn4_dv_status, `.pred_1 - Alive`) %>% 
        ggplot(aes(x = 1 - specificity, y = sensitivity)) +
        geom_path() +
        geom_abline(lty = 3) +
        coord_equal() +
        theme_bw()

        plot(p)
  }

    truth_and_predictions <- df %>%  
                            dplyr::select(hn4_dv_status) %>% 
                            bind_cols(predictions) 

    roc_curve_data <- truth_and_predictions %>% roc_curve(hn4_dv_status, `.pred_1 - Alive`) %>% mutate(split_id = split_id)
    pr_curve_data <- truth_and_predictions %>% pr_curve(hn4_dv_status, `.pred_1 - Alive`)  %>% mutate(split_id = split_id)

    auc <-  truth_and_predictions %>%        
            roc_auc(truth = hn4_dv_status, `.pred_1 - Alive`)

    auc_pr <-  truth_and_predictions %>%        
            pr_auc(truth = hn4_dv_status, `.pred_1 - Alive`)

    pred_ <- as.factor(ifelse(predictions %>% 
                      dplyr::select(`.pred_1 - Alive`) >= threshold, 
                      "1 - Alive", "2 - Dead"))

    y_test <- df %>% 
                dplyr::select(hn4_dv_status) %>% 
                bind_cols(pred_)


        performance <- as.data.frame(
                        rbind(
                            sens(y_test, truth = hn4_dv_status, ...2),
                            accuracy(y_test, truth = hn4_dv_status, ...2),
                            precision(y_test, truth = hn4_dv_status, ...2),
                            specificity(y_test, truth = hn4_dv_status, ...2),
                            j_index(y_test, truth = hn4_dv_status, ...2),
                            f_meas(y_test, truth = hn4_dv_status, ...2),
                            kap(y_test, truth = hn4_dv_status, ...2),
                            auc,
                            auc_pr
                        )
                    )
 

    colnames(performance) <- c("metric", "estimator", "estimate")
    performance$split <- split_id
    performance$method <- model_type
    #performance$threshold <- threshold

  return(list(performance=performance, 
              roc_curve_data=roc_curve_data, 
              pr_curve_data=pr_curve_data))
}
