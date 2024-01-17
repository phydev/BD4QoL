setClass("multiModel",
         fields = list(models="list", 
                       new_data = "data.frame",
                       pooled_predictions = "numeric"),
         methods = list(predict = function(){
             pooled_prediction <- data.frame()
             for (m in 1:n(object@models)){
                 pooled_prediction <- pooled_prediction + stats::predict(object@models[m], multiModel@new_data)
             }
             return(pooled_prediction) 
         })
)

setMethod(stats::predict, 
)

# multiModel class will save all models in the slot @models
# predict method will take the object set with multiModel and a dataset
# then it will make M predictions, one for each model and pool the results


models_list <- list(fitted, fitted, fitted, fitted, fitted, fitted)

# fork tidymodels -> ocbe-uio
# pull request

predict <- function(models, new_data){
    # pooled predictions for binary outcome models
    # //todo: expand to multiclass problems 
    # models: list of model workflow objects from tidymodels package
    # new_data: data to be predicted with the provided workflow in models
    
    if(length(models)==1){
        stop("Only one model was provided. Use stats::predict function instead.")
    }
    
    pooled_prediction <- as.data.frame(stats::predict(models[[1]], new_data, type="prob"))
    
    single_class_probabilities <- setNames( data.frame(matrix(ncol = length(models), 
                                                              nrow = nrow(pooled_prediction))), 
                                            c(paste0("p",1:length(models))))
    
    single_class_probabilities[, 1] <- pooled_prediction[, 1]
    
    for (m in 2:length(models)){
        prediction <- as.data.frame(stats::predict(models[[m]], new_data, type="prob"))
        pooled_prediction <- pooled_prediction + prediction
        single_class_probabilities[, m] <- prediction[, 1]
    }
    pooled_prediction <- pooled_prediction/length(models)
    ci <- transform(single_class_probabilities, 
                    CI=apply(single_class_probabilities, 1,  sd, na.rm = TRUE))
    ci <- as.data.frame(1.96 * ci$CI)
    results <- list(prediction = pooled_prediction, probabilities = single_class_probabilities, ci = ci)
    return(results) 
}

res <- predict(models_list, trees_df)

