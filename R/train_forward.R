#' Custom forward-selection
#' 
#' 
#' 
#'
#' 
#' @export train_forward
#' 
#'
#' @field y The response variable
#' @field data A data.frame containing the necessary data for the linear regression, including response variable
#'
#'
#'
#'

# https://en.wikipedia.org/wiki/Stepwise_regression
# Doing selection with Akaike information criterion

train_forward <- function(y, data){
  available_variables <- colnames(data)
  available_variables <- available_variables[available_variables != y]
  chosen_variables <- c()
  
  output <- data.frame(matrix(NA, nrow = length(available_variables) +1, ncol = length(available_variables) +3 ))
  colnames(output) <- c("(Intercept)", available_variables,"AIC","Difference")
  row.names(output) <- as.character(0:(length(available_variables)))
  output[is.na(output)] <- " "
  
  
  results <- list()
  
  formula <- reformulate(termlabels = as.character(1), response = y)
  
  results[[1]] <- lm(formula = formula, data = data)
  
  output[,1] <- "*"
  output[1,"AIC"] <- round(AIC(results[[1]]),3)
  
  for(i in 1:(ncol(train_data)-1)){
    
    formulas <- sapply(available_variables[!(available_variables %in% chosen_variables)] , function(x){
      
      formula <- reformulate(termlabels = c(chosen_variables,x), response = y)
      
    })
    
    candidates <- lapply(formulas , function(x) lm(x , data))
    
    lowest_AIC <- names(which.min(sapply(candidates, function(x) AIC(x))))
    chosen_variables <- append(chosen_variables, lowest_AIC)
    
    results[[i+1]] <- candidates[[lowest_AIC]]
    
    output[i+1, colnames(candidates[[lowest_AIC]]$model[-1])] <- "*"
    output[i+1, "AIC"] <- round(AIC(results[[i+1]]),3)
    output[i+1, "Differnce"] <- as.numeric(output[i+1, "AIC"])-as.numeric(output[i, "AIC"])
    
  }
  
  print(output)
  return(list(best_model = results[[which.min(sapply(results, AIC))]], candidates = candidates, output = output))
  
}
