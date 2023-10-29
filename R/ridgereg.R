#' Linear Regression by Ridge Regression
#' 
#' The ridgereg class runs a linear regression by ridge regression on a given formula and dataset. 
#' 
#' @field formula An R formula describing the desired formula for the linear regression
#' @field data A data.frame containing the necessary data for the linear regression
#' @field lambda A scalar value lambda to tune the model
#' @field beta_hats A matrix with the estimated coefficients
#' @field y_hat A matrix with the predicted values for y
#' @field formula_call The formula as used in the object initialization
#' @field data_call The data as used in the object initialization
#' 
#' @importFrom stats model.matrix
#' 
#' @export ridgereg
#' @export ridgereg

ridgereg <- setRefClass("ridgereg",
                        fields=list(formula="formula",
                                    data="data.frame",
                                    lambda="numeric",
                                    scale="logical",
                                    beta_hats="matrix",
                                    y_hat="matrix",
                                    res="matrix",
                                    formula_call="character",
                                    data_call="character",
                                    lambda_call="character",
                                    scale_call="character"
                                    ),
                        methods=list(initialize=function(formula, data, lambda, scale=0) {
                          .self$formula <<- formula
                          .self$data <<- data
                          .self$lambda <<- lambda
                          .self$formula_call <<- deparse(substitute(formula))
                          .self$data_call <<- deparse(substitute(data))
                          .self$lambda_call <<- deparse(substitute(lambda))
                          .self$scale_call <<- deparse(substitute(scale))
                          
                          y_var <- all.vars(formula)[[1]]
                          if (all.vars(formula)[2] == ".") {
                            X_var <- names(data[, !names(data) %in% y_var])
                          }
                          else {
                            X_var <- all.vars(formula)[2:length(all.vars(formula))] # https://stackoverflow.com/questions/18017765/extract-variables-in-formula-from-a-data-frame
                          }
                          
                          if (scale) {
                            X_norm <- as.data.frame(lapply(data[, X_var], FUN=function(x) {
                              if (is.numeric(x)) {(x - mean(x)) / sqrt(var(x))}
                              else {x}
                            }))
                            data_use <- cbind(data[, y_var, drop=FALSE], X_norm) # https://stackoverflow.com/questions/29325688/keep-column-name-when-select-one-column-from-a-data-frame-matrix-in-r
                          }
                          else {
                            data_use <- data
                          }
                          
                          X <- model.matrix(formula, data_use)
                          y <- as.matrix(data_use[rownames(X), y_var, drop=FALSE])
                          
                          .self$beta_hats <<- solve(t(X) %*% X + lambda * diag(dim(X)[2])) %*% t(X) %*% y # https://www.geeksforgeeks.org/how-to-create-the-identity-matrix-in-r/
                          .self$y_hat <<- X %*% .self$beta_hats
                          .self$res <<- y - .self$y_hat
                          
                          colnames(.self$beta_hats) <<- "beta_hat"
                          colnames(.self$y_hat) <<- "y_hat"
                          
                          .self
                        },
                        predict=function(model = .self , newdata = .self$data) {
                          
                          #.self as default
                            
                            
                            if(all.vars(model$formula)[2] == "."){
                              X_var <- colnames(newdata)[!(colnames(newdata) %in% all.vars(model$formula)[1])]
                            }else{
                              X_var <- all.vars(model$formula)[2:length(all.vars(model$formula))] # https://stackoverflow.com/questions/18017765/extract-variables-in-formula-from-a-data-frame
                            }
                          
                            if (model$scale_call == "TRUE") {
                              X_norm <- as.data.frame(lapply(newdata[, X_var], FUN=function(x) {
                                if (is.numeric(x)) {(x - mean(x)) / sqrt(var(x))}
                                else {x}
                              }))
                              data_use <-  X_norm # https://stackoverflow.com/questions/29325688/keep-column-name-when-select-one-column-from-a-data-frame-matrix-in-r
                            }else{
                              data_use <- newdata
                            }
                            
                            
                            X <- model.matrix(as.formula(paste0("y ~ ",paste0(X_var, collapse = "+"))), cbind(y = 1,data_use))
                            #prediction
                            output <- X %*% model$beta_hats
                            colnames(output) <- "y_hat"
                            return(output)
                          
                          },
                        coef=function() {.self$beta_hats}
                        )
)


ridgereg$methods(show = function(){
  coef <- t(.self$beta_hats)
  dimnames(coef)[[1]] <- ""
  
  cat("\nCall:\nridgereg(formula = ", .self$formula_call, ", data = ", .self$data_call, ", lambda = ", .self$lambda_call, ", scale = ", .self$scale_call, ")\n\n", 
      "Coefficients:\n", sep="")
  print.default(coef, print.gap=2L, quote=FALSE, right=TRUE)
})

ridgereg$methods(print = function(){
  coef <- t(.self$beta_hats)
  dimnames(coef)[[1]] <- ""
  
  cat("\nCall:\nridgereg(formula = ", .self$formula_call, ", data = ", .self$data_call, ", lambda = ", .self$lambda_call, ", scale = ", .self$scale_call, ")\n\n", 
      "Coefficients:\n", sep="")
  print.default(coef, print.gap=2L, quote=FALSE, right=TRUE)
})

#' @export
predict.ridgereg <- function(model, newdata){
  ridgereg$methods("predict")(model ,newdata)
}

