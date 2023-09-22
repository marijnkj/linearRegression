#' Document here
require(ggplot2)

linreg <- function(formula, data) {
  # linreg class
  # use model.matrix() to create matrix X of independent variables
  # get dependent variable y using all.vars()
  # return object of linreg class
  
  if (!inherits(formula, "formula") | !is.data.frame(data)) {
    stop("Check your variables! formula must be a formula and data a data.frame.")
  }
  else {
    # Linear regression
    X <- model.matrix(formula, data) # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/model.matrix
    y <- as.matrix(data[all.vars(formula)[1]])
  
    beta_hats <- solve(t(X) %*% X) %*% t(X) %*% y
    y_hat <- X %*% beta_hats
    res <- y - y_hat
    df <- length(y) - length(beta_hats)
    res_var <- ((t(res) %*% res) / df)[1]
    var_beta_hats <- diag(res_var * solve(t(X) %*% X))
    t_beta_hats <- beta_hats / sqrt(var_beta_hats)
    p_beta_hats <- pt(abs(t_beta_hats), df, lower.tail=FALSE) # NOT CORRECT
    
    create_linreg <- setRefClass("linreg",
                                 fields=list(beta_hats="matrix",
                                             y_hat="matrix",
                                             res="matrix",
                                             df="numeric",
                                             res_var="numeric",
                                             var_beta_hats="numeric",
                                             t_beta_hats="matrix",
                                             p_beta_hats="matrix"
                                             ),
                                 )
    create_linreg$methods(show = function(){
      cat("\n",
          "Coefficients \n")
      print(beta_hats[,1])
    })
    
    output_linreg <- create_linreg$new(beta_hats=beta_hats,
                                       y_hat=y_hat,
                                       res=res,
                                       df=df,
                                       res_var=res_var,
                                       var_beta_hats=var_beta_hats,
                                       t_beta_hats=t_beta_hats,
                                       p_beta_hats=p_beta_hats
                                       )
    
    
  }
}

model <- lm(Sepal.Length ~ Sepal.Width, iris)
summary(model)

myfunc <- function(data){
  
}
