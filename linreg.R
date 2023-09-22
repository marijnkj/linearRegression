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
    
    # Change column names for aesthetics
    colnames(beta_hats) <- c("beta_hat")
    colnames(y_hat) <- c("y_hat")
    
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
                                 methods=list(resid <- function(x) {x$res},
                                              pred <- function(x) {x$y_hat},
                                              coef <- function(x) {setNames(as.vector(x$beta_hats), row.names(x$beta_hats))})
                                 )
    
    create_linreg$methods(show = function(){
      cat("\n",
          "Coefficients \n")
      print(beta_hats[,1])
    })
    
    summary.linreg <- function(x) {
      df_summary <- data.frame(x$beta_hats, sqrt(x$var_beta_hats), x$t_beta_hats, x$p_beta_hats)
      colnames(df_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      print(df_summary)
      cat("\n",
          "Residual standard error: ", sqrt(x$res_var), " on ", x$df, " degrees of freedom", sep=""
          )
    }
    
    plot.linreg <- function() {
      ggplot(aes(x=.self$y_hat, y=.self$res)) |>
        geom_point()
    }

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

summary(output_linreg)

