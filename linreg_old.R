#' Document here
require(ggplot2)
require(dplyr)
library(gridExtra)

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
    p_beta_hats <- pt(abs(t_beta_hats), df, lower.tail=FALSE)*2 # CORRECT
    
    # Change column names for aesthetics
    colnames(beta_hats) <- c("beta_hat")
    colnames(y_hat) <- c("y_hat")
    colnames(res) <- c("residuals")
    
    create_linreg <- setRefClass("linreg",
                                 fields=list(beta_hats="matrix",
                                             y_hat="matrix",
                                             res="matrix",
                                             df="numeric",
                                             res_var="numeric",
                                             var_beta_hats="numeric",
                                             t_beta_hats="matrix",
                                             p_beta_hats="matrix",
                                             formula="formula"
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
    
    plot.linreg <- function(x) {
      df_fit_res <- as.data.frame(cbind(x$y_hat, x$res))
      df_fit_res |>
        group_by(by=y_hat) |>
        summarise(median=median(residuals)) -> df_medians1
      
      p_fit_res <- df_fit_res |>
        ggplot() +
        geom_point(aes(x=y_hat, y=residuals), shape=1) + # http://www.sthda.com/english/wiki/ggplot2-point-shapes
        geom_line(data=df_medians1, aes(x=by, y=median), color="red") + # https://stackoverflow.com/questions/9109156/ggplot-combining-two-plots-from-different-data-frames
        # http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
        ggtitle("Residuals vs Fitted") +
        ylab("Residuals") +
        xlab(paste("Fitted values\n", paste(as.character(x$formula)[c(2, 1, 3)], collapse=" "))) + # https://stackoverflow.com/questions/5951500/is-it-possible-to-make-print-formula-respect-the-environment-width-option
        theme_classic() + # http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
        theme(plot.title=element_text(hjust=0.5)) # https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
    
      df_fit_res <- df_fit_res |>
        mutate(res_std=(residuals-min(residuals))/(max(residuals)-min(residuals))) |>
        mutate(sqrt_res_std=sqrt(res_std))
      
      df_fit_res |>
        group_by(by=y_hat) |>
        summarise(median=median(sqrt_res_std)) -> df_medians2
      
      p_fit_std_res <- df_fit_res |>
        ggplot() +
        geom_point(aes(x=y_hat, y=sqrt_res_std), shape=1) +
        geom_line(data=df_medians2, aes(x=by, y=median), color="red") +
        ggtitle("Scale-Location") +
        ylab(expression(sqrt("Standardized residuals"))) + # https://stackoverflow.com/questions/12790253/how-to-make-the-square-root-symbol-in-axes-labels
        xlab(paste("Fitted values\n", paste(as.character(x$formula)[c(2, 1, 3)], collapse=" "))) +
        theme_classic() +
        theme(plot.title=element_text(hjust=0.5))
      
      grid.arrange(p_fit_res, p_fit_std_res, nrow=2) # http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
    }

    output_linreg <- create_linreg$new(beta_hats=beta_hats,
                                       y_hat=y_hat,
                                       res=res,
                                       df=df,
                                       res_var=res_var,
                                       var_beta_hats=var_beta_hats,
                                       t_beta_hats=t_beta_hats,
                                       p_beta_hats=p_beta_hats,
                                       formula=formula
                                       )
    
    return(output_linreg)
  }
}


model <- lm(Sepal.Length ~ Sepal.Width, iris)
summary(model)

plot(output_linreg)

