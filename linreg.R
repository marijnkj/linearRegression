### absolute value on axis
### import packages through namespace
### use cat instead of print (damn it)




linreg <- setRefClass("linreg",
                      fields=list(formula="formula", 
                                  data="data.frame",
                                  beta_hats="matrix",
                                  y_hat="matrix",
                                  res="matrix",
                                  df="numeric",
                                  res_var="numeric",
                                  var_beta_hats="numeric",
                                  t_beta_hats="matrix",
                                  p_beta_hats="matrix"
                                  ),
                      methods=list(initialize=function(formula, data) {
                        .self$formula <<- formula
                        .self$data <<- data
                        
                        X <- model.matrix(formula, data) # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/model.matrix
                        y <- as.matrix(data[all.vars(formula)[1]])
                        
                        .self$beta_hats <<- solve(t(X) %*% X) %*% t(X) %*% y
                        .self$y_hat <<- X %*% .self$beta_hats
                        .self$res <<- y - .self$y_hat
                        .self$df <<- length(y) - length(.self$beta_hats)
                        .self$res_var <<- ((t(.self$res) %*% .self$res) / .self$df)[1] # [1] to access the value
                        .self$var_beta_hats <<- diag(.self$res_var * solve(t(X) %*% X))
                        .self$t_beta_hats <<- .self$beta_hats / sqrt(.self$var_beta_hats)
                        .self$p_beta_hats <<- pt(abs(.self$t_beta_hats), .self$df, lower.tail=FALSE)*2 # CORRECT
                        
                        # Change column names for aesthetics
                        colnames(.self$beta_hats) <<- c("beta_hat")
                        colnames(.self$y_hat) <<- c("y_hat")
                        colnames(.self$res) <<- c("residuals")
                        
                        .self
                      }, 
                      resid=function() {.self$res},
                      pred=function() {.self$y_hat},
                      coef=function() {setNames(as.vector(.self$beta_hats), row.names(.self$beta_hats))})
)

  
linreg$methods(show = function(){
  cat("\n",
      "Coefficients \n")
  base::print(.self$beta_hats[,1])
})

linreg$methods(print = function(){
  cat("\n",
      "Coefficients \n")
  base::print(.self$beta_hats[,1])
})

linreg$methods(summary = function() {
  df_summary <- data.frame(.self$beta_hats, sqrt(.self$var_beta_hats), .self$t_beta_hats, .self$p_beta_hats)
  colnames(df_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  base::print(df_summary)
  cat("\n",
      "Residual standard error: ", sqrt(.self$res_var), " on ", .self$df, " degrees of freedom", sep=""
  )
})

linreg$methods(plot = function() {
  require(ggplot2)
  require(dplyr)
  require(gridExtra)
  
  df_fit_res <- as.data.frame(cbind(.self$y_hat, .self$res))
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
    xlab(paste("Fitted values\n", paste(as.character(.self$formula)[c(2, 1, 3)], collapse=" "))) + # https://stackoverflow.com/questions/5951500/is-it-possible-to-make-print-formula-respect-the-environment-width-option
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
    xlab(paste("Fitted values\n", paste(as.character(.self$formula)[c(2, 1, 3)], collapse=" "))) +
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5))
  
  grid.arrange(p_fit_res, p_fit_std_res, nrow=2) # http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
})



# linreg_mod <- linreg(formula=formula, data=data)
# linreg_mod$X
# linreg_mod$print()
linreg_mod$plot()
