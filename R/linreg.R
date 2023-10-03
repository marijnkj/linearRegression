# TODO
# - Write vignette
# - Create theme

#' Linear Regression by OLS
#' 
#' The linreg class runs a linear regression by OLS on a given formula and dataset. 
#' Important fields such as residuals, predicted values and estimated coefficients 
#' can be accessed by calling their respective methods. Additionally, a summary of 
#' the model can be printed along with some basic plots. 
#' 
#' @field formula An R formula describing the desired formula for the linear regression
#' @field data A data.frame containing the necessary data for the linear regression
#' @field beta_hats A matrix with the estimated coefficients
#' @field y_hat A matrix with the predicted values for y
#' @field res A matrix with the residuals
#' @field df Degrees of freedom
#' @field res_var Variance of the residuals
#' @field var_beta_hats Variance of the estimated coefficients
#' @field t_beta_hats A matrix of t-values for the estimated coefficients
#' @field p_beta_hats A matrix of p-values for the estimated coefficients (t-test)
#' @field formula_call The formula as used in the object initialization
#' @field data_call The data as used in the object initialization
#' 
#' @importFrom ggplot2 ggplot geom_point geom_line aes ggtitle ylab xlab theme_classic theme element_text margin element_blank element_rect
#' @importFrom dplyr group_by summarise mutate
#' @importFrom gridExtra grid.arrange
#' @importFrom methods new
#' @importFrom patchwork inset_element
# #' @importFrom magick image_read
# #' @importFrom patchwork inset_element
#' @export linreg
# #' @examples
# #' data(iris)
# #' linreg_mod <- linreg$new(formula=Petal.Length ~ Species, data=iris)
# #' 
# #' # Print coefficients
# #' linreg_mod$print()
# #' 
# #' # Print summary of linear regression
# #' linreg_mod$summary()
# #' 
# #' # Plot some basic graphs
# #' linreg_mod$plot()
# #' 
# #' # Get residuals
# #' linreg_mod$resid()
# #' 
# #' # Get y-hat values
# #' linreg_mod$pred()
# #' 
# #' # Get coefficients
# #' linreg_mod$coef()
# #' 
# #' @method initialize Takes input for formula and data, calculates OLS regression and stores the values to the rest of the fields
# #' @method resid Returns the residuals
# #' @method pred Returns the predicted y-values
# #' @method coef Returns the estimated coefficients

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
                                  p_beta_hats="matrix",
                                  formula_call="character",
                                  data_call="character"
                                  ),
                      methods=list(initialize=function(formula, data) {
                        .self$formula <<- formula
                        .self$data <<- data
                        .self$formula_call <<- deparse(substitute(formula))
                        .self$data_call <<- deparse(substitute(data))
                        
                        X <- model.matrix(formula, data) # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/model.matrix
                        y <- as.matrix(data[all.vars(formula)[1]])
                        
                        .self$beta_hats <<- solve(t(X) %*% X) %*% t(X) %*% y
                        .self$y_hat <<- X %*% .self$beta_hats
                        .self$res <<- y - .self$y_hat
                        .self$df <<- length(y) - length(.self$beta_hats)
                        .self$res_var <<- ((t(.self$res) %*% .self$res) / .self$df)[1] # [1] to access the value
                        .self$var_beta_hats <<- diag(.self$res_var * solve(t(X) %*% X))
                        .self$t_beta_hats <<- .self$beta_hats / sqrt(.self$var_beta_hats)
                        .self$p_beta_hats <<- pt(abs(.self$t_beta_hats), .self$df, lower.tail=FALSE)*2 # x2 for two-tailed p-values
                        
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
  coef <- t(.self$beta_hats)
  dimnames(coef)[[1]] <- "" # https://github.com/SurajGupta/r-source/blob/master/src/library/base/R/dataframe.R
  
  cat("\nCall:\nlinreg(formula = ", .self$formula_call, ", data = ", .self$data_call, ")\n\n", 
      "Coefficients:\n", sep="")
  print.default(coef, print.gap=2L, quote=FALSE, right=TRUE) # https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R#L116
})

linreg$methods(print = function(){
  coef <- t(.self$beta_hats)
  dimnames(coef)[[1]] <- "" # https://github.com/SurajGupta/r-source/blob/master/src/library/base/R/dataframe.R
  
  cat("\nCall:\nlinreg(formula = ", .self$formula_call, ", data = ", .self$data_call, ")\n\n", 
      "Coefficients:\n", sep="")
  print.default(coef, print.gap=2L, quote=FALSE, right=TRUE) # https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R#L116
})

linreg$methods(summary = function() {
  df_summary <- data.frame(.self$beta_hats, sqrt(.self$var_beta_hats), .self$t_beta_hats, .self$p_beta_hats)
  df_summary <- df_summary %>% mutate(" "=ifelse(.[[4]] < 0.001, "***", ifelse(.[[4]] < 0.01, "**", ifelse(.[[4]] < 0.05, "*", ifelse(.[[4]] < 0.1, ".", "")))))
  colnames(df_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", " ")
  
  print.default(as.matrix(df_summary), print.gap=2L, quote=FALSE, right=TRUE)
  cat("\n",
      "Residual standard error: ", sqrt(.self$res_var), " on ", .self$df, " degrees of freedom", sep=""
  )
})

linreg$methods(plot = function(theme="liu") {
  # Add LiU logo and fonts?
  ### THEMES ###
  theme_liu <- function() {
    # Not messing with fonts as it is system dependent
    #theme_classic() %+replace%
    theme(
      plot.background=element_rect(
        fill="#02b3e5",
        color="#02b3e5"),
      panel.border=element_rect(
        fill=NA
      ),
      axis.ticks=element_blank(),
      plot.title=element_text(
        color="white",
        size=20,
        face="bold",
        hjust=0,
        vjust=2),
      axis.title=element_text(
        # color="white",
        size=10),
      axis.text=element_text(
        # color="white",
        size=9),
      axis.text.x=element_text(margin=margin(5, b=10))
    )
  }
  
  ### PLOTTING ###
  # liu_logo <- magick::image_read(system.file("LiU-primary-black.png", package="linearRegression")) # https://stackoverflow.com/questions/54993463/include-image-in-r-packages
  
  df_fit_res <- as.data.frame(cbind(.self$y_hat, .self$res))
  df_fit_res |>
    group_by(by=y_hat) |>
    summarise(median=median(residuals)) -> df_medians1

  color = "red"
  p_fit_res <- df_fit_res |>
    ggplot() +
    geom_point(aes(x=y_hat, y=residuals), shape=1) + # http://www.sthda.com/english/wiki/ggplot2-point-shapes
    geom_line(data=df_medians1, aes(x=by, y=median), color=color) + # https://stackoverflow.com/questions/9109156/ggplot-combining-two-plots-from-different-data-frames
    # http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
    ggtitle("Residuals vs Fitted") +
    ylab("Residuals") +
    xlab(paste("Fitted values\n", paste(as.character(.self$formula)[c(2, 1, 3)], collapse=" "))) # https://stackoverflow.com/questions/5951500/is-it-possible-to-make-print-formula-respect-the-environment-width-option
    
  if (theme != "liu") {
    p_fit_res <- p_fit_res +  
      theme_classic() + # http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
      theme(plot.title=element_text(hjust=0.5)) # https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
  }
  else {
    p_fit_res <- p_fit_res +
      theme_liu() # +
      # inset_element(p=liu_logo, left=0.05, right=0.5, top=0.95, bottom=0.65)
  }
  
  df_fit_res <- df_fit_res |>
    mutate(res_std=scale(residuals)) |>
    mutate(sqrt_abs_res_std=sqrt(abs(res_std)))
  
  df_fit_res |>
    group_by(by=y_hat) |>
    summarise(median=median(sqrt_abs_res_std)) -> df_medians2
  
  p_fit_std_res <- df_fit_res |>
    ggplot() +
    geom_point(aes(x=y_hat, y=sqrt_abs_res_std), shape=1) +
    geom_line(data=df_medians2, aes(x=by, y=median), color=color) +
    # text(x=y_hat, y=sqrt_abs_res_std, labels=rownames(.data)) +
    ggtitle("Scale-Location") +
    ylab(expression(sqrt(abs("Standardized residuals")))) + # https://stackoverflow.com/questions/12790253/how-to-make-the-square-root-symbol-in-axes-labels
    xlab(paste("Fitted values\n", paste(as.character(.self$formula)[c(2, 1, 3)], collapse=" ")))
    
  if (theme != "liu") {
    p_fit_std_res <- p_fit_std_res +
      theme_classic() +
      theme(plot.title=element_text(hjust=0.5))
  }
  else {
    p_fit_std_res <- p_fit_std_res +
      theme_liu()
      #inset_element(p=liu_logo, left=-0.5, right=-0.05, top=-0.05, bottom=-0.2)
  }
  
  grid.arrange(p_fit_res, p_fit_std_res, nrow=2) # http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
})

data(iris)
linreg_mod <- linreg(formula=Petal.Length ~ Species, data=iris)
# linreg_mod$summary()
# linreg_mod$print()
linreg_mod$plot()
