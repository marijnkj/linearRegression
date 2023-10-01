\name{linreg-package}
\alias{linreg-package}
\alias{linreg}
\docType{package}
\title{
\packageTitle{linreg}
}
\description{
\packageDescription{linreg}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{linreg}
\packageIndices{linreg}
The linreg class provided by this package runs a linear regression by OLS on a given formula and dataset. Important fields such as residuals, predicted values and estimated coefficients can be accessed by calling their respective methods. Additionally, a summary of the model can be printed along with some basic plots. 
}
\author{
\packageAuthor{linreg}

Maintainer: \packageMaintainer{linreg}
}
\keyword{package, OLS}
\seealso{
\code{\link[<linreg>:<linreg>-package]{<linreg>}}
}
\examples{
data(iris)
linreg_mod <- linreg(formula=Petal.Length ~ Species, data=iris)

# Print coefficients
linreg_mod$print()

# Print summary of linear regression
linreg_mod$summary()

# Plot some basic graphs
linreg_mod$plot()

# Get residuals
linreg_mod$resid()

# Get y-hat values
linreg_mod$pred()

# Get coefficients
linreg_mod$coef()
}
