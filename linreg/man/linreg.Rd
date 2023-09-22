\name{linreg}
\alias{linreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
linreg(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (formula, data) 
{
    if (!inherits(formula, "formula") | !is.data.frame(data)) {
        stop("Check your variables! formula must be a formula and data a data.frame.")
    }
    else {
        X <- model.matrix(formula, data)
        y <- as.matrix(data[all.vars(formula)[1]])
        beta_hats <- solve(t(X) \%*\% X) \%*\% t(X) \%*\% y
        y_hat <- X \%*\% beta_hats
        res <- y - y_hat
        df <- length(y) - length(beta_hats)
        res_var <- ((t(res) \%*\% res)/df)[1]
        var_beta_hats <- diag(res_var * solve(t(X) \%*\% X))
        t_beta_hats <- beta_hats/sqrt(var_beta_hats)
        p_beta_hats <- pt(abs(t_beta_hats), df, lower.tail = FALSE)
        create_linreg <- setRefClass("linreg", fields = list(beta_hats = "matrix", 
            y_hat = "matrix", res = "matrix", df = "numeric", 
            res_var = "numeric", var_beta_hats = "numeric", t_beta_hats = "matrix", 
            p_beta_hats = "matrix"), )
        output_linreg <- create_linreg$new(beta_hats = beta_hats, 
            y_hat = y_hat, res = res, df = df, res_var = res_var, 
            var_beta_hats = var_beta_hats, t_beta_hats = t_beta_hats, 
            p_beta_hats = p_beta_hats)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
