\name{CutSplineFun}
\alias{CutSplineFun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spline Function with Constant Flat Tails}
\description{Spline Function for Calculation of Scenario Parameters Used in PortfolioValue}
\usage{CutSplineFun(xy = data.frame(x = NULL, y = NULL), x = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{\code{data.frame} giving the coordinates of the points to be interpolated}
  \item{x}{x coordinate of a point to be interpolated}
}

\value{y coordinate of a point to be interpolated}
\author{Oleg Mubarakshin}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(100, 200, 300)
y <- (x - 200)^2

x0 <- 150
y0 <- CutSplineFun(xy = data.frame(x = x, y = y), x = x0)

x0 <- 400
y0 <- CutSplineFun(xy = data.frame(x = x, y = y), x = x0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
