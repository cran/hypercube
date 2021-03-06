% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube.R
\docType{methods}
\name{generateCube}
\alias{generateCube}
\title{Generates a hypercube from a given dataframe}
\usage{
generateCube(
  data,
  columns,
  valueColumn,
  fun = c("sum", "min", "max", "prod", "mean", "median", "sd", "count")
)
}
\arguments{
\item{data}{A dataframe that is used as source for the hypercube.}

\item{columns}{A vector of column names that will form the dimensions of the hypercube.}

\item{valueColumn}{The name of the column that provides the values for the cells of
the hypercube.}

\item{fun}{Aggregation function for aggregating over those columns that do not correspond
with any dimension of the hypercube.}
}
\value{
Returns a \code{Cube} object.
}
\description{
This function generates a hypercube from a given dataframe. The dimensions of the
hypercube correspond to a set of selected columns from the dataframe.
}
\examples{

data("sales")
cube = generateCube(sales, columns = list(time = c("month", "year"),
      location = c("state"), product = "product"), valueColumn = "amount")

}
\seealso{
\code{\link[=Cube-class]{Cube}}
}
\author{
Michael Scholz \email{michael.scholz@th-deg.de}
}
\keyword{methods}
