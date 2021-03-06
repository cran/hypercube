% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube.R
\docType{methods}
\name{remove.selection}
\alias{remove.selection}
\alias{remove.selection,Cube-method}
\title{Removes selection criteria from a hypercube}
\usage{
remove.selection(x, dimensions)
}
\arguments{
\item{x}{Hypercube for which the selection criteria will be defined.}

\item{dimensions}{A vector of dimension names for which all selection criteria will be removed.}
}
\value{
Returns a \code{Cube} object with removed selection criteria.
}
\description{
This function removes all selection criteria for the given dimensions.
The cube itself will not be changed. The selection criteria only affect the data that
will be shown when printing the cube.
}
\examples{

data("sales")
print(str(sales))
cube = generateCube(sales, columns = list(time = c("month", "year"),
      location = c("state"), product = "product"), valueColumn = "amount")
cube = add.selection(cube, criteria = list(state = c("CA", "FL")))
cube
cube = remove.selection(cube, dimensions = c("state"))
cube

}
\seealso{
\code{\link[=Cube-class]{Cube}} \code{\link{add.selection}} \code{\link{remove.aggregation}}
}
\author{
Michael Scholz \email{michael.scholz@th-deg.de}
}
\keyword{methods}
