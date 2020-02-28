#' Class \code{"Cube"}
#'
#' @name Dimension-class
#' @aliases Dimension-class
#' @docType class
#' @slot name (character) The name of the dimension.
#' @slot values (vector) A vector of selected values for this dimension.
#' @slot aggregation (vector) A vector of aggregation functions that will be applied to this dimension.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Dimension", ...)}. This S4 class describes \code{Dimension} objects.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @keywords classes
#' @examples
#'
#' # show Dimension definition
#' showClass("Dimension")
#'
#'
#' @export
setClass(
  "Dimension",
  representation(
    name = "character",
    values = "character",
    aggregation = "character"
  )
)


#' Shows a \code{Dimension} object
#'
#' @param object The \code{Dimension} object
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @seealso \code{\link[=Cube-class]{Cube}}
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube@@view[[1]]
#'
#' @export
setMethod("show", "Dimension", function(object) {
  cat(object@name, "\n")
  cat("---------------------------------\n")
  cat("Values:", object@values, "\n")
  cat("Aggregation:", object@aggregation, "\n\n")
})
