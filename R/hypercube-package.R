#' Provides functions and methods for organizing data in hypercubes
#'
#' This package provides methods for organizing data in a hypercube
# '(i.e. a multi-dimensional cube). Cubes are generated from molten data frames.
#' Each cube can be manipulated with five operations rotation (changeDimensionOrder),
#' dicing and slicing (add.selection, remove.selection), drilling down (add.aggregation),
#' and rolling up (remove.aggregation).
#'
#'
#' \tabular{ll}{ Package: \tab hypercube\cr Type: \tab Package\cr Version:
#' \tab 0.2.1\cr Date: \tab 2020-02-27\cr License: \tab GPL-3\cr Depends: \tab
#' R (>= 3.0), methods\cr }
#'
#' @name hypercube-package
#' @aliases hypercube-package hypercube
#' @docType package
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @import methods stringr dplyr
#' @importFrom stats median sd
#' @importFrom plotly plot_ly
#' @concept hypercube
#' @keywords manip
#' @examples
#'
#' # Simple example
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube
#'
#' # More sophisticated example
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'        location = c("state"), product = "product"), valueColumn = "amount")
#' cube = add.selection(cube, criteria = list(state = c("AL", "TX")))
#' cube = add.aggregation(cube, dimensions = c("month", "year"), fun = "sum")
#' cube
#' df = as.data.frame(cube)
#' df
NULL
