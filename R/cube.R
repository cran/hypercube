#' Class \code{"Cube"}
#'
#' @name Cube-class
#' @aliases Cube-class
#' @docType class
#' @slot data (array) The data that are represented as hypercube.
#' @slot structure (list) The structure of the dimensions of the hypercube.
#' @slot view (list) Information about how to build a view for the hypercube. This information is stored in a list of \code{\link{Dimension-class}} objects.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Cube", ...)}. This S4 class describes \code{Cube} objects.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link{generateCube}}
#' @keywords classes
#' @examples
#'
#' # show Cube definition
#' showClass("Cube")
#'
#' @export
setClass(
  "Cube",
  representation(
    data = "array",
    structure = "list",
    view = "list"
  )
)

# Select an k-dimensional subcube
.select = function(data, columnValues) {
  if (is.list(columnValues)) {
    command = "data["
    firstElem = T
    for (column in names(dimnames(data))) {
      elem = columnValues[[column]]
      if (firstElem) {
        firstElem = F
      } else {
        command = paste(command, ",")
      }
      if (!is.null(elem)) {
        command = paste(command, "c(")
        first = T
        for (val in elem) {
          if (first) {
            first = F
          } else {
            command = paste(command, ",")
          }
          if (!is.na(val)) {
            command = paste(command, "'", val, "'", sep = "")
          }
        }
        command = paste(command, ")", sep = "")
      }
    }
    command = paste(command, "]")
    newData = eval(parse(text = command))
    return(newData)
  } else {
    stop("Parameter columnValues needs to be a list.")
  }
}

# Aggregate to a k-dimensional subcube
.aggregate = function(data, columns, fun = c("sum", "min", "max", "prod", "mean", "median", "sd", "count")) {
  fun = match.arg(fun)
  if (sum(columns %in% names(dimnames(data))) == length(columns)) {
    if (fun == "sum") {
      result = apply(data, columns, FUN = function(x) {return(sum(x, na.rm = T))})
    } else if (fun == "min") {
      result = apply(data, columns, FUN = function(x) {return(min(x, na.rm = T))})
    } else if (fun == "max") {
      result = apply(data, columns, FUN = function(x) {return(max(x, na.rm = T))})
    } else if (fun == "prod") {
      result = apply(data, columns, FUN = function(x) {return(prod(x, na.rm = T))})
    } else if (fun == "mean") {
      result = apply(data, columns, FUN = function(x) {return(mean(x, na.rm = T))})
    } else if (fun == "median") {
      result = apply(data, columns, FUN = function(x) {return(median(x, na.rm = T))})
    } else if (fun == "sd") {
      result = apply(data, columns, FUN = function(x) {return(sd(x, na.rm = T))})
    } else if (fun == "count") {
      result = apply(data, columns, FUN = function(x) {return(sum(!is.na(x)))})
    }
  } else {
    stop("Incorrect aggregation sequence.")
  }
  return(result)
}

# Rotate cube
.rotate = function(data, dimensions) {
  dims = names(dimnames(data))
  newOrder = as.numeric(sapply(dimensions, FUN = function(x) which(dims == x)))
  newData = aperm(data, newOrder)
  return(newData)
}

.parseSelection = function(view) {
  select = ""
  first = TRUE
  for (i in 1:length(view)) {
    v = view[[i]]
    if (v@values[1] != "") {
      if (!first) {
        select = paste(select, ",")
      }
      select = paste(select, v@name, " = c(", paste("'", v@values, "'", collapse = ',', sep = ""), ")", sep = "")
      if (first) {
        first = FALSE
      }
    }
  }
  if (select != "") {
    select = paste("list(", select, ")", sep = "")
  }
  return(eval(parse(text = select)))
}

.parseRotation = function(view) {
  dimensions = as.character(unlist(lapply(view, FUN = function(x) x@name)))
  return(dimensions)
}

.parseAggregation = function(view) {
  maxLength = max(unlist(lapply(view, FUN = function(x) return(length(x@aggregation)))))
  columns = vector()
  fun = vector()
  lvl = vector()
  for (i in 1:maxLength) {
    cols = unlist(lapply(view, FUN = function(x) {if (length(x@aggregation) >= i && str_length(x@aggregation[i]) > 0 && x@aggregation[i] != "none") {return(x@name)}}))
    fn = unlist(lapply(view, FUN = function(x) {if (length(x@aggregation) >= i && str_length(x@aggregation[i]) > 0 && x@aggregation[i] != "none") {return(x@aggregation[i])}}))
    columns = c(columns, cols)
    fun = c(fun, fn)
    lvl = c(lvl, rep(i, length(cols)))
  }
  return(data.frame(columns = columns, fun = fun, level = lvl))
}

.entropy = function(x) {
  freq = table(x) / length(x)
  h = -sum(freq * log2(freq))
  return(h)
}

.informationGain = function(feature, data, target) {
  data = data[!is.na(data[,feature]),]
  classH = .entropy(data[,target])
  if (is.factor(data[,feature])) {
    featureData = data %>% group_by_at(feature) %>% summarise(e = .entropy(get(target)), n = length(get(target)))
  } else {
    bins = length(unique(data[,feature]))
    data$cat = cut(data[,feature], breaks=bins, labels=c(1:bins))
    featureData = data %>% group_by(cat) %>% summarise(e = .entropy(get(target)), n = length(get(target)))
  }
  featureData$p = featureData$n/nrow(data)
  gain = classH - sum(featureData$p * featureData$e)
  return(gain)
}

.dimensionImportance = function(x, imp) {
  values = imp[which(rownames(imp) == x),]
  dim(values) = c(length(x), 1)
  rownames(values) = rownames(imp)[which(rownames(imp) == x)]
  colnames(values) = "importance"
  dimImportance = list(importance = sum(values), attributes = values)
  return(dimImportance)
}

.convert = function(x) {
  if (is.factor(x)) {
    y = tryCatch({
      x = as.numeric(as.character(x))
    }, warning = function(w) {
      x
    })
  } else {
    y = x
  }
  return(y)
}

#' Shows a Cube object
#'
#' Shows the actual view of a \code{Cube} object. All added selections and aggregations will be
#' regarded. Note that selection criteria will be applied before
#' aggregating the data.
#'
#' @param object The \code{Cube} object
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}}
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube
#'
#' @export
setMethod("show", "Cube", function(object) {
  dat = object@data
  columns = .parseRotation(view = object@view)
  if (!is.null(columns)) {
    dat = .rotate(data = dat, dimensions = columns)
  }
  columns = .parseSelection(view = object@view)
  if (!is.null(columns)) {
    dat = .select(data = dat, columnValues = columns)
  }
  aggDF = .parseAggregation(view = object@view)
  levels = ifelse(nrow(aggDF) == 0, 0, max(aggDF$level))
  if (levels > 0) {
    for (l in 1:levels) {
      aggDFSel = subset(aggDF, aggDF$level == l)
      splits = strsplit(paste(aggDFSel$fun,collapse = ","), ",")
      funFrequency = table(unlist(splits))
      funFrequency = sort(funFrequency, decreasing = T)
      for (fun in names(funFrequency)) {
        columns = aggDFSel$columns[which(str_detect(aggDFSel$fun, fun))]
        dat = .aggregate(data = dat, columns = as.character(columns), fun = as.character(fun))
      }
    }
  }
  print(dat)
})


#' Visualizes a Cube object as parallel coordinate plot
#'
#' Generates a parallel coordinate plot for a given \code{Cube} object. All added selections and aggregations will be
#' regarded. 
#'
#' @param x The \code{Cube} object that should be plotted.
#' @param color The color of the lines in the parallel coordinate plot. If this parameter is NA or NULL, a colorscale rather than a unique color will be used.
#' @param colorscale The colorscale for the lines in the parallel coordinate plot. Default is RdBu. All plotly colorscales (e.g., Blackbody, Earth, Jet) are possible.
#' @param ... Further plot_ly parameters.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}}
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' plot(cube)
#'
#' @export
setMethod("plot", "Cube", function(x, color = NA, colorscale = "RdBu", ...) {
  df = as.data.frame(x)
  nas = which(is.na(df[,ncol(df)]))
  if (length(nas) > 0) {
    df = df[-nas,]
  }
  dimensions = list()
  for (i in 1:ncol(df)) {
    value = .convert(df[,i])
    if (is.factor(value)) {
      dimensions[[i]] = list(range = c(min(as.numeric(value), na.rm = T), max(as.numeric(value), na.rm = T)), label = names(df)[i], values = as.numeric(value), tickvals = unique(as.numeric(value)), ticktext = levels(value))
    } else {
      dimensions[[i]] = list(range = c(min(value, na.rm = T), max(value, na.rm = T)), label = names(df)[i], values = value)
    }
  }
  if (is.na(color) || is.null(color)) {
    p = plot_ly(type = "parcoords", line = list(color = ~value, colorscale = colorscale), dimensions = dimensions, ...)
  } else {
    p = plot_ly(type = "parcoords", line = list(color = color, colorscale = colorscale), dimensions = dimensions, ...)
  }
  return(p)
})


#' Generates a hypercube from a given dataframe
#'
#' This function generates a hypercube from a given dataframe. The dimensions of the
#' hypercube correspond to a set of selected columns from the dataframe.
#'
#' @param data A dataframe that is used as source for the hypercube.
#' @param columns A vector of column names that will form the dimensions of the hypercube.
#' @param valueColumn The name of the column that provides the values for the cells of
#' the hypercube.
#' @return Returns a \code{Cube} object.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}}
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#'
#' @export generateCube
generateCube = function(data, columns, valueColumn) {
  if (!is.data.frame(data)) {
    stop("Parameter data must be a data.frame.")
  }
  if (!is.vector(columns)) {
    stop("Parameter columns must be a vector.")
  }
  if (!is.character(valueColumn)) {
    stop("Parameter valueColumn must be a character.")
  }
  data = tapply(
    data[[valueColumn]],
    data[,c(as.character(unlist(columns)))],
    FUN=function(x){return(sum(x))})
  view = sapply(X = unlist(columns), FUN = function(x) {return(new("Dimension", name = x, values = "", aggregation = ""))})
  names(view) = unlist(columns)
  cube = new("Cube", data = data, structure = columns, view = view)
  return(cube)
}


#' Adds selection criteria to a hypercube
#'
#' This function adds further selection criteria to a hypercube.
#' The cube itself will not be changed. The selection criteria only affect the data that
#' will be shown when printing the cube. Note that selection criteria will be applied before
#' aggregating the data.
#'
#' @param x Hypercube for which the selection criteria will be defined.
#' @param criteria A list of selection criteria.
#' @return Returns a \code{Cube} object with the added selection criteria.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}} \code{\link{remove.selection}} \code{\link{add.aggregation}}
#' @aliases add.selection,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' print(str(sales))
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube = add.selection(cube, criteria = list(state = c("CA", "FL")))
#' cube
#' cube = add.selection(cube, criteria = list(state = c("TX")))
#' cube
#'
#' @export add.selection
setGeneric("add.selection", function(x, criteria)
  standardGeneric("add.selection"))
setMethod("add.selection", "Cube", function(x, criteria) {
  if (!is.object(x)) {
    stop("Parameter x must be of type Cube.")
  }
  if (class(x)[[1]] != "Cube") {
    stop("Parameter x must be of type Cube.")
  }
  if (!is.list(criteria)) {
    stop("Parameter criteria must be a list.")
  } else {
    dims = as.character(unlist(x@structure))
    for (criterion in names(criteria)) {
      if (criterion %in% dims) {
        if (!is.null(criteria[[criterion]])) {
          if (sum(criteria[[criterion]] %in% dimnames(x@data)[[criterion]]) == length(criteria[[criterion]])) {
            x@view[[criterion]]@values = unique(c(x@view[[criterion]]@values ,criteria[[criterion]]))
            x@view[[criterion]]@values = x@view[[criterion]]@values[which(x@view[[criterion]]@values != "")]
          } else {
            stop(paste("There is no level", criteria[[criterion]], "in dimension", criterion, "."))
          }
        } else {
          stop(paste("Criterion", criterion, "is null."))
        }
      } else {
        stop(paste("There is no dimension", criterion, "in your cube."))
      }
    }
  }
  return(x)
})


#' Removes selection criteria from a hypercube
#'
#' This function removes all selection criteria for the given dimensions.
#' The cube itself will not be changed. The selection criteria only affect the data that
#' will be shown when printing the cube.
#'
#' @param x Hypercube for which the selection criteria will be defined.
#' @param dimensions A vector of dimension names for which all selection criteria will be removed.
#' @return Returns a \code{Cube} object with removed selection criteria.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}} \code{\link{add.selection}} \code{\link{remove.aggregation}}
#' @aliases remove.selection,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' print(str(sales))
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube = add.selection(cube, criteria = list(state = c("CA", "FL")))
#' cube
#' cube = remove.selection(cube, dimensions = c("state"))
#' cube
#'
#' @export remove.selection
setGeneric("remove.selection", function(x, dimensions)
  standardGeneric("remove.selection"))
setMethod("remove.selection", "Cube", function(x, dimensions) {
  if (!is.object(x)) {
    stop("Parameter x must be of type Cube.")
  }
  if (class(x)[[1]] != "Cube") {
    stop("Parameter x must be of type Cube.")
  }
  if (!is.vector(dimensions)) {
    stop("Parameter dimensions must be a vector.")
  }
  dims = unlist(x@structure)
  for (dimension in dimensions) {
    if (dimension %in% dims) {
      x@view[[dimension]]@values = ""
    } else {
      stop(paste("There is no dimension", dimension, "in your cube."))
    }
  }
  return(x)
})


#' Adds an aggregation to a hypercube
#'
#' This function adds a further aggregation to a hypercube.
#' The cube itself will not be changed. The aggregation only affect the data that
#' will be shown when printing the cube. Note that selection criteria will be applied before
#' aggregating the data.
#'
#' @param x Hypercube for which the selection criteria will be defined.
#' @param dimensions A vector of dimensions that are used in the aggregation.
#' @param fun The function that is used for aggregation. Possible functions are sum, prod, min, max, mean, median, sd, and count.
#' @return Returns a \code{Cube} object with the added aggregation.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}} \code{\link{remove.aggregation}} \code{\link{add.selection}}
#' @aliases add.aggregation,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube = add.aggregation(cube, dimensions = c("month", "year"), fun = "sum")
#' cube
#'
#' @export add.aggregation
setGeneric("add.aggregation", function(x, dimensions, fun = c("sum", "min", "max", "prod", "mean", "median", "sd", "count"))
  standardGeneric("add.aggregation"))
setMethod("add.aggregation", "Cube", function(x, dimensions, fun = c("sum", "min", "max", "prod", "mean", "median", "sd", "count")) {
  if (!is.object(x)) {
    stop("Parameter x must be of type Cube.")
  }
  if (class(x)[[1]] != "Cube") {
    stop("Parameter x must be of type Cube.")
  }
  if (!is.vector(dimensions)) {
    stop("Parameter dimensions must be a vector.")
  }
  fun = match.arg(fun)
  dims = unlist(x@structure)
  for (dimension in dimensions) {
    if (dimension %in% dims) {
      x@view[[dimension]]@aggregation = c(x@view[[dimension]]@aggregation, fun)
      x@view[[dimension]]@aggregation = x@view[[dimension]]@aggregation[which(x@view[[dimension]]@aggregation != "")]
    } else {
      stop(paste("There is no dimension", dimension, "in your cube."))
    }
  }
  nonAggregation = dims[which(!dims %in% dimensions)]
  for (dimension in nonAggregation) {
    x@view[[dimension]]@aggregation = c(x@view[[dimension]]@aggregation, "none")
    x@view[[dimension]]@aggregation = x@view[[dimension]]@aggregation[which(x@view[[dimension]]@aggregation != "")]
  }
  return(x)
})

#' Removes aggregations from a hypercube
#'
#' This function removes aggregations from a hypercube.
#' The cube itself will not be changed. The aggregation only affect the data that
#' will be shown when printing the cube.
#'
#' @param x Hypercube from which the aggregation will be removed.
#' @param dimensions A vector of dimensions for which the aggregations will be removed.
#' @param last Should the last aggregation be removed? If this parameter is set TRUE, the dimension vector will be ignored.
#' @return Returns a \code{Cube} object with the added aggregation.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}} \code{\link{add.aggregation}} \code{\link{remove.selection}}
#' @aliases remove.aggregation,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube = add.aggregation(cube, dimensions = c("month", "year"), fun = "sum")
#' cube
#' cube = add.aggregation(cube, dimensions = "year", fun = "sum")
#' cube
#' cube = remove.aggregation(cube, dimensions = "year")
#' cube
#'
#' @export remove.aggregation
setGeneric("remove.aggregation", function(x, dimensions = NA, last = FALSE)
  standardGeneric("remove.aggregation"))
setMethod("remove.aggregation", "Cube", function(x, dimensions = NA, last = FALSE) {
  if (!is.object(x)) {
    stop("Parameter x must be of type Cube.")
  }
  if (class(x)[[1]] != "Cube") {
    stop("Parameter x must be of type Cube.")
  }
  if (!is.vector(dimensions)) {
    if (!last)
      stop("Parameter dimensions must be a vector.")
  }
  dims = unlist(x@structure)
  maxLength = max(unlist(lapply(x@view, FUN = function(x) return(length(x@aggregation)))))
  if (last) {
    for (j in dims) {
      x@view[[j]]@aggregation = x@view[[j]]@aggregation[1:(maxLength-1)]
    }
    maxLength = maxLength - 1
  } else {
    for (dimension in dimensions) {
      if (dimension %in% dims) {
        if (maxLength < 2) {
          x@view[[dimension]]@aggregation = "none"
        } else {
          x@view[[dimension]]@aggregation = rep("none", maxLength)
        }
      } else {
        stop(paste("There is no dimension", dimension, "in your cube."))
      }
    }
  }
  for (i in maxLength:1) {
    s = sum(unlist(lapply(x@view, FUN = function(x) return(x@aggregation[i] == "none" || x@aggregation[i] == ""))))
    if (s == length(dims)) {
      for (j in dims) {
          if (i == 1) {
            x@view[[j]]@aggregation = ""
          } else {
            x@view[[j]]@aggregation = x@view[[j]]@aggregation[1:(i-1)]
          }
      }
    }
  }
  return(x)
})


#' Changes the order of the dimensions in a given cube
#'
#' @param x Hypercube for which the dimensions should be re-ordered.
#' @param dimensions Vector of dimensions. The order of the dimensions in this vector defines the order of the dimensions in the cube.
#' @return Returns a \code{Cube} object.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @seealso \code{\link[=Cube-class]{Cube}}
#' @aliases change.dimensionOrder,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube = change.dimensionOrder(cube, dimensions = c("product", "month", "year", "state"))
#' cube
#'
#' @export change.dimensionOrder
setGeneric("change.dimensionOrder", function(x, dimensions)
  standardGeneric("change.dimensionOrder"))
setMethod("change.dimensionOrder", "Cube", function(x, dimensions) {
  if (!is.object(x)) {
    stop("Parameter x must be of type Cube.")
  }
  if (class(x)[[1]] != "Cube") {
    stop("Parameter x must be of type Cube.")
  }
  if (!is.vector(dimensions)) {
    stop("Parameter dimensions must be a vector.")
  }
  if (all(names(x@view) %in% dimensions) && length(names(x@view)) == length(dimensions)) {
    x@view = x@view[dimensions]
  } else {
    stop("Parameter dimensions must be a vector containing the names of all dimensions of your cube.")
  }
  return(x)
})


#' Converts the actual view of a cube to a data frame
#'
#' Converts the actual view of a \code{Cube} object to a data frame. All added selections and
#' aggregations will be regarded. Note that selection criteria will be applied before
#' aggregating the data.
#'
#' @param x The \code{Cube} object that will be converted to a data frame.
#' @param row.names A character vector giving the row names for the data frame.
#' @param optional Should setting row names and converting column names be optional?
#' @param ... Further parameters that are passed to \code{\link{as.data.frame.table}}.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @return A molten data frame
#' @seealso \code{\link{add.aggregation}} \code{\link{add.selection}}
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' cube = change.dimensionOrder(cube, dimensions = c("product", "month", "year", "state"))
#' df = as.data.frame(cube)
#' df
#'
#' @export
as.data.frame.Cube = function(x, row.names = NULL, optional = FALSE, ...) {
  dat = x@data
  columns = .parseRotation(view = x@view)
  if (!is.null(columns)) {
    dat = .rotate(data = dat, dimensions = columns)
  }
  columns = .parseSelection(view = x@view)
  if (!is.null(columns)) {
    dat = .select(data = dat, columnValues = columns)
  }
  aggDF = .parseAggregation(view = x@view)
  levels = ifelse(nrow(aggDF) == 0, 0, max(aggDF$level))
  if (levels > 0) {
    for (l in 1:levels) {
      aggDFSel = subset(aggDF, aggDF$level == l)
      splits = strsplit(paste(aggDFSel$fun,collapse = ","), ",")
      funFrequency = table(unlist(splits))
      funFrequency = sort(funFrequency, decreasing = T)
      for (fun in names(funFrequency)) {
        columns = aggDFSel$columns[which(str_detect(aggDFSel$fun, fun))]
        dat = .aggregate(data = dat, columns = as.character(columns), fun = as.character(fun))
      }
    }
  }
  df = as.data.frame.table(dat, row.names = row.names, optional = optional, ...)
  names(df)[ncol(df)] = "value"
  return(df)
}


#' Calculates the sparsity of a given cube.
#'
#' Calculates the sparsity of the actual view of a \code{Cube} object. All added selections and
#' aggregations will be regarded. Note that selection criteria will be applied before
#' aggregating the data.
#'
#' @param x The \code{Cube} object for which the sparsity will be computed.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @return Sparsity value
#' @seealso \code{\link{importance}}
#' @aliases sparsity,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' sparsity(cube)
#'
#' @export
setGeneric("sparsity", function(x)
  standardGeneric("sparsity"))
setMethod("sparsity", "Cube", function(x) {
  df = as.data.frame(x)
  sparsity = sum(is.na(df[,ncol(df)])) / nrow(df)
  return(sparsity)
})


#' Calculates the dimension importances of a given cube.
#'
#' Calculates the importance values for all dimensions of the actual view of a \code{Cube} object. All added selections and
#' aggregations will be regarded. Note that selection criteria will be applied before
#' aggregating the data.
#'
#' @param x The \code{Cube} object for which the importance values will be computed.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @return Sparsity value
#' @seealso \code{\link{sparsity}}
#' @aliases importance,Cube-method
#' @keywords methods
#' @docType methods
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' importance(cube)
#'
#' @export
setGeneric("importance", function(x)
  standardGeneric("importance"))
setMethod("importance", "Cube", function(x) {
  data = as.data.frame(x)
  target = "value"
  data = data[!is.na(data$value),]
  features = names(data)
  features = as.matrix(features[-which(features == target)], ncol = 1)
  imp = apply(features, 1, .informationGain, data, target)
  imp = as.matrix(imp/sum(imp), ncol = 1)
  rownames(imp) = features
  importances = lapply(x@structure, .dimensionImportance, imp)
  class(importances) = "Importances"
  return(importances)
})


#' Prints an Importances object.
#'
#' Prints an \code{Importances} object.
#'
#' @param x The \code{Importances} object that will be printed.
#' @param ... Ignored parameters.
#' @author Michael Scholz \email{michael.scholz@@uni-passau.de}
#' @return Sparsity value
#' @seealso \code{\link{importance}}
#' @examples
#'
#' data("sales")
#' cube = generateCube(sales, columns = list(time = c("month", "year"),
#'       location = c("state"), product = "product"), valueColumn = "amount")
#' importances = importance(cube)
#' print(importances)
#'
#' @export
print.Importances = function(x, ...) {
  for (i in 1:length(x)) {
    cat(names(x)[i], ": ", x[[i]]$importance, "\n", sep = "")
    attributes = x[[i]]$attributes
    if (length(attributes) > 1) {
      for (a in 1:length(attributes)) {
        cat("- ", rownames(attributes)[a], ": ", attributes[a], "\n", sep = "")
      }
    }
  }
}
