
.odataR_options <- new.env()

.onLoad <- function(libname, pkgname) {
  invisible()
}

odataR_root_default = 'http://opendata.cbs.nl'

#' Sets the root for OData data and catalog structure
#'
#' Determines from which structure data and catalog information  will be extracted. If this function is not called
#' the default \code{http://opendata.cbs.nl} (the root for the CBS Statistics Netherlands structures) will be used
#' @param root NULL for the default or the url of the root structure otherwise
#' @export
#' @examples
#' odataR_set_root()
#' odataR_set_root('http://opendata.cbs.nl')
#' @seealso \code{\link{odataR_get_root}}

odataR_set_root <- function (root=NULL) {
  if (is.null(root)) {
    root = odataR_root_default
  }
  .odataR_options$root = root
  invisible(root)
}

#' Gets the root for OData data and catalog structure
#'
#' Determines from which structure data and catalog information will be extracted. If the function odataR_set_root
#' is not called yet this will be done first with the default \code{http://opendata.cbs.nl} (the root for the CBS Statistics Netherlands structures).
#' @export
#' @examples
#' odataR_get_root()
#' @seealso \code{\link{odataR_set_root}}
odataR_get_root <- function () {
  root = .odataR_options$root
  if (is.null(root)) {
    root = odataR_set_root()
  }
  invisible(root)
}

#' Gets the root for OData data structure
#'
#' Determines from which structure the data will be extracted. Derived from the common OData data and catalog root: \code{paste0(odataR_get_root(), '/ODataFeed/OData')}
#' @seealso \code{\link{odataR_get_root}} and \code{\link{odataR_get_root_catalog}}
#' @export
odataR_get_root_data <- function () {
  paste0(odataR_get_root(), '/ODataFeed/OData')
}

#' Gets the root for OData catalog structure
#'
#' Determines from which structure the catalog information will be extracted. Derived from the common OData data and catalog root: \code{paste0(odataR_get_root(), '/ODataCatalog/Tables')}
#' @seealso \code{\link{odataR_get_root}} and \code{\link{odataR_get_root_data}}
#' @export
odataR_get_root_catalog <- function () {
  paste0(odataR_get_root(), '/ODataCatalog/Tables')
}
