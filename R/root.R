
.odataR_options <- new.env()

.onLoad <- function(libname, pkgname) {
  invisible()
}

odataR_root_default = 'http://opendata.cbs.nl/ODataFeed/OData'

#' Sets the root for OData data structure
#'
#' Determines from which structure the data will be extracted
#' @param root NULL for the default or the url otherwise
#' @export
#' @examples
#' odataR_set_root()
#' odataR_set_root('http://opendata.cbs.nl/ODataFeed/OData')

odataR_set_root <- function (root=NULL) {
  if (is.null(root)) {
    root = odataR_root_default
  }
  .odataR_options$root = root
  invisible(root)
}

#' Gets the root for OData data structure
#'
#' Determines from which structure the data will be extracted
#' @export
#' @examples
#' odataR_get_root()
#'
odataR_get_root <- function () {
  root = .odataR_options$root
  if (is.null(root)) {
    root = odataR_set_root()
  }
  invisible(root)
}

