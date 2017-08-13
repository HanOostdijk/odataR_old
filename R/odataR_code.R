

odataR_namespaces = c(ns="http://www.w3.org/2005/Atom",
  m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata",
  d="http://schemas.microsoft.com/ado/2007/08/dataservices")


#' Get (part of) an OData data structure
#'
#' This is the only function in the package that actually acquires data from the database. This function returns a block of at most 10000 rows (restriction of the interface) in XML format. It is therefore also suitable in cases where the result is not a nice table (e.g. in the case of TypeInfo). The function \code{\link{odataR_get_table}} will call this function one or more times (depending on the size of the data) and convert the combined data to a table or property structure.
#' @param root Root of data structure
#' @param table_id Identification of table
#' @param query OData query to restrict data returned from structure, See
#'  \code{\link{odataR_get_table}} for examples of queries
#' @param save_file_name Name of file to save the XML data in or NULL
#' @return An \code{XMLInternalDocument} if successful otherwise \code{NULL}
#' @export
#' @examples
#' \dontrun{
#' doc = odataR_get_data(table_id='82935NED')
#' doc = odataR_get_data(root='http://opendata.cbs.nl/ODataFeed/OData',table_id='82935NED')
#' }

odataR_get_data <- function (
        root           = odataR_get_root_data(),
        table_id       = NULL,
        query          = NULL,
        save_file_name = NULL) {
  # query  = "?$format=atom&$filter=GeneesmiddelengroepATC eq '100000'"
  if (!is.null(table_id) && !(substr(table_id, 1, 1) == '/'))  {
    table_id = paste0('/', table_id)
  }
  f1  = paste0(root, table_id, query)
  f1  = URLencode(f1)
  r   = curl::curl_fetch_memory(f1)
  x   = rawToChar(r$content)
  doc = get_xml_data(x)
  if (!is.null(doc)) {
    if (!is.null(save_file_name)) {
      XML::saveXML(doc, save_file_name)
    }
  }
  return(doc)
}

get_xml_data <- function(xdata) {
  if (substr(xdata,1,15)=='<!DOCTYPE html>') {
    cat('result of query contains no xml data.\n')
    return(NULL)
  }
  oldw <- getOption("warn")
  options(warn = -1)
  doc = tryCatch(XML::xmlParse(xdata, asText = T),
      warning = function(e) {invisible(NULL)},
      error = function(e) {
        tt = paste('result of query probably no xml data.',
             'First characters of result:', substr(xdata,1,15))
        cat(tt, '\n')
       invisible(NULL)
      },
      finally={options(warn = oldw)}
      )
}

#' List the tables in an OData structure
#'
#' List the tables in an OData structure
#' @param root Root of data structure
#' @param query OData query to restrict data returned from structure, See
#'  \code{\link{odataR_get_table}} for examples of queries
#' @return A data.frame when successful otherwise \code{NULL}
#' @export
#' @examples
#' \dontrun{
#' odataR_set_root("http://dataderden.cbs.nl")
#' df_cat = odataR_list_tables()
#' RIVM   = odataR_list_tables(query="?$filter=Catalog eq 'RIVM'")
#' }

odataR_list_tables <- function (root = odataR_get_root_catalog(),
  query = NULL) {
  odataR_get_subtable(root,query=query)
}


#' Get information about sub tables in OData data structure
#'
#' A logical table consists of a some related sub tables. This function returns a named character vector where the names give for each sub table the type and the contents the url of the sub table. This information   can be used by the function \code{\link{odataR_get_subtable}} to retrieve the contents of a sub table
#' @param root Root of data structure
#' @param table_id Identification of table
#' @export
#' @return A named character vector where the names give for each sub table the type and the contents the url of the sub table
#' @examples
#' \dontrun{
#' subtabs = odataR_get_subtables(table_id='82935NED')
#' subtabs = odataR_get_subtables(root='http://opendata.cbs.nl/ODataFeed/OData',table_id='82935NED')
#' }

odataR_get_subtables <- function (
        root     = odataR_get_root_data(),
        table_id = NULL) {
  doc   = odataR_get_data(root, table_id)
  m1    = XML::xpathSApply(doc,"//@href/..",
    function(x) c(XML::xmlValue(x), XML::xmlAttrs(x)[["href"]]))
  hrefs = m1[2,]
  names(hrefs) =m1[1,]
  return(hrefs)
}

example_run <- function () {
  library(odataR)
  subtabs = odataR_get_subtables(table_id="82935NED")
  ti      = odataR_get_subtable(subtabs['TableInfos'],mt=NULL)
  df      = odataR_get_subtable(subtabs['TypedDataSet'])
  props   = odataR_get_subtable(subtabs['DataProperties'],mt='prop')

}

#' Convert a sub table (contents or properties) to a data.frame
#'
#' Convert a  sub table (contents or properties) to a data.frame. The function calls \code{\link{odataR_get_data}} one or more times (the latter if the server returns more than 10000 rows) to actually retrieve the XML data. With  parameter \code{mt} one indicates that the requests relates to a 'normal' sub table or relates to the properties sub table. In these cases the data is converted to a data.frame . By specifying \code{mt=NULL} the contents is not converted to a data.frame but returned as XML.
#' @param dsn Named character vector with full name of subtable
#' @param mt  Indicates destination of the XML data. Character \code{'table'} (default) or \code{'prop'} or \code{NULL}
#' @param query OData query to restrict data returned from structure, See
#'  \code{\link{odataR_get_table}} for examples of queries
#' @param save_XML Name of file to save the XML data in or NULL or default name
#' @return A data.frame with the contents of the sub table (\code{mt='table'}) or the properties of the table (\code{mt='prop'}) or the XML contents of the sub table (\code{mt=NULL})
#' @export
#' @examples
#' \dontrun{
#' subtabs = odataR_get_subtables(table_id="82935NED")
#  df      = odataR_get_subtable(subtabs['TypedDataSet'],'table')
#' }
odataR_get_subtable <- function (
        dsn,
        mt       = 'table',
        query    = NULL,
        save_XML = NULL) {
  n1       = paste0('temp_', names(dsn))
  if (is.null(save_XML)) {
    save_file_name = NULL
  } else if (nchar(save_XML) == 0) {
    save_file_name = paste0(n1, '.xml')
  } else {
    save_file_name = save_XML
  }
  t1    = odataR_get_data(dsn, NULL, query=query,
    save_file_name = save_file_name)
  if (is.null(mt))
    return(t1)
  if (mt=='prop') {
    mt1 = prop_table_fun
  } else {
    mt1 = data_table_fun
  }
  t1d = mt1(t1)
  next1 = XML::xpathSApply(t1,"//ns:link[@rel='next']",
    function(x) XML::xmlAttrs(x)[["href"]],
    namespaces = odataR_namespaces)
  while (length(next1)> 0 ) {
    t1    = odataR_get_data(next1) # no save for part2 and later
    t1d   = rbind(t1d,t1d = mt1(t1))
    next1 = XML::xpathSApply(t1,"//ns:link[@rel='next']",
      function(x) XML::xmlAttrs(x)[["href"]],
      namespaces = odataR_namespaces)
  }
  return(t1d)
}

data_table_fun <- function(doc) {
  t1n <- XML::xpathApply(doc,
    '//ns:entry[1]//m:properties[1]/d:*',
    XML::xmlName,
    namespaces = odataR_namespaces)
  t1d  = XML::xpathSApply(doc, '//m:properties/d:*',XML::xmlValue)
  t1d  = as.data.frame(matrix(t1d, ncol = length(t1n), byrow = T),
    stringsAsFactors =F)
  names(t1d) = t1n
  return(t1d)
}

prop_table_fun <- function(doc) {
  m     = XML::xpathSApply(doc, '//m:properties/d:*',
    function(x)
      c(
        XML::xpathSApply(XML::xmlParent(x), './d:ID', XML::xmlValue, namespaces = odataR_namespaces),
        XML::xmlName(x),
        XML::xmlValue(x)
      ))
  # m matrix: r1 number; r2 field ; r3 value
  uf    = unique(m[2, ])
  # "ID" "Position" "ParentID" "Type" "Key" "Title" "Description" "ReleasePolicy"
  # "Datatype" "Unit" "Decimals" "Default"
  nc    = length(uf)
  nr    = 1+max(as.numeric(m[1, ]))
  m2 = matrix(rep('', nr * nc), nrow = nr, ncol = nc)
  for (i in 1:nr) {
    m3 = m[, m[1, ] == paste(i-1)] # counting origin=0
    ix = match(m3[2, ], uf)
    m2[i, ix] = m3[3, ]
  }
  colnames(m2) = uf
  rownames(m2) = 1:nr
  as.data.frame(m2,stringsAsFactors =F)
}

#' Get table in decoded form
#'
#' The main function is \code{\link{odataR_get_table}} that extracts the information from the database and decodes the dimensions. It uses the other functions in the package behind the screens.
#' E.g. when executing \code{df = odataR_get_table(table_id="82935NED")} the following is executed:
#' \itemize{
#'  \item get names of sub tables: \cr
#'  \code{subtabs = odataR_get_subtables(table_id="82935NED")}
#'  \item retrieve coded data: \cr
#'  \code{df      = odataR_get_subtable(subtabs['TypedDataSet'])}
#'  \item retrieve properties: \cr
#'  \code{props   = odataR_get_subtable(subtabs['DataProperties'],mt='prop')}
#'  \item decode dimensions: \cr
#'  \code{df      = couple_data(df, ...)}
#' }
#' @param root Root of data structure
#' @param table_id Identification of table
#' @param query OData query to restrict data returned from structure
#' @param typed Boolean indicating 'TypedDataSet' when T or 'UntypedDataSet' when F'
#' @param keepcode Character string with dimension(s) for which the coded values are kept
#' @return A data.frame when successful otherwise \code{NULL}
#' @export
#' @section Remark:
#' See \url{http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html} for details about the query possibilities
#'
#' \code{$format=atom} is default so can be omitted, but specifying \code{$format=json} will not work in this context
#' @examples
#' \dontrun{
#' df      = odataR_get_table(table_id="82935NED")
#' df      = odataR_get_table(table_id="82935NED",keepcode = "RegioS")
#' df      = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"))
#' df      = odataR_get_table(table_id="82935NED",keepcode = "RegioS",
#'       query  = "?$format=atom&$filter=startswith(RegioS,'NL01')" )
#' df      = odataR_get_table(table_id="82935NED",
#'   query  = paste0("?$filter=startswith(RegioS,'NL01')",
#'                   "&$select=RegioS,Perioden,TotaleInvesteringen_1") )
#' df      = odataR_get_table(table_id="82935NED",
#'   query  = paste0("?$filter=startswith(RegioS,'NL01')",
#'                   "&$select=RegioS,Perioden,TotaleInvesteringen_1",
#'                   "&$skip=2&$top=3") )
#' }

odataR_get_table <- function(
    root     = odataR_get_root_data(),
    table_id = NULL,
    query    = NULL,
    typed    = T,
    keepcode = c() ) {
  tds      = ifelse(typed==F,'UntypedDataSet','TypedDataSet')
  subtabs  = odataR_get_subtables(table_id=table_id)
  df       = odataR_get_subtable(subtabs[tds],query=query)
  props    = odataR_get_subtable(subtabs['DataProperties'],mt='prop')
  tv       = topic_vars(props)
  tv       = tv %>% dplyr::filter(Key %in% names(df))
  dv       = dim_vars(props)
  dv       = dv %>% dplyr::filter(Key %in% names(df))
  couple_data(df,dv,tv,subtabs,keepcode)
}

couple_data <- function(
  df,        # data.frame with coded dimensions (e.g. read by odataR_get_subtable)
  dv,        # character vector with the names of the dimensions
  tv,        # character vector with the names of the topics
  table_list, # named vector with urls of sub tables
  keepcode   # dimension for which code value is kept
  # (e.g. read by odataR_get_subtables)
) {
  tt = df %>%
    dplyr::mutate_at(tv$Key,dplyr::funs(as.numeric))  #topics -> numeric (new syntax dplyr mutate)
    # dplyr::mutate_each_(dplyr::funs(as.numeric),tv$Key)  #topics -> numeric (old syntax dplyr mutate)
  for (dim in dv$Key)  {
    if (dim %in% keepcode ) {
      kc = T
    } else {
      kc = F
    }
    tt = couple_data_dim(tt, table_list[dim],keep_code=kc) # link dimension data
  }
  return(tt)
}

couple_data_dim <- function(tt, dsn, keep_code=F) {
  dim  = names(dsn)
  tab1 = odataR_get_subtable(dsn, 'table') %>%
    dplyr::select(Key, Title) %>%
    dplyr::rename_(.dots = setNames('Title', paste0(dim, '_decode')))
  by1  = c('Key') ; names(by1) = dim
  tt = tt %>%
    dplyr::inner_join(tab1, by = by1) %>%
    dplyr::rename_(.dots = setNames(dim, paste0(dim, '_coded'))) %>%
    dplyr::rename_(.dots = setNames(paste0(dim, '_decode'), dim))
  if (keep_code == F) {
    tt = tt %>%
      dplyr::select_(.dots = setdiff(names(.), paste0(dim, '_coded')))
  }
  return(tt)
}

topic_vars <- function(props) {
  props %>%
    dplyr::filter(Type=='Topic') %>%
    dplyr::select(Key)
}

dim_vars <- function(props) {
  props %>%
    dplyr::filter(Type %in% c('Dimension','TimeDimension','GeoDimension')) %>%
    dplyr::select(Key)
}

