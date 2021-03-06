#' odataR: A package for accessing OData information.
#'
#' The odataR package contains some functions to obtain information from databases with the OData interface.
#'
#' Located under the root of an OData database are some tables. To obtain the information one should specify the correct root and the name of the table. Because this package was created to access the information of the CBS (Statistics Netherlands) the default root is appropriate for this information. The name of a table (here called table_id) can be found in the documentation of the database. For the CBS you can use the catalog \url{http://opendata.cbs.nl/dataportaal/portal.html} to see the available tables and their metadata. It is also possible to get the catalog information in a data.frame by using the function \code{\link{odataR_list_tables}}.
#'
#' Setting a different root can be done by using the function \code{\link{odataR_set_root}} and querying the root by executing \code{\link{odataR_get_root}}.
#'
#' The other functions in the package are related to actually obtain information from the database. In the CBS database a table name (\code{table_id}) points to a set of sub tables that together provide the information. One 'main' sub table contains the topic data and  dimensions in coded form indicating where the topic data relates to. The other sub tables convey the meaning of the coded dimensions. E.g. a topic field could be the number of unemployed workers and the dimensions could be gender, age-class, province and year.
#'
#' The main function is \code{\link{odataR_get_table}} that extracts the information from the database and decodes the dimensions. The other functions are used by \code{odataR_get_table} but can also be used independently. E.g \code{\link{odataR_get_subtable}} can be used to get the information in encoded form (needs less memory) and with \code{odataR_get_subtable(subtabs['TableInfos'],mt=NULL)} one gets the table information in XML form (however the information in the catalog is better readable for a human.)
#'
#' For examples see \code{\link{odataR_get_table}}
#'
#' @docType package
#' @name odataR
#' @importFrom magrittr "%>%"
NULL