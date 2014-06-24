#' @title EIA Electricity Data
#' @author Thomas Hopper
#' @import plyr
#' @import XLConnect
#' 
#' @format A data frame with variables:
#' \describe{
#'    \item{country}{Country name}
#'    \item{years 1980-2012}{Electricity produced (or consumed) each year}
#' }
#' @source \url{http://www.eia.gov/electricity/data.cfm#summary}
"eia_international"

#' @format A data frame with variables:
#' \describe{
#'   \item{MSN}{Seven-digit code for the electricity source (see Description column for long version).}
#'   \item{YYYYMM}{Date (Year-Month-Day) of electricity production. Day is arbitrarily set to "1".}
#'   \item{Value}{Electricity produced by source in Description column.}
#'   \item{Column_Order}{Column in the original Excel file.}
#'   \item{Description}{Description of electricity source.}
#'   \item{Unit}{Units for the value in Value.}
#' }
"us_elect_gen_source"