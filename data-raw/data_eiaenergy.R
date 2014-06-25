#' @title eiaenergy
#' @author Thomas Hopper <tomhopper@gmail.com>
#' @name
#' @description
#' to rename countries to match map data,
#' \enumerate{
#'   \item{make sure that the country names are factors}
#'   \item{set the country column name to "id"}
#'   \item{set up a vector of name equivalencies}
#'   \item{load the plyr package}
#'   \item{apply the equivalency to the country name column}
#' }
#' 

#' TODO:
#' a) More data sets from EIA
#' b) Build package and test

#library(ggplot2)
#' requires plyr
#' requires XLConnect

getEIAInternationalData <- function (data.URI, raw.file.name) {
  tmp <- tempfile(fileext=".xls")
  download.file(data.URI, tmp, quiet = TRUE, mode = "wb")
  file.rename(from=tmp, to=raw.file.name)
  data.workbook <- loadWorkbook(filename = raw.file.name, create = FALSE)
  return(data.workbook)
}

mungeEIAInternationalData <- function(data.workbook, first.date, last.date) {
  eia.data <- readWorksheet(data.workbook, sheet = "Data3", startRow = 3, startCol = 1, header = TRUE, drop = c(2))
  colnames(eia.data) <- c("country",as.character(first.date:last.date))
  eia.data <- eia.data[-1,]
  rownames(eia.data) <- eia.data$country
  return(eia.data)
}

get_dates <- function(uri) {
  webpage <- suppressWarnings(readLines(uri))
  #' get the line number containing the start of our select control
  startline <- grep(pattern = "<select name=\\\"year1\\\"", x = webpage, value = FALSE)
  #' start our search with the next line
  current.line <- startline + 1
  #' initialize years as a vector of integers
  years <- c(NA_integer_)
  #' grab each line until we reach the end of the select control.
  #' note: if grep() does not find a match, it returns a vector of length == 0
  while(length(grep(pattern = "</select>", x = webpage[current.line], value = FALSE)) == 0) {
    #' get the number listed in the control
    #' note: may not be robust if the HTML includes ">" and "<" that are not part of "option"
    search.value <- as.numeric(sub(pattern = "^.*>(.*)<.*", replacement = "\\1", x = webpage[current.line]))
    #' if the returned value is "NA," then we didn't find an option line
    #' if the returned value is not "NA," then we have found a year. Add this as an element of "years."
    if(!is.na(search.value)) {
      years <- c(years, search.value)
    }
    #' increment current.line so we look at the next line
    current.line <- current.line + 1
  }
  #' years[1] is NA, so strip out all NA.
  years <- years[!is.na(years)]
  #' sort years from low to high
  years <- sort(x = years, decreasing = FALSE)
  return(years)
}

if (require(plyr)) {
  
  #' Data available by csv download
  
  
  #' U.S. monthly net electricity generation by source and total
  #' MER_T07_02A.csv from
  #' \link{http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T07.02A}
  if (!file.exists("data-raw/MER_T07_02A.csv")) {
    tmp <- tempfile(fileext = ".csv")
    download.file("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T07.02A", tmp,
                  quiet = TRUE)
    #' move to data-raw
    file.rename(from=tmp, to="data-raw/us_elect_gen_source_month.csv")
    
    #' Read the data in and clean it up:
    #' Date data is stored as YYYYMM; we want actual date, with day. For simplicity, we'll use
    #' the first day of the month.
    #' Also, dates include annual totals encoded as month = 13, which we don't want
    #' (or want to separate out into a separate data frame)
    us.by.month <- read.csv(file="data-raw/us_elect_gen_source_month.csv", header=TRUE, sep=",", dec=".", , quote="\"")
    us.by.month$YYYYMM <- as.character(us.by.month$YYYYMM)
    us.by.year <- us.by.month[substr(us.by.month$YYYYMM, nchar(us.by.month$YYYYMM)-2+1, nchar(us.by.month$YYYYMM)) == "13",]
    us.by.month <- us.by.month[!substr(us.by.month$YYYYMM, nchar(us.by.month$YYYYMM)-2+1, nchar(us.by.month$YYYYMM)) == "13",]
    us.by.month$YYYYMM <- as.Date(paste(us.by.month$YYYYMM, "01", sep=""), "%Y%m%d")
    #' Generation Values in billions (10^9) kWh / month, includes "Not Available" values
    #' and imports as a factor. Convert "Not Available" to NA and then convert from factor
    #' to numeric.
    us.by.month$Value <- revalue(x=us.by.month$Value, replace=c("Not Available" = NA))
    us.by.month$Value <- as.numeric(levels(us.by.month$Value))[us.by.month$Value]
    
    comment(us.by.month$MSN) <- "See column Description for explanation of codes"
    comment(us.by.month) <- "Data available from link{http://www.eia.gov/electricity/data.cfm#summary}. Accessed 2014-06-12"
    
    #' Visual check of the results
    #ggplot(data = us.by.month, aes(x = YYYYMM, y = Value)) + geom_line() + facet_grid(facets=MSN~., scales="free_y")
    
    
    us.by.year$YYYYMM <- as.numeric(substr(us.by.year$YYYYMM, start = 1, stop = nchar(us.by.year$YYYYMM)-2))
    us.by.year$Value <- revalue(x=us.by.year$Value, replace=c("Not Available" = NA))
    us.by.year$Value <- as.numeric(levels(us.by.year$Value))[us.by.year$Value]
    comment(us.by.year) <- "Data available from link{http://www.eia.gov/electricity/data.cfm#summary}"
    
    save(list = c("us.by.month", "us.by.year"), file = "data/us_elect_gen_source.rdata")
  }
  
  
  #' Data only available by Excel file download
  if (require(XLConnect)) {
    #' The URI for for net electricity generation by country data requires the begin and end dates.
    #' We also need this to name the data frame columns.
    #' As of June 2014, the maximum range is 1980 to 2012
    years <- get_dates("http://www.eia.gov/cfapps/ipdbproject/IEDIndex3.cfm?tid=2&pid=2&aid=12")
    first.date <- years[1]
    last.date <- years[length(years)]
    #' @description
    #' Total net generation, by country, 1980 - 2012 (first and last available as of 2014-06-12)
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=2&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Excel Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=2&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    #' total net generation, all countries by region, 1980 - 2012 (includes region roll-up; not good for mapping):
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=2&aid=12&cid=regions,&syid=1980&eyid=2012&unit=BKWH}
    raw.file.name <- c("data-raw/int_net_total_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=2&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep="")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name) 
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.total.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date)
    }
    #' Non-hydro renewables generation, by country, 1980 - 2012
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=34&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Excel Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=34&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    raw.file.name <- c("data-raw/int_net_nonhydro_renewable_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=34&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep = "")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.nonhydro.renewable.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Nuclear net generation, all countries by year, 1980 - 2012
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=27&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=27&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    raw.file.name <- c("data-raw/int_net_nuclear_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=27&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep = "")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.nuclear.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Fossil fuel net generation, all countries by year, 1980 - 2012
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=28&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=28&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    raw.file.name <- c("data-raw/int_net_fossil_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=28&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep = "")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.fossil.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Hydroelectric net generation, all countries by year, 1980 - 2012
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=33&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=33&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    raw.file.name <- c("data-raw/int_net_hydro_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=33&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep = "")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.hydro.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' All renewable generation, all countries by year, 1980 - 2012
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=29&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=29&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    raw.file.name <- c("data-raw/int_net_renewable_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=29&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep = "")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.renewable.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Pumped hydro storage, all countries by year, 1980 - 2012
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=82&aid=12&cid=all,&syid=1980&eyid=2012&unit=BKWH}
    #' Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=82&pdid=&aid=12&cid=all&syid=1980&eyid=2012&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    raw.file.name <- c("data-raw/int_net_pumped_hydro.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=82&pdid=&aid=12&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep = "")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.net.pumped.hydro <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Total primary energy production
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=44&pid=44&aid=1&cid=all,&syid=1980&eyid=2012&unit=QBTU}
    #' Excel download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=44&pid=44&pdid=44&aid=1&cid=regions&syid=1980&eyid=2012&form=&defaultid=3&typeOfUnit=BTUUNIT&unit=QBTU&products=}
    raw.file.name <- c("data-raw/int_total_energy_gen.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=44&pid=44&pdid=44&aid=1&cid=regions&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=3&typeOfUnit=BTUUNIT&unit=QBTU&products=", sep="")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.total.energy.gen <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Total primary energy consumption
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=44&pid=44&aid=2&cid=regions&syid=1980&eyid=2012&unit=QBTU}
    #' Excel download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=44&pid=44&pdid=44,45,46,47&aid=2&cid=regions&syid=1980&eyid=2012&form=&defaultid=3&typeOfUnit=BTUUNIT&unit=QBTU&products=}
    raw.file.name <- c("data-raw/int_total_energy_con.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=44&pid=44&pdid=44,45,46,47&aid=2&cid=regions&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=3&typeOfUnit=BTUUNIT&unit=QBTU&products=", sep="")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.total.energy.con <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    #' Total electric power consumption, all countries by year, 1980 - 2011
    #' Interactive:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=2&pid=2&aid=2&cid=all,&syid=1980&eyid=2011&unit=BKWH}
    #' Download:
    #' \link{http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=2&pdid=&aid=2&cid=all&syid=1980&eyid=2011&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=}
    years <- get_dates("http://www.eia.gov/cfapps/ipdbproject/IEDIndex3.cfm?tid=2&pid=2&aid=2")
    first.date <- years[1]
    last.date <- years[length(years)]
    raw.file.name <- c("data-raw/int_total_elect_consumption.xls")
    if (!file.exists(raw.file.name)) {
      data.URI <- paste("http://www.eia.gov/cfapps/ipdbproject/XMLinclude_3.cfm?tid=2&pid=2&pdid=&aid=2&cid=all&syid=", first.date, "&eyid=", last.date, "&form=&defaultid=2&typeOfUnit=STDUNIT&unit=BKWH&products=", sep="")
      data.workbook <- getEIAInternationalData(data.URI, raw.file.name)
      setMissingValue(data.workbook, c("--", "NA", "s"))
      int.total.elect.consumption <- mungeEIAInternationalData(data.workbook, first.date, last.date) 
    }
    
    #' Save RDATA file with all international data
    int.data.list <- c("int.net.total.gen", "int.net.nonhydro.renewable.gen", "int.net.nuclear.gen", "int.net.fossil.gen", "int.net.hydro.gen", "int.net.renewable.gen", "int.net.pumped.hydro", "int.total.elect.consumption", "int.total.energy.gen", "int.total.energy.con")
    save(list=int.data.list, file="data/eia_international.rdata")
    #' 
    #' \link{http://stackoverflow.com/questions/11810605/replace-contents-of-factor-column-in-r-dataframe}
  } else {
    stop("The 'XLConnect' package is required to process the Excel files downloaded from the DOE EIA. Please install it and then re-load eiaenergy.")
  }
} else {
  stop("The 'plyr' package is required to process data. Please install it and then re-load eiaenergy.")
}