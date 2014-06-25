---
output: html_document
---
# U.S. DOE EIA Electricity Generation and Consumption Data

A package intended to provide easy access to the U.S. DOE EIA electricity data.

If raw data files (/data-raw/) are missing, downloads the data, converts it to a data frame and saves the data frame for easy loading via data().

Currently works with: 

* International generation data by year and generation type (1980 - 2012);
* International total consumption data by year (1980 - 2011);
* International total energy generation (1980 - 2012);
* International total energy consumption (1980 - 2012);
* U.S. total generation by month and generation type (from January 1973 to February 2014);
* U.S. total generation by year (month = 13) and generation type (from 1949 to 2013).

Available year ranges should update automatically when running /data-raw/eiaelectricity.R, making downloading and munging new nearly automatic. The solution depends on the dropdown `<select>` menus on the international energy pages, and may not work if the page structure changes.

## Depends

* plyr
* XLConnect

## To Do

* When updating data for (missing) raw data files, check if .rdata files exist, and if so, delete them.
* Provide public function to allow users to force a data update.
* Add additional datasets from the EIA.
* Work out means of forcing reload of dplyr after plyr if dplyr was previously loaded by the user.