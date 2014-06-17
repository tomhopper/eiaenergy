# U.S. DOE EIA Electricity Generation and Consumption Data

A package intended to provide easy access to the U.S. DOE EIA electricity data.

Converts downloaded data to a data frame and saves the data frame for easy loading via data().

Currently works with: 

* international generation data by year and generation type from 1980 to 2012;
* international consumption data by year and generation type from 1980 to 2011;
* U.S. total generation by month and generation type from January 1973 to February 2014;
* U.S. total generation by year (month = 13) and by generation type from 1949 to 2013.

## Depends

plyr
XLConnect

## To Do

* Automate the updating of year ranges when downloading and munging data.
* Provide public function to allow users to force a data update.
* Add additional datasets from the EIA.