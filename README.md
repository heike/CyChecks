
<!-- README.md is generated from README.Rmd. Please edit that file -->
CyChecks
========

*Overview*

The goal of CyChecks is to allow users easy access to the publicly available data concerning Iowa State University (the Cyclones) employee salaries.

![](http://diysolarpanelsv.com/images/iowa-state-cyclone-clip-art-27.png)

Some of the services this access could offer include:

1.  Aiding job seekers in negotiating starting salaries.
2.  Shedding light on possible pay inequities with respect to gender
3.  Seeing what the highest paid positions are at the university.

*Background*

The state of Iowa offers a large amount of public data at [this site](https://data.iowa.gov/). You can access the data by signing up for an API token [here](https://dev.socrata.com/foundry/data.iowa.gov/s3p7-wy6w). Iowa state employee salaries are available at [this site](https://data.iowa.gov/State-Finances/State-of-Iowa-Salary-Book/s3p7-wy6w). Using an API token, CyChecks provides a function to easily get data for any given year.

The data from this site does not include the employee's home department. Unfortunately, to our knowledge the data linking names to departments is not easily accessible. ISU's Human Resources Department kindly provided a list of employees with their home departments and associated colleges valid as of January 1 2019. Although we realize this isn't ideal, this data is included in the package. We have merged the 2018 employee list with salary data fom 2018, and provide this data with the package. However, we realize that the department data may not always be received in the same form, so have not included a function to automate this process.

*Summary*

This package currently includes:

1.  A function to download data directly from the iowa.gov website.
2.  Employee/salary dataset for X-2018 (scraped from the web)
3.  Employee/department/college dataset valid as of Jan 1 2019 (received by ISU Human Resources)
4.  Employee/salary/department/college dataset assumed to be valid for 2018
5.  A function to anonymize names in the web scraped dataset
6.  A shiny app for visualizing the 2018 dataset (4)

Installation
------------

You can install the development version CyChecks from [github](https://CRAN.R-project.org) with:

``` r
devtools::install_github("https://github.com/vanichols/CyChecks")
```

Example
-------

You can access the data using our XX function.

``` r
## basic example code
```
