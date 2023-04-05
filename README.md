# Migration and Displacement Country Profiles

The [Migration and Displacement Country Profiles](https://data.unicef.org/resources/migration-and-displacement-country-profiles-mdcp/) are ...

## About the code

The app is basically a selection tool. When a country is selected, it shows the .html of the selected country. The html for the country are generated by *www/make_profile_v6.Rmd* and *www/generatereport.R*.

The MDCP is a shiny application holding every single country's profile in a container app. So unless changes are made to the map selection part of the application, the 'app.R' file won't be modified.

In '/www' folder, there are 'generatereport.R' and 'make_profile_v5.Rmd' and all single country profiles. The 'generatereport.R' file extracts data from local database and controls the rendering of 'make_profile_v5.Rmd'.

The tables/texts/charts in the MDCP are rendered by 'make_profile_v5.Rmd' which is a R markdown file that hides the coding parts within itself. One can modify this file and batch update all profiles using 'generatereport.R'.

| File                        | Description                                                                                    |
|-----------------------------|------------------------------------------------------------------------------------------------|
| app.R                       | Shinyapp that is basically a selection tool for a country                                      |
|                             |                                                                                                |
| data/all_ctry.Rdata         | List of countries to analyse ISO 3166-1 alpha-3                                                |
| data/location.Rdata         | Country table with iso codes, region, UNICEF regions. Also incluydes a table with region codes |
| data/UNSD_M49_Aug2019.Rdata | Country table with iso codes and region                                                        |
| www/generatereport.R        | Used to produce the reports for countries using the design made in www/make_profile_v6.Rmd     |
| www/make_profile_v6.Rmd     | Rmarkdown to produce the html for a country                                                    |

: List of files

## How an update of MDCP would work

## Authors

Original version from Yukun Pei.
