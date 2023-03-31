# MDCP
Migration and Displacement Country Profiles


The workflow of updating MDCP

The MDCP is a shiny application holding every single country's profile in a container app. So unless changes are made to the map selection part of the application, the 'app.R' file won't be modified.

In '/www' folder, there are 'generatereport.R' and 'make_profile_v5.Rmd' and all single country profiles. The 'generatereport.R' file extracts data from local database and controls the rendering of 'make_profile_v5.Rmd'.

The tables/texts/charts in the MDCP are rendered by 'make_profile_v5.Rmd' which is a R markdown file that hides the coding parts within itself. One can modify this file and batch update all profiles using 'generatereport.R'.