rm(list = ls())
# library(profvis)
# 
# profvis({

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(tidyr)
library(ggplot2)

library(Rilostat) # get_ilostat_toc get_ilostat
library(rmarkdown) # render
library(wbstats) # wb_data

library(circlize)
library(knitr)
library(kableExtra)

############### File paths #################

path.basic <- "C:/Users/palma/OneDrive - UNICEF/Migration and Displacement"
input <- file.path(path.basic, "Data/UNPD/UNMigrantStock2020")
input.wpp <- file.path(path.basic, "Data/UNPD/WPP2022")
input.unicef <- file.path(path.basic, "Data/UNICEF")
input.unicef.data <- file.path(path.basic,"Data/unicef_datawarehouse")
input.unsd <- file.path(path.basic, "Data/UNSD")
input.unhcr <- file.path(path.basic, "Data/UNHCR/GlobalTrends2020")
input.unhcr2 <- file.path(path.basic, "Data/UNHCR/GlobalTrends2021")
input.idmc <- file.path(path.basic, "Data/IDMC/IDMC2022")
input.wup <- file.path(path.basic,'Data/UNPD/WUP2018')
input.ilo <- file.path(path.basic, "Data/ILO")
input.uis <- file.path(path.basic, "Data/UIS")
input.igme <- file.path(path.basic, "Data/IGME")

############### Load files #################

load(file.path(input.wup, 'wup2018_wpp2022.Rdata'))
load(file.path(input.wpp, "wpp_location.Rdata"))
load(file.path(input.wpp, "wpp_age_group.Rdata"))
load(file.path(input.wpp, "wpp_5y_age_group.Rdata"))
load(file.path(input.wpp,'wpp_dem.Rdata'))
load(file.path(input.wpp,'wpp_fertility.Rdata'))
load(file.path(input, "UN_MigrantStock.Rdata"))
load(file.path(input, "UN_MigrantStockAge.Rdata"))
load(file.path(input, "UN_MigrantStockAge0to17.Rdata"))
load(file.path(input, "UN_MigrantStockByOriginAndDestination.Rdata"))
load(file.path(input.unhcr,'unhcr2020.Rdata'))
load(file.path(input.unhcr2,'unhcr2021.Rdata'))
load(file.path(input.unhcr2,'demref2021.imp.asy.ori.Rdata'))
load(file.path(input.idmc,'idmc_2021.Rdata'))
# load(file.path(input.ilo,'ilo.Rdata')) using ILO R package
load(file.path(input.uis,'uis.Rdata')) # still bulk download

### UNHCR, UNRWA Refugee data ####

UNRWA_registeredrefugees_2022Q4 <- read_csv(paste0(path.basic, "/Data/UNRWA/UNRWA_registeredrefugees_2022Q4.csv"))
Missing_Migrants_Global_Figures_allData_withCountries <- read_csv(paste0(path.basic, "/Data/MMP/Missing_Migrants_Global_Figures_allData_withCountries.csv"))

### UNHCR Connectivity data ####
dcr.df <- rbind(tibble(V1 = c( "Armenia", "Azerbaijan", "Bosnia and Herzegovina", "Croatia", "France",  
                               "Germany","Hungary","Luxembourg","Montenegro","Romania", "Russian Federation",
                               "Switzerland","Tajikistan","Turkey","Ukraine",'Czechia','Serbia','North Macedonia',
                               'Argentina',"Brazil","Chile","Costa Rica","Mexico","Panama","Peru",
                               'Israel',"Kuwait","Lebanon","Libya",'Canada',
                               "China","Indonesia","Japan","Malaysia","Papua New Guinea" , 'Republic of Korea' ), 
                       V2 = 0,V3 = 0, V4 = 100),
                c('Venezuela (Bolivarian Republic of)',0,13,87),c('Bolivia (Plurinational State of)',0,100,0),
                c('Algeria',0,100,0),c('Djibouti',8.3,88.7,3.0),c('Egypt',0,7.1,92.9),c('Iraq',0,41.2,58.8),c('Iran (Islamic Republic of)',0,64.1,35.9),c('Jordan',0,1.5,98.5),c('Syrian Arab Republic',0,95.2,4.8),c('Yemen',0,17.4,82.6), 
                c('Afghanistan',0,100,0),c('Bangladesh',0,37.3,62.7),c('India',0,87.1,12.9),c('Nepal',0,100,0),c('Pakistan',2.5,34.0,63.5),
                c('Botswana',0,100,0),c('Burkina Faso',0,7.7,92.3),c('Burundi',30.6,69.4,0),c('Cameroon',30.1,64,5.9),c('Central African Republic',52.7,23.9,23.4),c('Chad',6.3,93.1,0.6),
                c('Congo',3.5,77.5,19),c("C?te d'Ivoire",0,51.4,48.6),c('Democratic Republic of the Congo',37.1,56.8,6.1),c('Eritrea',0,100,0),c('Ethiopia',59.8,39.4,0.9),
                c('Gabon',0,0,100),c('Ghana',0,25.4,74.6),c('Guinea',0,77.6,22.4),c('Kenya',0,27.9,72.1),c('Liberia',8.7,91.3,0),c('Malawi',0,0,100),c('Mali',0,100,0),
                c('Mauritania',0,0,100),c('Mozambique',61.5,0,38.5),c('Namibia',0,100,0),c('Niger',16.4,13.3,70.2),c('Nigeria',0,100,0),c('Rwanda',0,75.1,24.9),
                c('Senegal',0,100,0),c("Somalia",0,100,0),c("South Africa",0,0,100),c("South Sudan",94,6,0),c("Sudan",4.2,72,23.8),c('Togo',8.8,75.9,15.4),
                c('Uganda',0,78.5,21.5),c('United Republic of Tanzania',2.6,72.3,25.2),c('Zambia',66,0,34),c('Zimbabwe',0,81.9,18.1),
                c('Thailand',19.2,21.8,59.1)) |> mutate_at(vars(2:4),as.numeric) |>
  rbind(c('North America',0,0,100),
        c('Middle East & North Africa',0, 25.4,74.6),c('South Asia',1.7, 48.5, 49.8),c('Sub-Saharan Africa', 22.8, 51.4, 25.8),c('Latin America & Caribbean',0.0,9.0,91.0), 
        c('East Asia & Pacific', 4.6,5.3,90.1),c('Europe & Central Asia',0,0,100), c('WORLD',7.0,31.1,61.9)) |> as_tibble() |> mutate_at(vars(2:4),as.double)

# wpp.location |> filter(iso3 %in% all_ctry, !(area %in% dcr.ctry.ref$country)) |> select(country = area, unhcr.subregion = subregion)

colnames(dcr.df) <- c('area', 'pct.nocov','pct.2g','pct.3g+')
dcr.ctry.ref <- tibble(country = dcr.df[1:86,1] |> pull(),
                       unhcr.subregion = rep(NA,86)) |> 
  mutate(unhcr.subregion = ifelse(country %in% c('Israel',"Kuwait","Lebanon","Libya","Algeria","Djibouti","Egypt","Iraq","Iran (Islamic Republic of)","Jordan","Syrian Arab Republic" ,"Yemen" ),'Middle East & North Africa',unhcr.subregion)) |>
  mutate(unhcr.subregion = ifelse(country %in% c("Botswana","Burkina Faso","Burundi","Cameroon","Central African Republic","Chad","Congo","C?te d'Ivoire","Democratic Republic of the Congo","Eritrea","Ethiopia","Gabon","Ghana","Guinea","Kenya","Liberia","Malawi","Mali","Mauritania","Mozambique","Namibia",
                                                 "Niger","Nigeria","Rwanda","Senegal","Somalia","South Africa","South Sudan","Sudan","Togo","Uganda","United Republic of Tanzania","Zambia","Zimbabwe"),'Sub-Saharan Africa',unhcr.subregion)) |>
  mutate(unhcr.subregion = ifelse(country %in% c('Argentina',"Brazil","Chile","Costa Rica","Mexico","Panama","Peru",'Venezuela (Bolivarian Republic of)',  'Bolivia (Plurinational State of)' ),'Latin America & Caribbean',unhcr.subregion)) |>
  mutate(unhcr.subregion = ifelse(country %in% c("Afghanistan","Bangladesh","India","Nepal","Pakistan"),'South Asia',unhcr.subregion)) |>
  mutate(unhcr.subregion = ifelse(country %in% c("China","Indonesia","Japan","Malaysia","Papua New Guinea" , 'Republic of Korea' ,'Thailand'),'East Asia & Pacific',unhcr.subregion)) |>
  mutate(unhcr.subregion = ifelse(country %in% c("Armenia", "Azerbaijan", "Bosnia and Herzegovina", "Croatia", "France",  "Germany","Hungary","Luxembourg","Montenegro","Romania", "Russian Federation","Switzerland","Tajikistan","Turkey","Ukraine",'Czechia','Serbia','North Macedonia'),'Europe & Central Asia',unhcr.subregion)) |>
  mutate(unhcr.subregion = ifelse(country == 'Canada','North America',unhcr.subregion))


### Education data ####
##UIS data Gross Enrollment rate downloaded here: http://data.uis.unesco.org/
uis <- read.csv(file.path(path.basic,'Data/UIS/GrossEnrolmentRatio.csv'),
                encoding = 'UTF-8', stringsAsFactors = F)

### Child protection data ####
# Load child protection data from csv downloaded via unicef data warehouse
cp.br.under5 <- read.csv(file.path(input.unicef.data,'fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.PT_CHLD_Y0T4_REG...csv'), encoding = 'UTF8') |>
  filter(SEX.Sex == '_T: Total',!str_detect(REF_AREA.Geographic.area, 'UNICEF')) |>
  mutate(iso3 = str_sub(REF_AREA.Geographic.area,1,3))

### Mortality data ####
# Load IGME data from sheet 'UNIGME2022_Country_Rates', from Yang Liu in Mortality team yanliu@unicef.org
igme <- read.csv(file.path(input.igme,'UNIGME2022_Country_Rates.csv')) |>
  filter( Indicator %in% c('Infant mortality rate','Neonatal mortality rate','Under-five mortality rate','Mortality rate age 10-19'), Sex == 'Total') |>
  mutate(Year = as.integer(Year + 0.5 ), Indicator = factor(Indicator,levels = c('Infant mortality rate','Neonatal mortality rate','Under-five mortality rate','Mortality rate age 10-19')))

### ALL WPP countries
wpp.location <- wpp.location |> mutate(subregion = ifelse(region == 'Northern America', 'Northern America',subregion), subregion.id = ifelse(region == 'Northern America', 905 ,subregion.id))

all_ctry <- c(intersect(wpp.age.group |> filter(area.id < 900, year == 2022,poptotal.thsd >90) |> pull(iso3),
                        mig.stock.orig.dest |> filter(year == 2020, dest.area.id<900) |> distinct(dest.iso3) |> pull()))
all_ctry <- all_ctry[all_ctry!= 'TWN']
#save(all_ctry,file = 'all_ctry.Rdata')



######################## ILO data from API################################
# TOC 
ilo.toc <- get_ilostat_toc()
#write.csv(ilo.toc, file=file.path(input.ilo, str_c("ilo.toc",  format(Sys.time(), "%Y_%m"), ".csv")))

ilo.location <- get_ilostat_toc(
  segment = 'ref_area') 
ilo.location <- ilo.location |> filter(freq == 'A')

# UNE_2EAP_SEX_AGE_RT_A: Unemployment rate by sex and age -- ILO modelled estimates (%) 
ilo.unempl.pct.est <- get_ilostat("UNE_2EAP_SEX_AGE_RT_A") |>
  left_join(ilo.location,by = 'ref_area')

# EIP_NEET_SEX_RT_A: Share of youth not in employment, education or training (NEET) by sex (%)
ilo.neet.pct <- get_ilostat("EIP_NEET_SEX_RT_A") |>
  left_join(ilo.location,by = 'ref_area')

# EIP_2EET_SEX_RT_A: Share of youth not in employment, education or training (NEET) by sex (%), modelled estimates
ilo.neet.pct.est <- get_ilostat("EIP_2EET_SEX_RT_A") |>
  left_join(ilo.location,by = 'ref_area')

#SDG indicator 8.7.1 - Proportion of children engaged in economic activity (%)
ilo.chldlbr<- get_ilostat("SDG_A871_SEX_AGE_RT_A")|>
  left_join(ilo.location,by = 'ref_area')

#SDG indicator 1.1.1 - Working poverty rate (percentage of employed living below US$1.90 PPP) (%)
ilo.wrkngpvrty <- get_ilostat('SDG_0111_SEX_AGE_RT_A')|>
  left_join(ilo.location,by = 'ref_area')


############### World bank data #################
OGHIST <- read_xlsx(file.path(path.basic,'Data/WB/OGHIST.xlsx'), sheet = 'Country Analytical History', skip = 5) |> 
  gather(key = 'date',value = 'class.iso3',3:37)

wb.remittance.us <- wb_data("BX.TRF.PWKR.CD.DT", country = 'all',start_date = 2021,end_date = 2021)  |> 
  select(`Country Name` = country,`Country Code` = iso3c, rem.us = BX.TRF.PWKR.CD.DT) |> 
  mutate(rem.us = rem.us/1000000)
wb.remittance.pct <- wb_data("BX.TRF.PWKR.DT.GD.ZS", country = 'all',start_date = 2021,end_date = 2021)  |> 
  select(`Country Name` = country,`Country Code` = iso3c, rem.pct = BX.TRF.PWKR.DT.GD.ZS)

############### Helper functions and variables ###############
## Input time period of concern for population numbers
yr_bg1 <- 1950 
yr_ed1 <- 2050


findcolor <- function(year) {
  func <-colorRampPalette(unicef_colors_bin[1])
  color <- func(7)[year-2012]
  return(color)}


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#return NA if there isn't value. Used in education data
pur <- function(x) ifelse(is.null(x),NA,x)

scale_round_k <- function(n){
  ifelse(n<1000, round(n/1000,digits=2), round(n/1000))
}

#Color palettes
#UNICEF blue and secondary colors
unicef_colors <- c("#1CABE2", "#00833D", "#80BD41", 
                   "#FFC20E", "#F26A21", "#E2231A",
                   "#961A49", "#6A1E74", "#D8D1C9", 
                   "#777779", "#2D2926", "#374EA2")

unicef_colors_tri <- c("#14789e", "#1CABE2", "#60c4eb", 
                       "#b3880a", "#ffc20e", "#ffd456",
                       "#535355", "#777779", "#a0a0a1")

#bin UNICEF cyan - UNICEF cyan (20%)
unicef_colors_bin <- c("#1CABE2", "#d2eef9")

# Age group labels
age.group <- c("0to4", "5to9", "10to14", "15to19", "20to24", "25to29", "30to34","35to39" ,"40to44" ,"45to49", "50to54", "55to59" ,"60to64", "65to69", "70to74" ,"75plus", "total" )
age.group.labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34","35-39" ,"40-44" ,"45-49", "50-54", "55-59" ,"60-64", "65-69", "70-74" ,"75+", "Total" )
age.group.labels <- tibble(age.group=factor(age.group, levels = age.group),
                           age.group.label=factor(age.group.labels, levels=age.group.labels))


############### Render profile ###############

# render_report = function(country) {
#   rmarkdown::render(
#     "www/make_profile_v5.Rmd", params = list(
#       data = country
#     ),
#     output_file = paste0("Report-",country, "-test.html")
#   )
# }
error_list <- 'NO'

render_report_try <- function(country) {
  out <- tryCatch(
    {
      rmarkdown::render(
        "www/make_profile_v6.Rmd", params = list(
          data = country
        ),
        output_file = paste0("profile_v6_", country, ".html")
      )
      
    },
    error=function(cond) {
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      error_list <<- c(error_list,country)
    },
    finally={
      
      message(paste("Processed country:", country))
    }
  )    
  return(out)
}

prospect_country <- c( "UGA","KEN", "SDN", "ETH","EGY","JOR", "IRQ", "LBN")


# ctry_list <- c( "UGA","KEN", "SDN", "ETH","EGY","JOR", "IRQ", "LBN", #prospect country
#                 "BGD", "CMR", "ECU", "HND", "IDN", "LBY", "RWA", # blueprint
#                 "SYR","TUR")


#list of countries to generate reports
#all countries
#ctry_list <- all_ctry

#selected countries
ctry_list <- "MEX"

for(i in ctry_list){
  render_report_try(i)
}
