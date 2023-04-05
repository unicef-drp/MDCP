rm(list = ls())
#library(tidyverse) #
# library(profvis)
# 
# profvis({

library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(ggplot2)

library(Rilostat) # get_ilostat_toc get_ilostat
library(rmarkdown) # render
library(wbstats) # wb_data

############### File paths #################

path.basic <- "C:/Users/palma/OneDrive - UNICEF/Migration and Displacement"
input <- file.path(path.basic, "Data/UNPD/UNMigrantStock2020")
input.wpp <- file.path(path.basic, "Data/UNPD/WPP2022")
input.unicef <- file.path(path.basic, "Data/UNICEF")
input.unicef.data <- file.path(path.basic,"Data/unicef_datawarehouse")
input.unsd <- file.path(path.basic, "Data/UNSD")
input.unhcr <- file.path(path.basic, "Data/UNHCR/GlobalTrends2020")
input.unhcr2 <- file.path(path.basic, "Data/UNHCR/GlobalTrends2021")
input.unrwa <- file.path(path.basic, "Data/UNRWA")
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
load(file.path(input.unrwa, "unrwa2021.Rdata")) 
load(file.path(input.unhcr,'unhcr2020.Rdata'))
load(file.path(input.unhcr2,'unhcr2021.Rdata'))
load(file.path(input.unhcr2,'demref2021.imp.asy.ori.Rdata'))
load(file.path(input.idmc,'idmc_2021.Rdata'))
# load(file.path(input.ilo,'ilo.Rdata')) using ILO R package
load(file.path(input.uis,'uis.Rdata')) # still bulk download

# Load child protection data from csv downloaded via unicef data warehouse
cp.br.under5 <- read.csv(file.path(input.unicef.data,'fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.PT_CHLD_Y0T4_REG...csv'), encoding = 'UTF8') |>
  filter(SEX.Sex == '_T: Total',!str_detect(REF_AREA.Geographic.area, 'UNICEF')) |>
  mutate(iso3 = str_sub(REF_AREA.Geographic.area,1,3))

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

############### Connectivity data #################
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

ilo.chldlbr<- get_ilostat("SDG_A871_SEX_AGE_RT_A")|>
  left_join(ilo.location,by = 'ref_area')

ilo.wrkngpvrty <- get_ilostat('SDG_0111_SEX_AGE_RT_A')|>
  left_join(ilo.location,by = 'ref_area')


############### World bank remittances data #################

wb.remittance.us <- wb_data("BX.TRF.PWKR.CD.DT", country = 'all',start_date = 2021,end_date = 2021)  |> 
  select(`Country Name` = country,`Country Code` = iso3c, rem.us = BX.TRF.PWKR.CD.DT)
wb.remittance.pct <- wb_data("BX.TRF.PWKR.DT.GD.ZS", country = 'all',start_date = 2021,end_date = 2021)  |> 
  select(`Country Name` = country,`Country Code` = iso3c, rem.pct = BX.TRF.PWKR.DT.GD.ZS)

############### Map layers #################

# library(terra)
# map.dir <- file.path(path.basic,'Data/unmap') 
# 
# color.palette <- "Reds" # colors for polygons with data
# 
# NoDataColor <- "grey97" # color for polygons with no data
# boundary.color <- "grey" # color for country boundaries
# background.color <- "grey97" # color for oceans, seas and lakes
# 
# # Disclaimer text 
# disclaimer.text <- "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations.
# Dotted line represents approximately the Line of Control in Jammu and Kashmir agreed upon by India and Pakistan. The final status of Jammu and Kashmir
# has not yet been agreed upon by the parties. Final boundary between the Republic of Sudan and the Republic of South Sudan has not yet been determined."
# 
# # Plot variables 
# plot.coastlines <- FALSE # outline the coastlines with the same color as country boundaries? (TRUE or FALSE)
# plot.lakes <- TRUE # show lakes polygons for the 21 large lakes (TRUE or FALSE)
# plot.antarctica <- FALSE # show antarctica polygon (TRUE or FALSE)
# 
# # Read in UN shapefiles 
# # Read in the UN cartography polygon shapefile (no antarctica)
# world.un <- vect(file.path(map.dir, "un-world-2012-no-antartica-10pct.shp"))
# 
# # Read in the Un Cartography shapefile with country/area boundaries
# bnd.un <- vect(map.dir, "2012_UNGIWG_bnd_ln_01", crs="epsg:4326")
# # convert to Robinson projection
# crs(bnd.un) <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# # Read in the Un Cartography shapefile with coastlines
# cst.un <- vect(map.dir, "2012_UNGIWG_cst_ln_01")
# # convert to Robinson projection
# crs(cst.un) <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
# crop(cst.un, bnd.un) #removing antarctica
# 
# # Read in the Un Cartography shapefile with lakes
# lks.un <- vect(map.dir, "2012_UNGIWG_lks_ply_01")
# # convert to Robinson projection
# crs(lks.un) <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
# 
# 
# # Read in the Un Cartography shapefile with Antarctica
# wld.un <- vect(map.dir, "un-world-2012-65pct")
# ant.un <- wld.un[wld.un$TERR_NAME=="Antarctica",]
# crs(ant.un) <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
# 
# 
# world.un@data$id <- as.integer(rownames(world.un@data))+1
# 
# world.un.df <- data.frame()
# for(i in 1:length(world.un)){
#   new.df <- fortify(world.un[world.un$id == i,]) |> 
#     mutate(id = i)
#   world.un.df <- rbind(world.un.df, new.df)
# }
# world.un.df <- left_join(world.un.df, select(world.un@data,id,ISO3_CODE),by = 'id')
# 
# bnd@data$id <- as.integer(rownames(bnd@data))+1
# 
# bnd.df <- data.frame()
# for(i in 1:length(bnd)){
#   new.df <- fortify(bnd[bnd$id == i,]) |> mutate(id = i)
#   bnd.df <- rbind(bnd.df, new.df)
# }
# bnd.df <- left_join(bnd.df, select(bnd@data,id,CARTOGRAPH),by = 'id') |> 
#   mutate(CARTOGRAPH = recode(CARTOGRAPH, 'International boundary line' = 'line', 
#                              "Dashed boundary line" = 'dashed', "Undetermined international dashed boundary line" = 'dashed',
#                              "Dotted boundary line" = 'dotted',"Dotted boundary line (Abyei)" ='dotted', .default = NA_character_)) |>
#   filter(!is.na(CARTOGRAPH ))



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
        output_file = paste0("Report-",country, "-test2.html")
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

ctry_list <- 'UGA'
#ctry_list <- 'USA'

for(i in ctry_list){
  render_report_try(i)
}
