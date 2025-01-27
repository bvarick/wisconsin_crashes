library(tidyverse)
library(sf)
#library(tmap)
library(leaflet)
library(RColorBrewer)
library(tidycensus)
library(htmltools)
library(magick)
library(htmlwidgets)
library(MASS)
library(raster)


## Load TOPS data ----
## To load TOPS data for the whole state for crashes involving bikes and pedestrians):
## Step 1 - download csv from the TOPS Data Retrieval Tool with the query:  SELECT * FROM DTCRPRD.SUMMARY_COMBINED C WHERE C.CRSHDATE BETWEEN TO_DATE('2023-JAN','YYYY-MM') AND LAST_DAY(TO_DATE('2023-DEC','YYYY-MM')) AND (C.BIKEFLAG = 'Y' OR C.PEDFLAG = 'Y') ORDER BY C.DOCTNMBR 
## Step 2 - include RACE1 and RACE2 for download in preferences
## Step 3 - save the csv in the "data" directory as crash-data-download_2023.csv
TOPS_data <- as.list(NULL)
for (file in list.files(path = "data/TOPS/", pattern = "crash-data-download")) {
  message(paste("importing data from file: ", file))
  year <- substr(file, 21, 24)
  csv_run <- read_csv(file = paste0("data/TOPS/",file), col_types = cols(.default = "c"))
  csv_run["retreive_date"] <- file.info(file = paste0("data/TOPS/",file))$mtime
  TOPS_data[[file]] <- csv_run
}
rm(csv_run, file, year)
TOPS_data <- bind_rows(TOPS_data)
## clean up data ----
TOPS_data <- TOPS_data %>% 
  mutate(date = mdy(CRSHDATE), 
         age1 = as.double(AGE1),
         age2 = as.double(AGE2),
         latitude  = as.double(LATDECDG),
         longitude = as.double(LONDECDG)) %>% 
  mutate(month = month(date, label = TRUE),
         year = as.factor(year(date)))

retrieve_date <- max(TOPS_data %>% filter(year %in% max(year(TOPS_data$date), na.rm = TRUE)) %>% pull(retreive_date))


# Injury Severity Index and Color -----
injury_severity <- data.frame(InjSevName = c("Injury Severity Unknown", "No apparent injury", "Possible Injury", "Suspected Minor Injury","Suspected Serious Injury","Fatality"), 
                              code = c(NA, "O", "C", "B", "A", "K"),
                              color = c("grey", "#fafa6e", "#edc346", "#d88d2d", "#bd5721", "#9b1c1c"))

injury_severity_pal <- colorFactor(palette = injury_severity$color, levels = injury_severity$InjSevName)

TOPS_data <- left_join(TOPS_data, injury_severity %>% dplyr::select(InjSevName, code), join_by(INJSVR1 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName1 = InjSevName)
TOPS_data <- left_join(TOPS_data, injury_severity %>% dplyr::select(InjSevName, code), join_by(INJSVR2 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName2 = InjSevName)

bike_roles <- c("BIKE", "O BIKE")
ped_roles <- c("PED", "O PED", "PED NO")
vuln_roles <- c(bike_roles, ped_roles)

TOPS_data <- TOPS_data %>% mutate(ped_inj = ifelse(ROLE1 %in% vuln_roles, 
                                                   INJSVR1, 
                                                   ifelse(ROLE2 %in% vuln_roles, 
                                                          INJSVR2,
                                                          NA)))

TOPS_data <- left_join(TOPS_data, injury_severity %>% dplyr::select(InjSevName, code), join_by(ped_inj == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(ped_inj_name = InjSevName)

# bike or ped
TOPS_data <- TOPS_data %>% mutate(vulnerable_role = ifelse(ROLE1 %in% bike_roles | ROLE2 %in% bike_roles, 
                                                            "Bicyclist", 
                                                            ifelse(ROLE1 %in% ped_roles | ROLE2 %in% ped_roles, 
                                                                   "Pedestrian",
                                                                   NA)))

# Race names
race <- data.frame(race_name = c("Asian", "Black", "Indian","Hispanic","White"), 
                   code = c("A", "B", "I", "H", "W"))

TOPS_data <- left_join(TOPS_data, race %>% dplyr::select(race_name, code), join_by(RACE1 == code)) %>% rename(race_name1 = race_name)
TOPS_data <- left_join(TOPS_data, race %>% dplyr::select(race_name, code), join_by(RACE2 == code)) %>% rename(race_name2 = race_name)

## make mutate TOPS_data
TOPS_data <- TOPS_data %>% 
  mutate(Year = year,
         PedestrianInjurySeverity = ped_inj_name,
         CrashDate = CRSHDATE,
         CrashTime = CRSHTIME,
         County = CNTYNAME,
         Street = ONSTR,
         CrossStreet = ATSTR) %>%
  mutate(PedestrianAge = ifelse(ROLE1 %in% vuln_roles, age1, age2))

TOPS_geom <- st_as_sf(TOPS_data %>% filter(!is.na(latitude)), coords = c("longitude", "latitude"), crs = 4326)

## load school locations ----
WI_schools <- st_read(dsn = "data/Schools/WI_schools.gpkg")
WI_schools <- WI_schools %>% 
  filter(is.double(LAT),
         LAT > 0) %>%
  dplyr::select("SCHOOL", "DISTRICT", "SCHOOLTYPE", "COUNTY", "LAT", "LON")

school_symbol <- makeIcon(iconUrl = "other/school_FILL0_wght400_GRAD0_opsz24.png",
                          iconWidth = 24,
                          iconHeight = 24,
                          iconAnchorX = 12,
                          iconAnchorY = 12)

focus_columns <- c("PedestrianInjurySeverity", "CrashDate", "CrashTime", "County", "Street", "CrossStreet", "PedestrianAge", "Year", "vulnerable_role")
focus_county <- "MILWAUKEE"


WI_schools <- WI_schools %>% filter(COUNTY %in% str_to_title(focus_county))


# generate map with leaflet ----
Pedestrian_Crash_Data <- TOPS_data %>% 
  filter(CNTYNAME == focus_county,
         !is.na(latitude)) %>%
  dplyr::select(all_of(c(focus_columns, "longitude", "latitude")))


# generate density map ----
crash_density <- kde2d(Pedestrian_Crash_Data$longitude, Pedestrian_Crash_Data$latitude, n = 200)
crash_density <- raster(crash_density)
crash_density <- cut(crash_density, breaks = 10)
crash_density_poly <- rasterToPolygons(crash_density, dissolve = T)

density_pal <- colorNumeric(palette = "YlOrRd", domain = c(min(crash_density_poly$layer, na.rm = TRUE), max(crash_density_poly$layer, na.rm = TRUE)))


# add county census data ----
census_api_key(key = substr(read_file(file = "api_keys/census_api_key"), 1, 40))
county_populations <- get_estimates(geography = "county", year = 2022, product = "population", state = "Wisconsin", geometry = TRUE) %>%
  filter(variable == "POPESTIMATE") %>%
  mutate(County = str_to_upper(str_replace(NAME, " County, Wisconsin", "")))
county_populations <- st_transform(county_populations, crs = 4326) %>% filter(County %in% focus_county)

milwaukee_crash_map <- 
  leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  addPolygons(data = county_populations,
              color = "black",
              weight = 1,
              fill = FALSE,
              group = "County Lines") %>%
  addPolygons(data = crash_density_poly,
              color = "black",
              weight = 0,
              opacity = 0.9,
              group = "Heat Map",
              fillColor = density_pal(crash_density_poly$layer))%>%
  addMarkers(data = WI_schools,
             lng=WI_schools$LON,
             lat = WI_schools$LAT,
             icon = school_symbol,
             label = lapply(paste0("<b>", WI_schools$SCHOOL, " School</b></br>",
                                   WI_schools$DISTRICT, " School District</br>",
                                   WI_schools$SCHOOLTYPE), htmltools::HTML),
             group = "Schools") %>%
  addCircleMarkers(data = Pedestrian_Crash_Data,
                   lng=Pedestrian_Crash_Data$longitude,
                   lat=Pedestrian_Crash_Data$latitude,
                   fillColor=injury_severity_pal(Pedestrian_Crash_Data$PedestrianInjurySeverity),
                   radius=4,
                   stroke=TRUE,
                   color = "black",
                   weight = 1,
                   fillOpacity = 0.8,
                   label = lapply(paste0("<b>", str_to_title(replace_na(Pedestrian_Crash_Data$vulnerable_role, ""))," </b><br>",
                                         Pedestrian_Crash_Data$CrashDate, "</br>",
                                         Pedestrian_Crash_Data$PedestrianInjurySeverity, "</br>",
                                         replace_na(Pedestrian_Crash_Data$vulnerable_role, ""), " age: ", ifelse(!is.na(Pedestrian_Crash_Data$PedestrianAge), Pedestrian_Crash_Data$PedestrianAge, "unknown age")), htmltools::HTML),
                   group = "Crash Points") %>%
  addLegend(position = "bottomleft", labels = injury_severity$InjSevName, colors = injury_severity$color, group = "Crash Points", title = "Injury Severity") %>%
  groupOptions(group = "Schools", zoomLevels = 15:20) %>%
  groupOptions(group = "Crash Points", zoomLevels = 13:20) %>%
  groupOptions(group = "County Lines", zoomLevels = 5:20) %>%
  groupOptions(group = "Heat Map", zoomLevels = 5:13)

  
milwaukee_crash_map

saveWidget(milwaukee_crash_map, file = "figures/dynamic_crash_maps/milwaukee_pedestrian_crash_map.html", selfcontained = TRUE)



