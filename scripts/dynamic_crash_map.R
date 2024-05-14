library(tidyverse)
library(sf)
#library(tmap)
library(leaflet)
library(RColorBrewer)
library(tidycensus)
library(htmltools)
library(magick)
library(htmlwidgets)

Sys.setenv(LANG = "en-US.UTF-8")

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
                              InjSevName_es = c("Gravedad de la herida desconocida", "Sin herida aparente", "Posible herida", "Sospecha de herida menor", "Sospecha de herida grave", "Fatalidad"),
                              code = c(NA, "O", "C", "B", "A", "K"),
                              color = c("grey", "#fafa6e", "#edc346", "#d88d2d", "#bd5721", "#9b1c1c"))

injury_severity_pal <- colorFactor(palette = injury_severity$color, levels = injury_severity$InjSevName)

TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(INJSVR1 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName1 = InjSevName)
TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(INJSVR2 == code)) %>% 
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

TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(ped_inj == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(ped_inj_name = InjSevName)

# bike or ped
TOPS_data <- TOPS_data %>% mutate(vulnerable_role = ifelse(ROLE1 %in% bike_roles | ROLE2 %in% bike_roles, 
                                                            "Bicyclist", 
                                                            ifelse(ROLE1 %in% ped_roles | ROLE2 %in% ped_roles, 
                                                                   "Pedestrian",
                                                                   NA)),
                                  vulnerable_role_es = ifelse(ROLE1 %in% bike_roles | ROLE2 %in% bike_roles, 
                                                              "Ciclista", 
                                                              ifelse(ROLE1 %in% ped_roles | ROLE2 %in% ped_roles, 
                                                                     "Peatón",
                                                                     NA)))

# Race names
race <- data.frame(race_name = c("Asian", "Black", "Indian","Hispanic","White"), 
                   code = c("A", "B", "I", "H", "W"))

TOPS_data <- left_join(TOPS_data, race %>% select(race_name, code), join_by(RACE1 == code)) %>% rename(race_name1 = race_name)
TOPS_data <- left_join(TOPS_data, race %>% select(race_name, code), join_by(RACE2 == code)) %>% rename(race_name2 = race_name)

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
  select("SCHOOL", "DISTRICT", "SCHOOLTYPE", "LAT", "LON")

school_translate <- data.frame(en = c("Elementary School", "High School", "Combined Elementary/Secondary School", "Middle School", "Junior High School"),
                               es = c("Escuela primaria", "Escuela secundaria", "Escuela primaria/secundaria combinada", "Escuela secundaria", "Escuela secundaria"))

WI_schools <- WI_schools %>%
  mutate(SCHOOLTYPE_es <- school_translate$es[match(WI_schools$SCHOOLTYPE, school_translate$en)])

school_symbol <- makeIcon(iconUrl = "other/school_FILL0_wght400_GRAD0_opsz24.png",
                          iconWidth = 24,
                          iconHeight = 24,
                          iconAnchorX = 12,
                          iconAnchorY = 12)


focus_columns <- c("PedestrianInjurySeverity", "CrashDate", "CrashTime", "County", "Street", "CrossStreet", "PedestrianAge", "Year", "vulnerable_role", "vulnerable_role_es")
focus_county <- "DANE"

# generate map with leaflet ----
Pedestrian_Crash_Data <- TOPS_data %>% 
  #  filter(CNTYNAME == focus_county) %>%
  select(c(all_of(focus_columns), "longitude", "latitude"))

County_Crash_Data <- Pedestrian_Crash_Data %>%
  group_by(County, Year) %>%
  summarise(TotalCrashes = n(),
            longitude = mean(longitude, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE)) %>%
  group_by(County) %>%
  summarise(MeanCrashes = mean(TotalCrashes, na.rm = TRUE),
            longitude = mean(longitude, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE))

# add population census data
census_api_key(key = substr(read_file(file = "api_keys/census_api_key"), 1, 40))
county_populations <- get_estimates(geography = "county", year = 2022, product = "population", state = "Wisconsin", geometry = TRUE) %>%
  filter(variable == "POPESTIMATE") %>%
  mutate(County = str_to_upper(str_replace(NAME, " County, Wisconsin", "")))
county_populations <- st_transform(county_populations, crs = 4326)
County_Crash_geom <- left_join(county_populations, County_Crash_Data, join_by("County"))
County_Crash_geom <- County_Crash_geom %>%
  mutate(CrashesPerPopulation = MeanCrashes/(value/100000))
County_Crash_geom$CrashesPerPopulation[is.na(County_Crash_geom$CrashesPerPopulation)] <- 0

county_pal <- colorNumeric(palette = "YlOrRd", domain = c(min(County_Crash_geom$CrashesPerPopulation, na.rm = TRUE), max(County_Crash_geom$CrashesPerPopulation, na.rm = TRUE)))

#title style
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML(paste0("Pedestrians & Bicyclists involved in a crash</br>",
                             min(year(TOPS_data$date), na.rm = TRUE),
                             " - ",
                             max(year(TOPS_data$date), na.rm = TRUE)))
) 

wisconsin_crash_map <- 
  leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  #  addControl(title, position = "topleft", className="map-title") %>%
  #  addControl(subtitle, position = "bottomleft", className="map-subtitle") %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  addPolygons(data = County_Crash_geom,
              color = "black",
              weight = 1,
              fill = FALSE,
              group = "Crash Points") %>%
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
  addPolygons(data = County_Crash_geom,
              color = "black",
              weight = 1,
              fillColor=county_pal(County_Crash_geom$CrashesPerPopulation),
              fillOpacity = 0.6,
              label = lapply(paste0("<b>", str_to_title(County_Crash_geom$County), " County</b></br>",
                                    "population: ", format(County_Crash_geom$value, nsmall=0, big.mark=","), "<br>",
                                    "average crashes per year: ", round(County_Crash_geom$MeanCrashes,0), "</br>",
                                    "average crashes/year per 100k residents: ", round(County_Crash_geom$CrashesPerPopulation,0)), htmltools::HTML),
              group = "Counties") %>%
  addLegend(position = "bottomleft", pal = county_pal, values = County_Crash_geom$CrashesPerPopulation, group = "Counties", title = "Crashes/year</br>(normalized per 100k residents)") %>%
  groupOptions(group = "Schools", zoomLevels = 13:20) %>%
  groupOptions(group = "Crash Points", zoomLevels = 10:20) %>%
  groupOptions(group ="Counties", zoomLevels = 1:9)
  
wisconsin_crash_map

saveWidget(wisconsin_crash_map, file = "figures/dynamic_crash_maps/wisconsin_pedestrian_crash_map.html", 
           selfcontained = TRUE,
           title = "Wisconsin Bike & Pedestrian Crash Map")

wisconsin_crash_map_title <- wisconsin_crash_map %>%
  addControl(title, position = "topleft", className="map-title")

saveWidget(wisconsin_crash_map_title, file = "figures/dynamic_crash_maps/wisconsin_pedestrian_crash_map_title.html", 
           selfcontained = TRUE,
           title = "Wisconsin Bike & Pedestrian Crash Map")

# Spanish version ----

Sys.setenv(LANG = "es-MX.UTF-8")

wisconsin_crash_map_es <- 
  leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  #  addControl(title, position = "topleft", className="map-title") %>%
  #  addControl(subtitle, position = "bottomleft", className="map-subtitle") %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  addPolygons(data = County_Crash_geom,
              color = "black",
              weight = 1,
              fill = FALSE,
              group = "Crash Points") %>%
  addMarkers(data = WI_schools,
             lng=WI_schools$LON,
             lat = WI_schools$LAT,
             icon = school_symbol,
             label = lapply(paste0("<b>Escuela ", WI_schools$SCHOOL, "</b></br>",
                                   "Distrito Escolar ", WI_schools$DISTRICT, "</br>",
                                   WI_schools$SCHOOLTYPE_es), htmltools::HTML),
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
                   label = lapply(paste0("<b>", str_to_title(replace_na(Pedestrian_Crash_Data$vulnerable_role_es, ""))," </b><br>",
                                         Pedestrian_Crash_Data$CrashDate, "</br>",
                                         injury_severity$InjSevName_es[match(Pedestrian_Crash_Data$PedestrianInjurySeverity, injury_severity$InjSevName)], "</br>",
                                         "edad de ", replace_na(Pedestrian_Crash_Data$vulnerable_role_es, ""), ": ", ifelse(!is.na(Pedestrian_Crash_Data$PedestrianAge), Pedestrian_Crash_Data$PedestrianAge, "edad desconocida")), htmltools::HTML),
                   group = "Crash Points") %>%
  addLegend(position = "bottomleft", labels = injury_severity$InjSevName_es, colors = injury_severity$color, group = "Crash Points", title = "Gravedad de la herida") %>%
  addPolygons(data = County_Crash_geom,
              color = "black",
              weight = 1,
              fillColor=county_pal(County_Crash_geom$CrashesPerPopulation),
              fillOpacity = 0.6,
              label = lapply(paste0("<b>Condado de ", str_to_title(County_Crash_geom$County), "</b></br>",
                                    "población: ", format(County_Crash_geom$value, nsmall=0, big.mark=","), "<br>",
                                    "choques promedio por año: ", round(County_Crash_geom$MeanCrashes,0), "</br>",
                                    "choques promedio/año por cada 100.000 habitantes: ", round(County_Crash_geom$CrashesPerPopulation,0)), htmltools::HTML),
              group = "Counties") %>%
  addLegend(position = "bottomleft", pal = county_pal, values = County_Crash_geom$CrashesPerPopulation, group = "Counties", title = "Choques por año</br>(por 100,000 habitantes)") %>%
  groupOptions(group = "Schools", zoomLevels = 13:20) %>%
  groupOptions(group = "Crash Points", zoomLevels = 10:20) %>%
  groupOptions(group ="Counties", zoomLevels = 1:9)

wisconsin_crash_map_es

saveWidget(wisconsin_crash_map_es, file = "figures/dynamic_crash_maps/wisconsin_pedestrian_crash_map_es.html", 
           selfcontained = TRUE,
           title = "Mapa de Choques de Bicicletas y Peatones en Wisconsin")

title_es <- tags$div(
  tag.map.title, HTML(paste0("Peatones y ciclistas involucrados en un choque</br>",
                             min(year(TOPS_data$date), na.rm = TRUE),
                             " - ",
                             max(year(TOPS_data$date), na.rm = TRUE)))
) 

wisconsin_crash_map_es_title <- wisconsin_crash_map_es %>%
  addControl(title_es, position = "topleft", className="map-title")

saveWidget(wisconsin_crash_map_es_title, file = "figures/dynamic_crash_maps/wisconsin_pedestrian_crash_map_es_title.html", 
           selfcontained = TRUE,
           title = "Mapa de Choques de Bicicletas y Peatones en Wisconsin")