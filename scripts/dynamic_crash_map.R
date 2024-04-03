library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(RColorBrewer)
library(tidycensus)
library(htmltools)

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

# Injury Severy Index and Color -----
injury_severity <- data.frame(InjSevName = c("No apparent injury", "Possible Injury", "Suspected Minor Injury","Suspected Serious Injury","Fatality"), 
                              code = c("O", "C", "B", "A", "K"),
                              color = c("#fafa6e", "#edc346", "#d88d2d", "#bd5721", "#9b1c1c"))

TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(INJSVR1 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName1 = InjSevName)
TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(INJSVR2 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName2 = InjSevName)

TOPS_data <- TOPS_data %>% mutate(ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), 
                                                   INJSVR1, 
                                                   ifelse(ROLE2 %in% c("BIKE", "PED"), 
                                                          INJSVR2,
                                                          NA)))

TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(ped_inj == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(ped_inj_name = InjSevName)

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
  mutate(PedestrianAge = ifelse(ROLE1 %in% c("BIKE", "PED"), age1, age2))


TOPS_geom <- st_as_sf(TOPS_data %>% filter(!is.na(latitude)), coords = c("longitude", "latitude"), crs = 4326)

## add county borders ----
CountyBoundaries <- read_sf("data/WI_County_Boundaries_24K.geojson")

focus_columns <- c("PedestrianInjurySeverity", "CrashDate", "CrashTime", "County", "Street", "CrossStreet", "PedestrianAge", "Year")
focus_county <- "DANE"

## generate map with tmap ----
tmap_mode("view")

Pedestrian_Crash_Data <- TOPS_geom %>% 
#  filter(CNTYNAME == focus_county) %>%
  select(all_of(focus_columns))

tm_basemap("Stadia.AlidadeSmooth") +
  tm_shape(Pedestrian_Crash_Data) +
  tm_dots("PedestrianInjurySeverity", palette = injury_severity$color, popup.vars	= focus_columns)
  
tmap_save(file = "figures/dynamic_crash_maps/dynamic_crash_map.html")


# generate map with leaflet ----
Pedestrian_Crash_Data <- TOPS_data %>% 
  #  filter(CNTYNAME == focus_county) %>%
  select(c(all_of(focus_columns), "longitude", "latitude"))

injury_severity_pal <- colorFactor(palette = injury_severity$color, domain = injury_severity$InjSevName)

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
county_populations <- get_estimates(geography = "county", year = 2022, product = "population", state = "Wisconsin") %>%
  filter(variable == "POPESTIMATE") %>%
  mutate(County = str_to_upper(str_replace(NAME, " County, Wisconsin", "")))
County_Crash_Data <- left_join(County_Crash_Data, county_populations, join_by("County"))
County_Crash_Data <- County_Crash_Data %>%
  mutate(CrashesPerPopulation = MeanCrashes/value*100000)

county_pal <- colorNumeric(palette = "YlOrRd", domain = c(min(County_Crash_Data$CrashesPerPopulation, na.rm = TRUE), max(County_Crash_Data$CrashesPerPopulation, na.rm = TRUE)))

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
  tag.map.title, HTML("Pedestrian Crashes</br>2017-2023")
)  

tag.map.subtitle <- tags$style(HTML("
  .leaflet-control.map-subtitle { 
    transform: translate(0%,0%);
    position: fixed !important;
    left: 90%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: regular;
    font-size: 12px;
  }
"))

subtitle <- tags$div(
  tag.map.subtitle, HTML("data from UW TOPS lab - retrieved 4/2024</br>per direction of the WisDOT Bureau of Transportation Safety")
)  

leaflet() %>%
  addControl(title, position = "topleft", className="map-title") %>%
  addControl(subtitle, position = "bottomleft", className="map-subtitle") %>%
    addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
  addCircleMarkers(data = Pedestrian_Crash_Data,
                   lng=Pedestrian_Crash_Data$longitude,
                   lat=Pedestrian_Crash_Data$latitude,
                   fillColor=injury_severity_pal(Pedestrian_Crash_Data$PedestrianInjurySeverity),
                   radius=3,
                   stroke=TRUE,
                   color = "black",
                   weight = 1,
                   fillOpacity = 0.8,
                   group = "Crash Points") %>%
  addLegend(position = "bottomleft", labels = injury_severity$InjSevName, colors = injury_severity$color, group = "Crash Points", title = "Injury Severity") %>%
  groupOptions(group = "Crash Points", zoomLevels = 10:20) %>%
  addPolygons(data = CountyBoundaries,
              fill = FALSE,
              color = "black",
              weight = 1) %>%
  addCircleMarkers(data = County_Crash_Data,
                   lng=County_Crash_Data$longitude,
                   lat=County_Crash_Data$latitude,
                   fillColor=county_pal(County_Crash_Data$CrashesPerPopulation),
                   radius=County_Crash_Data$value/20000,
                   stroke = TRUE,
                   color = "black",
                   weight = 1,
                   fillOpacity = 0.7,
                   group = "Counties") %>%
  addLegend(position = "bottomleft", pal = county_pal, values = County_Crash_Data$CrashesPerPopulation, group = "Counties", title = "Mean Crashes/Year in County</br>(per 100,000 residents)") %>%
#  addLegendSize(position = "bottomright", color = "black", shape = "circle", values = County_Crash_Data$value.y, group = "Counties", title = "Population of County") %>%
  groupOptions(group ="Counties", zoomLevels = 1:9)

