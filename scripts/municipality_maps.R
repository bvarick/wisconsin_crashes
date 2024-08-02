library(tidyverse)
library(ggmap)
library(sf)
library(osrm)
library(smoothr)
library(ggnewscale)
library(RColorBrewer)
library(magick)
library(rsvg)
library(parallel)
library(tidycensus)

## add data from WiscTransPortal Crash Data Retrieval Facility ----
## query:  SELECT * 
## FROM DTCRPRD.SUMMARY_COMBINED C 
## WHERE C.CRSHDATE BETWEEN TO_DATE('2022-JAN','YYYY-MM') AND 
## LAST_DAY(TO_DATE('2022-DEC','YYYY-MM')) AND 
## (C.BIKEFLAG = 'Y' OR C.PEDFLAG = 'Y') 
## ORDER BY C.DOCTNMBR

## Load TOPS data ----
## load TOPS data for the whole state (crashes involving bikes and pedestrians),
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

## clean up data
TOPS_data <- TOPS_data %>% 
  mutate(date = mdy(CRSHDATE), 
         age1 = as.double(AGE1),
         age2 = as.double(AGE2),
         latitude  = as.double(LATDECDG),
         longitude = as.double(LONDECDG)) %>% 
  mutate(month = month(date, label = TRUE),
         year = as.factor(year(date)))

retrieve_date <- max(TOPS_data %>% filter(year %in% max(year(TOPS_data$date), na.rm = TRUE)) %>% pull(retreive_date))

# county index
counties <- data.frame(name = c("Dane", "Milwaukee"),
                       CNTYCODE = c(13, 40),
                       COUNTY = c("DANE", "MILWAUKEE"))

# Injury Severy Index and Color -------------------------------------------
# injury severity index
injury_severity <- data.frame(InjSevName = c("Injury severity unknown", "No apparent injury", "Possible Injury", "Suspected Minor Injury","Suspected Serious Injury","Fatality"), 
                              code = c(NA, "O", "C", "B", "A", "K"),
                              color = c("grey", "#fafa6e", "#edc346", "#d88d2d", "#bd5721", "#9b1c1c"))

TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(INJSVR1 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName1 = InjSevName)
TOPS_data <- left_join(TOPS_data, injury_severity %>% select(InjSevName, code), join_by(INJSVR2 == code)) %>% 
  mutate(InjSevName = factor(InjSevName, levels = injury_severity$InjSevName)) %>%
  rename(InjSevName2 = InjSevName)
# add bike or pedestrian roles ----

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
                                                                  NA)))
## load bike LTS networks
bike_lts <- as.list(NULL)
for(file in list.files("data/bike_lts")) {
  county <- str_sub(file, 10, -9)
  lts_run <- st_read(paste0("data/bike_lts/", file))
  lts_run[["lts"]] <- as.factor(lts_run$LTS_F)
  bike_lts[[county]] <- lts_run
}
bike_lts_scale <- data.frame(code = c(1, 2, 3, 4, 9),
                             color = c("#1a9641",
                                       "#a6d96a",
                                       "#fdae61",
                                       "#d7191c",
                                       "#d7191c"))

# register stadia API key ----
register_stadiamaps(key = substr(read_file(file = "api_keys/stadia_api_key"), 1, 36))
#options(ggmap.file_drawer = "basemaps")
# dir.create(file_drawer(), recursive = TRUE, showWarnings = FALSE)
# saveRDS(list(), file_drawer("index.rds"))
#readRDS(file_drawer("index.rds"))
#file_drawer("index.rds")

# load census api key ----
census_api_key(key = substr(read_file(file = "api_keys/census_api_key"), 1, 40))

# load logo
logo <- image_read(path = "other/BFW_Logo_180_x_200_transparent_background.png")
school_symbol <- image_read_svg(path = "other/school_FILL0_wght400_GRAD0_opsz24.svg")

## ---- generate charts/maps ----
## set parameters of run

#county_focus <- str_to_upper(unique(WI_schools %>% pull(CTY_DIST)))
#county_focus <- c("DANE")
county_focus <- "Dane"

municipality_geom <- st_read("data/WI_Cities,_Towns_and_Villages_January_2024.geojson")
#municipality_focus <- c("Mcfarland")
#municipality_focus <- c("Monona", "Fitchburg")
municipality_focus <- municipality_geom %>% filter(CNTY_NAME == county_focus) %>% pull(MCD_NAME)

for(municipality in municipality_focus) {
  
  message(paste("***", municipality))
  options(ggmap.file_drawer = paste0("basemaps/municipalities/", municipality))
  dir.create(file_drawer(), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(), file_drawer("index.rds"))
  readRDS(file_drawer("index.rds"))
  file_drawer("index.rds")
  
  municipality_filtered <- municipality_geom %>% filter(CNTY_NAME == county_focus, MCD_NAME == municipality) %>% pull(geometry)
  
  # create bounding box from school, 5km away.
  bbox_poly <- st_transform(st_buffer(municipality_filtered, 1000), crs = 4326)
  bbox <- st_bbox(bbox_poly)
  bbox <- c(left = as.double(bbox[1]), 
            bottom = as.double(bbox[2]), 
            right = as.double(bbox[3]), 
            top = as.double(bbox[4]))
  
  #get basemap
  basemap <- get_stadiamap(bbox = bbox, zoom = 15, maptype = "stamen_toner_lite")
  
  # generate map
  ggmap(basemap) +
    labs(title = paste0(
      #        "Crashes between cars and youth (<18) pedestrians/bicyclists near ",
      "Crashes between cars and all pedestrians/bicyclists in/near ",
      municipality),
      subtitle = paste0(min(year(TOPS_data$date), na.rm = TRUE), 
                        " - ", 
                        max(year(TOPS_data$date), na.rm = TRUE)),
      caption = paste0("crash data from UW TOPS lab - retrieved ",
                       strftime(retrieve_date, format = "%m/%Y"),
                       " per direction of the WisDOT Bureau of Transportation Safety",
                       "\nbasemap from StadiaMaps and OpenStreetMap Contributers"),
      x = NULL,
      y = NULL) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.caption = element_text(color = "grey")) +
    
    ## add bike lts
    geom_sf(data = bike_lts[[county]] %>% st_intersection(bbox_poly),
            inherit.aes = FALSE,
            aes(color = lts)) +
    scale_color_manual(values = bike_lts_scale$color, name = "Bike Level of Traffic Stress") +
    
    # add crash locations
    new_scale_fill() +
    geom_point(data = TOPS_data %>%
                 filter(ROLE1 %in% c("BIKE", "PED") 
                        #                          & age1 < 18 
                        | ROLE2 %in% c("BIKE", "PED") 
                        #                          & age2 < 18
                 ) %>%
                 filter(longitude >= as.double(bbox[1]),
                        latitude >= as.double(bbox[2]),
                        longitude <= as.double(bbox[3]),
                        latitude <= as.double(bbox[4])),
               aes(x = longitude,
                   y = latitude,
                   fill = ped_inj_name),
               shape = 23,
               size = 3) +
    scale_fill_manual(values = injury_severity$color, name = "Crash Severity") +
    geom_sf(data = municipality_filtered,
            inherit.aes = FALSE,
            color = 'black',
            fill = NA,
            linewidth = 1) +
    annotation_raster(logo,
                      # Position adjustments here using plot_box$max/min/range
                      ymin = bbox['top'] - (bbox['top']-bbox['bottom']) * 0.16,
                      ymax = bbox['top'],
                      xmin = bbox['right'] + (bbox['right']-bbox['left']) * 0.05,
                      xmax = bbox['right'] + (bbox['right']-bbox['left']) * 0.20) +
    coord_sf(clip = "off")
  
  ggsave(file = paste0("figures/municipalities/",
                       municipality,
                       ".pdf"),
         #title = paste0(municipality, " Youth Pedestrian/Bike crashes"),
         title = paste0(municipality, " All Pedestrian/Bike crashes"),
         device = pdf,
         height = 8.5,
         width = 11,
         units = "in",
         create.dir = TRUE)
}
