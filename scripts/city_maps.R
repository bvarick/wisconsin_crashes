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
for (file in list.files(path = "data/TOPS", pattern = "crash-data-download")) {
  message(paste("importing data from file: ", file))
  year <- substr(file, 21, 24)
  csv_run <- read_csv(file = paste0("data/TOPS/",file), col_types = cols(.default = "c"))
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

# Injury Severy Index and Color -----
injury_severity <- data.frame(InjSevName = c("No apparent injury", "Possible Injury", "Suspected Minor Injury","Suspected Serious Injury","Fatality"), 
                              code = c("O", "C", "B", "A", "K"),
                              #                 color = c("#fafa6e", "#edc346", "#d88d2d", "#bd5721", "#9b1c1c"))
                              color = c("#fafa6e", "#edc346", "#d88d2d", "#d88d21", "#9b1c1c" ))

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

logo <- image_read(path = "other/BFW_Logo_180_x_200_transparent_background.png")


## set tile server info
# register stadia API key ----
register_stadiamaps(key = substr(read_file(file = "api_keys/stadia_api_key"), 1, 36))
options(ggmap.file_drawer = "basemaps")
# dir.create(file_drawer(), recursive = TRUE, showWarnings = FALSE)
# saveRDS(list(), file_drawer("index.rds"))
readRDS(file_drawer("index.rds"))
file_drawer("index.rds")

## set parameters -----

focus_muni <- c("MILWAUKEE", "MADISON")
focus_inj <- c("A", "K")
focus_role <- c("BIKE", "PED")
focus_years <- c("2023")

## generate maps for focus city

for(muni in focus_muni) {
  # create bounding box around crashes that happen in city.
  muni_data <- TOPS_data %>% filter(MUNINAME %in% muni)
  bbox <- c(left = min(muni_data$longitude, na.rm = TRUE), 
            bottom = min(muni_data$latitude, na.rm = TRUE), 
            right = max(muni_data$longitude, na.rm = TRUE), 
            top = max(muni_data$latitude, na.rm = TRUE))
  
  #get basemap
  basemap <- get_stadiamap(bbox = bbox, zoom = 12, maptype = "stamen_toner_lite")
  
  # generate map
  ggmap(basemap) +
    labs(title = paste0("Crashes between pedestrians/bicyclists in ", str_to_title(muni)),
         subtitle = paste0("that result in a severe injury or fatality | ",
                           focus_years),
         caption = "data from Wisconsin DOT, UW TOPS Laboratory, and OpenStreetMap",
         x = NULL,
         y = NULL) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank()) +
    
    ## add bike lts
    #geom_sf(data = bike_lts[[county]],
    #       inherit.aes = FALSE,
    #       aes(color = lts)) +
    #scale_color_manual(values = bike_lts_scale$color, name = "Bike Level of Traffic Stress") +
    
    # add crash locations
    new_scale_fill() +
    geom_point(data = TOPS_data %>%
                 filter(ROLE1 %in% focus_role 
                        & INJSVR1 %in% focus_inj
                        #   & age1 < 18 
                        | ROLE2 %in% focus_role
                        & INJSVR2 %in% focus_inj
                        #  & age2 < 18
                 ) %>%
                 filter(longitude >= as.double(bbox[1]),
                        latitude >= as.double(bbox[2]),
                        longitude <= as.double(bbox[3]),
                        latitude <= as.double(bbox[4])) %>%
                 filter(year %in% focus_years),
               aes(x = longitude,
                   y = latitude,
                   fill = ped_inj_name),
               shape = 21,
               size = 2) +
    scale_fill_manual(values = injury_severity %>% filter(code %in% focus_inj) %>% pull(color), name = "Crash Severity") +
    annotation_raster(logo,
                      # Position adjustments here using plot_box$max/min/range
                      ymin = bbox['top'] - 0.25 * 0.16,
                      ymax = bbox['top'],
                      xmin = bbox['right'] + 0.25 * 0.05,
                      xmax = bbox['right'] + 0.25 * 0.20) +
    coord_sf(clip = "off")
  
  ggsave(file = paste0("figures/city_maps/",
                       str_to_title(muni),
                       ".pdf"),
         
         title = paste0(str_to_title(muni), " Pedestrian/Bike crashes"),
         device = pdf,
         height = 8.5,
         width = 11,
         units = "in",
         create.dir = TRUE)
}
