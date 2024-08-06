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

injury_severity_pal <- colorFactor(palette = injury_severity$color, levels = injury_severity$InjSevName)

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

# ---- add additional data
## add school enrollment data
enrollment <- read_csv(file = "data/Schools/Enrollement_2022-2023/enrollment_by_gradelevel_certified_2022-23.csv",
                       col_types = "ccccccccccccciid")
enrollment_wide <- 
  enrollment %>% 
  mutate(district_school = paste0(DISTRICT_CODE, SCHOOL_CODE),
         variable_name = paste0(GROUP_BY, "__", GROUP_BY_VALUE)) %>%
  mutate(variable_name = str_replace_all(variable_name, "[ ]", "_")) %>%
  pivot_wider(id_cols = c(district_school, GRADE_LEVEL, SCHOOL_NAME, DISTRICT_NAME, GRADE_GROUP, CHARTER_IND), names_from = variable_name, values_from = PERCENT_OF_GROUP) %>%
  group_by(district_school, SCHOOL_NAME, DISTRICT_NAME, GRADE_GROUP, CHARTER_IND) %>%
  summarise_at(vars("Disability__Autism":"Migrant_Status__[Data_Suppressed]"), mean, na.rm = TRUE)

district_info <- data.frame(name = c("Madison Metropolitan", "Milwaukee"),
                            code = c("3269","3619"),
                            walk_boundary_hs = c(1.5, 2),
                            walk_boundary_ms = c(1.5, 2),
                            walk_boundary_es = c(1.5, 1))

## load school locations
WI_schools <- st_read(dsn = "data/Schools/WI_schools.gpkg")

WI_schools <- left_join(WI_schools %>% mutate(district_school = paste0(SDID, SCH_CODE)), 
                        enrollment_wide, 
                        join_by(district_school))

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
#census_api_key(key = substr(read_file(file = "api_keys/census_api_key"), 1, 40))

# load logo
logo <- image_read(path = "other/BFW_Logo_180_x_200_transparent_background.png")
school_symbol <- image_read_svg(path = "other/school_FILL0_wght400_GRAD0_opsz24.svg")

## ---- generate charts/maps ----
## set parameters of run

#county_focus <- str_to_upper(unique(WI_schools %>% pull(CTY_DIST)))
#county_focus <- c("DANE")
county_focus <- c("DANE")

#school_type_focus <- unique(WI_schools %>% filter(CTY_DIST %in% str_to_title(county_focus)) %>% pull(SCHOOLTYPE))
school_type_focus <- c("Elementary School")

#district_focus <- unique(WI_schools %>%   filter(CTY_DIST %in% str_to_title(county_focus),  SCHOOLTYPE %in% school_type_focus, !is.na(DISTRICT_NAME)) %>%  pull(DISTRICT_NAME))
district_focus <- c("Madison Metropolitan")

school_number <- length(unique(WI_schools %>% filter(CTY_DIST %in% str_to_title(county_focus),  
                                              SCHOOLTYPE %in% school_type_focus, 
                                              DISTRICT_NAME %in% district_focus) %>%  
                          pull(district_school)))
## * generate county charts ----
for(county in county_focus) {
  message(county)
  TOPS_data %>% 
    filter(CNTYNAME %in% county) %>% 
    filter(ROLE1 %in% vuln_roles & age1 < 18 | ROLE2 %in% vuln_roles & age2 < 18) %>% 
    group_by(year) %>% summarise(count = n_distinct(DOCTNMBR)) %>%
    ggplot() +
    geom_col(aes(x = year,
                 y = count),
             fill = "darkred") +
    scale_y_continuous(expand = expansion(mult = c(0,0.07))) +
    labs(title = paste0("Pedestrians/bicyclists under 18 years old hit by cars in ",
                        str_to_title(county), 
                        " County"),
         x = "Year",
         y = "Number of crashes",
         caption = paste0("crash data from UW TOPS lab - retrieved ",
                          strftime(retrieve_date, format = "%m/%Y"),
                          " per direction of the WisDOT Bureau of Transportation Safety",
                          "\nbasemap from StadiaMaps and OpenStreetMap Contributers"))
  ggsave(file = paste0("figures/school_maps/Crash Maps/", 
                       str_to_title(county), 
                       " County/_",
                       str_to_title(county), 
                       " County_year.pdf"), 
         title = paste0(county, " County Youth Pedestrian/Bike crashes"),
         device = pdf,
         height = 8.5,
         width = 11,
         units = "in",
         create.dir = TRUE)
  
  # #  generate map for county
  # county_data <- WI_schools %>% filter(CTY_DIST %in% str_to_title(county))
  # bbox <- st_bbox(st_transform(st_buffer(county_data %>% pull(geom), dist = 4000), crs = 4326))
  # bbox <- c(left = as.double(bbox[1]), bottom = as.double(bbox[2]), right = as.double(bbox[3]), top = as.double(bbox[4]))
  # 
  # #get basemap
  # basemap <- get_stadiamap(bbox = bbox, zoom = 12, maptype = "stamen_toner_lite")
  # 
  # # generate map
  # ggmap(basemap) +
  #   labs(title = paste0("Crashes between cars and youth (under 18) pedestrians/bicyclists in ", 
  #                       str_to_title(county),
  #                       " County"),
  #        subtitle = paste0(min(year(TOPS_data$date), na.rm = TRUE), " - ", max(year(TOPS_data$date), na.rm = TRUE)),
  #        caption = "data from Wisconsin DOT, UW TOPS Laboratory, Wisconsin DPI, and OpenStreetMap",
  #        x = NULL,
  #        y = NULL) +
  #   theme(axis.text=element_blank(),
  #         axis.ticks=element_blank()) +
  #   
  #   # add crash heatmap
  #   # stat_density_2d(data = TOPS_data %>% 
  #   #                   filter(ROLE1 %in% c("BIKE", "PED") & age1 < 18 | ROLE2 %in% c("BIKE", "PED") & age2 < 18),
  #   #                 inherit.aes = FALSE,
  #   #                 geom = "polygon", 
  #   #                 aes(fill = after_stat(level),
  #   #                     x = longitude,
  #   #                     y = latitude),
  #   #                 alpha = 0.2,
  #   #                 color = NA,
  #   #                 na.rm = TRUE,
  #   #                 bins = 12,
  #   #                 n = 300) +
  #   # scale_fill_distiller(type = "div", palette = "YlOrRd", guide = "none", direction = 1) +
  #   
  #   # add crashes
  #   new_scale_color() +
  #   geom_point(data = TOPS_data %>% 
  #                filter(ROLE1 %in% c("BIKE", "PED") & age1 < 18 | ROLE2 %in% c("BIKE", "PED") & age2 < 18) %>%
  #                filter(longitude >= as.double(bbox[1]), 
  #                       latitude >= as.double(bbox[2]), 
  #                       longitude <= as.double(bbox[3]), 
  #                       latitude <= as.double(bbox[4])), 
  #              aes(x = longitude,
  #                  y = latitude,
  #                  color = InjSevName),
  #              shape = 18,
  #              size = 1) +
  #   scale_color_manual(values = injury_severity$color, name = "Crash Severity")
  #   
  #   # add school location
  #   # new_scale_color() +
  #   # geom_sf(data = st_transform(WI_schools, crs = 4326),
  #   #         inherit.aes = FALSE,
  #   #         aes(color = "school"),
  #   #         size = 2,
  #   #         shape = 0) +
  #   # scale_color_manual(values = "black", name = NULL)
  # 
  # ggsave(file = paste0("figures/school_maps/Crash Maps/", 
  #                                      str_to_title(county), " County/_", 
  #                                      str_to_title(county), " County.pdf"), 
  #        title = paste0(str_to_title(county), " County Youth Pedestrian/Bike crashes"),
  #        device = pdf,
  #        height = 8.5,
  #        width = 11,
  #        units = "in",
  #        create.dir = TRUE)
}

# * generate individual school maps ----
options(osrm.server = "http://127.0.0.1:5000/")
options(osrm.profile = "walk")

i <- 0
#districts_done <- read_csv(file = "other/districts_done.csv")

#district_focus <- district_focus[! district_focus %in% districts_done$district]

for(district in district_focus) {
  
  message(paste("***", district, "School District |"))
  options(ggmap.file_drawer = paste0("basemaps/districts/", district))
  dir.create(file_drawer(), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(), file_drawer("index.rds"))
  readRDS(file_drawer("index.rds"))
  file_drawer("index.rds")
  for(school in WI_schools %>%
      filter(DISTRICT_NAME %in% district,
             SCHOOLTYPE %in% school_type_focus,
             !st_is_empty(geom)) %>%
      pull(district_school)) {
    school_data <- WI_schools %>% filter(district_school == school)
    i <- i + 1
    message(paste(school_data %>% pull(SCHOOL_NAME), "-", district, "School District", "-", school_data %>% pull(CTY_DIST), "County |", i, "/", school_number, "|", round(i/school_number*100, 2), "%"))

    #find walk boundary distance for school
    if(length(which(district_info$name == district)) > 0) {
      ifelse((school_data %>% pull(SCHOOLTYPE)) %in% "High School",
             walk_boundary_mi <- district_info$walk_boundary_hs[district_info$name == district],
             ifelse((school_data %>% pull(SCHOOLTYPE)) %in% c("Junior High School", "Middle School"),
                    walk_boundary_mi <- district_info$walk_boundary_ms[district_info$name == district],
                    ifelse((school_data %>% pull(SCHOOLTYPE)) %in% c("Combined Elementary/Secondary School", "Elementary School"),
                           walk_boundary_mi <- district_info$walk_boundary_es[district_info$name == district],
                           walk_boundary <- 2)))
    } else {
      walk_boundary_mi <- 2
    }
    walk_boundary_m <- walk_boundary_mi * 1609
    
    walk_boundary_poly <- fill_holes(st_make_valid(osrmIsodistance(
      loc = st_transform(school_data %>% pull(geom), crs = 4326),
      breaks = c(walk_boundary_m),
      res = 80)
    ), units::set_units(1, km^2))
    
    # create bounding box from school, 5km away.
    bbox <- st_bbox(st_transform(st_buffer(school_data %>% pull(geom), dist = walk_boundary_m + 500), crs = 4326))
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
        "Crashes between cars and all pedestrians/bicyclists near ",
        school_data %>% pull(SCHOOL_NAME),
        " School"),
           subtitle = paste0(school_data %>% pull(DISTRICT_NAME),
                             " School District | ",
                             min(year(TOPS_data$date), na.rm = TRUE), 
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
      geom_sf(data = bike_lts[[county]],
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
      scale_fill_manual(values = injury_severity_pal(injury_severity$color), name = "Crash Severity") +
      # add walk boundary
      new_scale_color() +
      new_scale_fill() +
      geom_sf(data = walk_boundary_poly,
              inherit.aes = FALSE,
              aes(color = paste0(walk_boundary_mi, " mile walking boundary")),
              fill = NA,
              linewidth = 1) +
      scale_color_manual(values = "black", name = NULL) +
      # add school location
      # geom_sf(data = st_transform(school_data, crs = 4326), inherit.aes = FALSE) +
      annotation_raster(school_symbol,
                        # Position adjustments here using plot_box$max/min/range
                        ymin = as.double((st_transform(school_data, crs = 4326) %>% pull(geom))[[1]])[2] - 0.001,
                        ymax = as.double((st_transform(school_data, crs = 4326) %>% pull(geom))[[1]])[2] + 0.001,
                        xmin = as.double((st_transform(school_data, crs = 4326) %>% pull(geom))[[1]])[1] - 0.0015,
                        xmax = as.double((st_transform(school_data, crs = 4326) %>% pull(geom))[[1]])[1] + 0.0015) +
      geom_sf_label(data = st_transform(school_data, crs = 4326),
                    inherit.aes = FALSE,
                    mapping = aes(label = paste(SCHOOL_NAME, "School")),
                    nudge_y = 0.0015,
                    label.size = 0.04,
                    size = 2) +
      annotation_raster(logo,
                        # Position adjustments here using plot_box$max/min/range
                        ymin = bbox['top'] - (bbox['top']-bbox['bottom']) * 0.16,
                        ymax = bbox['top'],
                        xmin = bbox['right'] + (bbox['right']-bbox['left']) * 0.05,
                        xmax = bbox['right'] + (bbox['right']-bbox['left']) * 0.20) +
      coord_sf(clip = "off")
    
    ggsave(file = paste0("figures/school_maps/Crash Maps/",
                         str_to_title(school_data %>% pull(CTY_DIST)), 
                         " County/",
                         school_data %>% pull(DISTRICT_NAME), 
                         " School District/",
                         str_replace_all(school_data %>% pull(SCHOOLTYPE), "/","-"), 
                         "s/",
                         str_replace_all(school_data %>% pull(SCHOOL_NAME), "/", "-"), 
                         " School.pdf"),
           #title = paste0(school_data %>% pull(SCHOOL), " Youth Pedestrian/Bike crashes"),
           title = paste0(school_data %>% pull(SCHOOL), " All Pedestrian/Bike crashes"),
           device = pdf,
           height = 8.5,
           width = 11,
           units = "in",
           create.dir = TRUE)
  }
#  districts_done <- bind_rows(districts_done, data.frame(district = c(district)))
#  write_csv(districts_done, file = "other/districts_done.csv")
}

# double check that all schools have a map ----
double_check <- list(NULL)
for(school in WI_schools$district_school) {
  school_data <- WI_schools %>% filter(district_school %in% school)
  school_check <- data.frame(district_school = c(school),
                             exists = c(file.exists(paste0("figures/school_maps/Crash Maps/",
                                                           str_to_title(school_data %>% pull(CTY_DIST)), 
                                                           " County/",
                                                           school_data %>% pull(DISTRICT_NAME), 
                                                           " School District/",
                                                           str_replace_all(school_data %>% pull(SCHOOLTYPE), "/","-"), 
                                                           "s/",
                                                           str_replace_all(school_data %>% pull(SCHOOL_NAME), "/", "-"), 
                                                           #" School.pdf"))))
                                                           " School_all.pdf"))))
  double_check[[school]] <- school_check
}
double_check <- bind_rows(double_check)
unique(WI_schools %>% 
         filter(district_school %in% (double_check %>% 
                                        filter(exists == FALSE) %>% 
                                        pull(district_school)),
                !st_is_empty(geom)) %>% 
         pull(DISTRICT_NAME))
