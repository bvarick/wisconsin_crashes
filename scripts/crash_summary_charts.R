library(tidyverse)
library(RColorBrewer)
library(tidycensus)
library(ggrepel)

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

# add population census data ----
census_api_key(key = substr(read_file(file = "api_keys/census_api_key"), 1, 40))
county_populations <- get_estimates(geography = "county", year = 2022, product = "population", state = "Wisconsin") %>%
  filter(variable == "POPESTIMATE") %>%
  mutate(County = str_to_upper(str_replace(NAME, " County, Wisconsin", "")))


## generate county charts ----
county_focus <- unique(TOPS_data %>%
                         group_by(CNTYNAME) %>%
                         summarise(TotalCrashes = n()) %>% 
                         slice_max(TotalCrashes, n = 8) %>%
                         pull(CNTYNAME))


TOPS_data %>%
  group_by(CNTYNAME, Year) %>%
  summarise(TotalCrashes = n()) %>%
  mutate(County = CNTYNAME) %>%
  left_join(county_populations, join_by("County")) %>%
  mutate(CrashesPerPopulation = TotalCrashes/value*100000) %>%
  filter(County %in% county_focus) %>%
  ggplot() +
  geom_line(aes(x = Year,
                y = CrashesPerPopulation,
                color = str_to_title(CNTYNAME),
                group = CNTYNAME),
            size = 1) +
  geom_label_repel(data = TOPS_data %>%
                     group_by(CNTYNAME, Year) %>%
                     summarise(TotalCrashes = n()) %>%
                     mutate(County = CNTYNAME) %>%
                     left_join(county_populations, join_by("County")) %>%
                     mutate(CrashesPerPopulation = TotalCrashes/value*100000) %>%
                     filter(County %in% county_focus,
                            Year == 2023),
                   aes(x = Year,
                       y = CrashesPerPopulation,
                       label = str_to_title(County),
                       fill = County),
                   size=3, 
                   min.segment.length=0, 
                   segment.size = 0.25,
                   nudge_x=0.5, 
                   direction="y") +
  scale_color_brewer(type = "qual", guide = NULL) +
  scale_fill_brewer(type = "qual", guide = NULL) +
  scale_x_discrete(expand = expansion(add = c(0.5,0.75))) +
  labs(title = "Drivers crashing into pedestrians & bicyclists per 100,000 residents",
       subtitle = "2017-2023",
       x = "Year",
       y = "Total crashes per year per 100,000 residents",
       color = "County",
       caption = paste0("crash data from UW TOPS lab - retrieved ",
                        strftime(retrieve_date, format = "%m/%Y"),
                        " per direction of the WisDOT Bureau of Transportation Safety",
                        "\nbasemap from StadiaMaps and OpenStreetMap Contributers")) +
  theme(plot.caption = element_text(color = "grey"))

ggsave(file = paste0("figures/crash_summaries/counties_year.pdf"),
       height = 8.5,
       width = 11,
       units = "in")
e