library(tidyverse)

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

# Race names
race <- data.frame(race_name = c("Asian", "Black", "Indian","Hispanic","White"), 
                   code = c("A", "B", "I", "H", "W"))

TOPS_data <- left_join(TOPS_data, race %>% select(race_name, code), join_by(RACE1 == code)) %>% rename(race_name1 = race_name)
TOPS_data <- left_join(TOPS_data, race %>% select(race_name, code), join_by(RACE2 == code)) %>% rename(race_name2 = race_name)



## set parameters ----
county_focus <- c("MILWAUKEE")
municipality_focus <- c("MILWAUKEE")

## build data summaries for city ----
data_summary <- list(NULL)
# crashes by year that resulted in a pedestrian fatality or severe injury
data_summary[["crash_by_year"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
         ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA))) %>%
  group_by(MUNINAME, year, ped_type, ped_inj) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by race of pedestrian/bicyclist for focus year
data_summary[["crash_by_race"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         ped_race = ifelse(ROLE1 %in% c("BIKE", "PED"), race_name1, ifelse(ROLE2 %in% c("BIKE", "PED"), race_name2, NA))) %>%
  group_by(MUNINAME, ped_type, ped_inj, ped_race) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by race of driver that resulted in a pedestrian fatality or severe injury
data_summary[["crash_by_driver_race"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         driver_race = ifelse(ROLE1 %in% c("DR"), race_name1, ifelse(ROLE2 %in% c("DR"), race_name2, NA))) %>%
  group_by(MUNINAME, year, ped_type, ped_inj, driver_race) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by age of pedestrian/bicyclist
data_summary[["crash_by_age"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         ped_age = ifelse(ROLE1 %in% c("BIKE", "PED"), age1, ifelse(ROLE2 %in% c("BIKE", "PED"), age2, NA))) %>%
  group_by(MUNINAME, year, ped_type, ped_inj, ped_age) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by age of driver that resulted in a severe injury or fatality of a pedestrian/bicyclist
data_summary[["crash_by_driver_age"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         driver_age = ifelse(ROLE1 %in% c("DR"), age1, ifelse(ROLE2 %in% c("BIKE", "PED"), age2, NA))) %>%
  group_by(MUNINAME, year, ped_type, ped_inj, driver_age) %>%
  summarise(count = n_distinct(DOCTNMBR))


# crashes by sex of pedestrian/bicyclist
data_summary[["crash_by_sex"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         ped_sex = ifelse(ROLE1 %in% c("BIKE", "PED"), SEX1, ifelse(ROLE2 %in% c("BIKE", "PED"), SEX1, NA))) %>%
  group_by(MUNINAME, year, ped_type, ped_inj, ped_sex) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by sex of driver that resulted in a severe injury or fatality of a pedestrian/bicyclist
data_summary[["crash_by_driver_sex"]] <- TOPS_data %>%
  filter(MUNINAME %in% municipality_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         driver_sex = ifelse(ROLE1 %in% c("DR"), SEX1, ifelse(ROLE2 %in% c("BIKE", "PED"), SEX1, NA))) %>%
  group_by(MUNINAME, year, ped_type, ped_inj, driver_sex) %>%
  summarise(count = n_distinct(DOCTNMBR))

## export csv files for city ----
for(table_name in as.vector(names(data_summary[-1]))) {
  write_csv(data_summary[[table_name]], file = paste0("data_summaries/city/",table_name, ".csv"))
}


## build data summaries for county ----
data_summary <- list(NULL)
# crashes by year that resulted in a pedestrian fatality or severe injury
data_summary[["crash_by_year"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA))) %>%
  group_by(CNTYNAME, year, ped_type, ped_inj) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by race of pedestrian/bicyclist for focus year
data_summary[["crash_by_race"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         ped_race = ifelse(ROLE1 %in% c("BIKE", "PED"), race_name1, ifelse(ROLE2 %in% c("BIKE", "PED"), race_name2, NA))) %>%
  group_by(CNTYNAME, ped_type, ped_inj, ped_race) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by race of driver that resulted in a pedestrian fatality or severe injury
data_summary[["crash_by_driver_race"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         driver_race = ifelse(ROLE1 %in% c("DR"), race_name1, ifelse(ROLE2 %in% c("DR"), race_name2, NA))) %>%
  group_by(CNTYNAME, year, ped_type, ped_inj, driver_race) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by age of pedestrian/bicyclist
data_summary[["crash_by_age"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         ped_age = ifelse(ROLE1 %in% c("BIKE", "PED"), age1, ifelse(ROLE2 %in% c("BIKE", "PED"), age2, NA))) %>%
  group_by(CNTYNAME, year, ped_type, ped_inj, ped_age) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by age of driver that resulted in a severe injury or fatality of a pedestrian/bicyclist
data_summary[["crash_by_driver_age"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         driver_age = ifelse(ROLE1 %in% c("DR"), age1, ifelse(ROLE2 %in% c("BIKE", "PED"), age2, NA))) %>%
  group_by(CNTYNAME, year, ped_type, ped_inj, driver_age) %>%
  summarise(count = n_distinct(DOCTNMBR))


# crashes by sex of pedestrian/bicyclist
data_summary[["crash_by_sex"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         ped_sex = ifelse(ROLE1 %in% c("BIKE", "PED"), SEX1, ifelse(ROLE2 %in% c("BIKE", "PED"), SEX1, NA))) %>%
  group_by(CNTYNAME, year, ped_type, ped_inj, ped_sex) %>%
  summarise(count = n_distinct(DOCTNMBR))

# crashes by sex of driver that resulted in a severe injury or fatality of a pedestrian/bicyclist
data_summary[["crash_by_driver_sex"]] <- TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ROLE1 %in% c("BIKE", "PED") 
         & INJSVR1 %in% c("A", "K") 
         | ROLE2 %in% c("BIKE", "PED") 
         & INJSVR2 %in% c("A", "K")
  ) %>%
  mutate(ped_type = ifelse(ROLE1 %in% c("BIKE", "PED"), ROLE1, ifelse(ROLE2 %in% c("BIKE", "PED"), ROLE2, NA)),
         ped_inj = ifelse(ROLE1 %in% c("BIKE", "PED"), as.character(InjSevName1), ifelse(ROLE2 %in% c("BIKE", "PED"), as.character(InjSevName2), NA)),
         driver_sex = ifelse(ROLE1 %in% c("DR"), SEX1, ifelse(ROLE2 %in% c("BIKE", "PED"), SEX1, NA))) %>%
  group_by(CNTYNAME, year, ped_type, ped_inj, driver_sex) %>%
  summarise(count = n_distinct(DOCTNMBR))

## export csv files for county ----
for(table_name in as.vector(names(data_summary[-1]))) {
  write_csv(data_summary[[table_name]], file = paste0("data_summaries/county/",table_name, ".csv"))
}
