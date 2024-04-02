library(tidyverse)

enrollment_data <- read_csv(file = "/home/ben/Documents/Data analysis/map/data/Enrollement_2022-2023/enrollment_by_gradelevel_certified_2022-23.csv")
enrollment_data_wide <- 
  enrollment_data %>% 
  mutate(district_school = paste0(DISTRICT_CODE, SCHOOL_CODE),
         variable_name = paste0(GROUP_BY, "__", GROUP_BY_VALUE)) %>%
  mutate(variable_name = str_replace_all(variable_name, "[ ]", "_")) %>%
  pivot_wider(id_cols = c(district_school, GRADE_LEVEL, SCHOOL_NAME, DISTRICT_NAME), names_from = variable_name, values_from = PERCENT_OF_GROUP)

write_csv(enrollment_data_wide, file = "/home/ben/Documents/Data analysis/map/data/Enrollement_2022-2023/enrollment_by_gradelevel_certified_2022-23_wide.csv")


# school comparison
schools <- data.frame(Name = c("East High", "West High", "Memorial High", "LaFollette High"),
                      district_school = c(32690150,32690840,32690360,32690420))

enrollment_data_wide %>% 
  filter(district_school %in% schools$district_school) %>%
  group_by(district_school)
  summarise(mean_econ_disadv = mean(as.double('Economic_Status__Econ_Disadv'), na.rm = TRUE))
  ggplot() + 
  geom_col(aes(x = SCHOOL_NAME,
               y = mean_econ_disadv))
