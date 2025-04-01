library(tidyverse)
library(RColorBrewer)
library(tidycensus)
library(ggrepel)

load(file = "data/TOPS/TOPS_data.Rda")
load(file = "data/TOPS/vuln_roles.Rda")
load(file = "data/TOPS/retrieve_date.Rda")
load(file = "data/TOPS/injury_severity.Rda")

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
  filter(ped_inj %in% c("A", "K")) %>%
  group_by(CNTYNAME, year) %>%
  summarise(TotalCrashes = n()) %>%
  mutate(County = CNTYNAME) %>%
  left_join(county_populations, join_by("County")) %>%
  mutate(CrashesPerPopulation = TotalCrashes/value*100000) %>%
  filter(County %in% county_focus) %>%
  ggplot() +
  geom_line(aes(x = year.x,
                y = CrashesPerPopulation,
                color = str_to_title(CNTYNAME),
                group = CNTYNAME),
            size = 1) +
  geom_label_repel(data = TOPS_data %>%
                     filter(ped_inj %in% c("A", "K")) %>%
                     group_by(CNTYNAME, year) %>%
                     summarise(TotalCrashes = n()) %>%
                     mutate(County = CNTYNAME) %>%
                     left_join(county_populations, join_by("County")) %>%
                     mutate(CrashesPerPopulation = TotalCrashes/value*100000) %>%
                     filter(County %in% county_focus,
                            year.x == year(max(TOPS_data$date))),
                   aes(x = year.x,
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
       subtitle = paste0("Fatalities and Severe Injuries | ", year(min(TOPS_data$date)), "-", year(max(TOPS_data$date))),
       x = "Year",
       y = "Total crashes per year per 100,000 residents",
       color = "County",
       caption = paste0("crash data from UW TOPS lab - retrieved ",
                        strftime(retrieve_date, format = "%m/%Y"),
                        " per direction of the WisDOT Bureau of Transportation Safety")) +
  theme(plot.caption = element_text(color = "grey"))

ggsave(file = paste0("figures/crash_summaries/counties_year.png"),
       height = 5,
       width = 8,
       units = "in",
       dpi = 300)

TOPS_data %>%
  filter(County %in% county_focus) %>%
  group_by(County, vulnerable_role) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_col(aes(x = County,
               y = count,
               fill = vulnerable_role))


county_focus = "DANE"

TOPS_data %>%
  filter(CNTYNAME %in% county_focus) %>%
  filter(ped_inj %in% c("A", "K", "B")) %>%
  group_by(CNTYNAME, vulnerable_role, year) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_col(aes(x = year,
               y = count,
               fill = vulnerable_role),
           position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = paste0("Car crashes with vulnerable road users in ", str_to_title(county_focus), " County"),
       subtitle = paste0("Fatalities and injuries"),
       x = "Year",
       y = "Number of Crashes",
       fill = NULL,
       caption = paste0("crash data from UW TOPS lab - retrieved ",
                        strftime(retrieve_date, format = "%m/%Y"),
                        " per direction of the WisDOT Bureau of Transportation Safety")) +
  theme(plot.caption = element_text(color = "grey"))

ggsave(file = paste0("figures/crash_summaries/", county_focus, "_vuln_role_year.png"),
       height = 5,
       width = 8,
       units = "in",
       dpi = 300)
