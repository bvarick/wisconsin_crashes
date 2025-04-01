library(tidyverse)
library(RColorBrewer)
library(stringr)
deaths <- read_csv("data/All_deaths_school_aged.csv")
ggplot(data = deaths %>% filter(Year != "Total", `Injury Mechanism & All Other Leading Causes` != "Total")) +
  geom_line(aes(x = `Year Code`,
               y = Deaths,
               color = str_wrap(`Injury Mechanism & All Other Leading Causes`, 30)),
            linewidth = 1, show.legend = FALSE) +
  geom_label_repel(data = deaths %>% filter(Year == "2023", `Injury Mechanism & All Other Leading Causes` != "Total"),
                   aes (x = `Year Code`,
                        y = Deaths,
                        color = str_wrap(`Injury Mechanism & All Other Leading Causes`, 30),
                        label = str_wrap(`Injury Mechanism & All Other Leading Causes`, 30)), 
                   show.legend = FALSE,
                   hjust = "outward",
                   direction = "y",
                   size = 2.5,
                   nudge_x = 0.75,
                   # direction = 'x',
                   xlim = c(2018, 2024)) +
  scale_color_brewer(palette = "Set3", guide = "none") +
  scale_x_continuous(expand = expansion(add = c(0.1, 0.1)), breaks = seq(2018, 2023, 1)) +
  labs(title = "Deaths of school-aged children in Wisconsin",
       subtitle = "data from CDC, 2018 - 2023",
       x = "Year",
       y = "Number of Deaths",
       color = "Cause of Death") +
    theme(plot.margin = unit(c(1,3,1,1), "cm")) +
    coord_cartesian(clip = "off", xlim = c(2018, 2023))
ggsave(file = "figures/all_deaths/all_deaths_causes.png",
       height = 5,
       width = 8,
       units = "in",
       dpi = 300)
