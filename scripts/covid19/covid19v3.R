negToZero = function(x) {
  x[x < 0] = NA
  return(x)
}

# Load the package required to read JSON files.
source('scripts/utils/functions.R')
forceLibrary(c("rjson", 'dplyr', 'ggplot2'))

library(utils)
library(httr)
library(tibble)
library(ggplot2)
library(gganimate)
#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_archive/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- tempfile(fileext = ".csv")))




#read the Dataset sheet into "R". The dataset will be called "data".
data <- read.csv(tf)


world_data = data %>%  arrange(country, year_week)

cases_world_data = world_data %>% 
  filter(indicator == 'cases',
         population >= 7000000,
         # grepl('2020', year_week),
         !is.na(country_code)) %>% 
  mutate(cases_per_population = weekly_count / population)

df <- cases_world_data %>% 
  group_by(country_code) %>%
  # summarise(value = sum(cases_per_population)) %>%
  mutate(csum = cumsum(cases_per_population))
df


covid_cases_formatted <- df %>%
  # select(-note) %>% 
  # na.omit() %>% 
  group_by(year_week) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-csum),
         csum_rel = csum/csum[rank==1],
         Value_lbl = paste0("",round(csum*1e6))) %>%
  group_by(country) %>% 
  filter(rank <=10) %>%
  ungroup() %>% 
  as.data.frame()


staticplot = ggplot(covid_cases_formatted, aes(rank, group = country, 
                                       fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = csum/2,
                height = csum, 
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y= csum, label = Value_lbl, hjust=0), show.legend = T) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")

        )

anim = staticplot +
  transition_states(year_week, transition_length = 4, state_length = 2, wrap = FALSE) +
  view_follow(fixed_x = TRUE) +
  enter_grow() +
  exit_shrink() +
  shadow_trail(max_frames = 12, alpha = 0.15, size = 0.3) +
  ease_aes("sine-in-out") +
  labs(title = 'Total COVID19 Cases : {closest_state}',
       subtitle  =  "Top 10 Countries",
       caption  = "Cases x 10^6 / Population | Data Source: ECDC")


animate(plot = anim, duration = 100, fps = 10)

# anim

library(gifski)
  
animate(anim, 200, duration = 100, fps = 10, 
        renderer = gifski_renderer("gganim2.gif"))

library(av)

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4
