# COVID-19 Cases vs Deaths - Bubble Race (Flight Path)
# Visualizing mortality rates and infection spread over time

# 1. Setup and Libraries
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "gganimate", "tidyr", "zoo", "scales", "RColorBrewer"))

# 2. Data Fetching
print("Fetching data from Our World in Data...")
owid_url <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"
data <- read.csv(owid_url, stringsAsFactors = FALSE)

# 3. Data Preprocessing
print("Processing data...")
if (!"location" %in% colnames(data) & "country" %in% colnames(data)) {
  data <- data %>% rename(location = country)
}

# Filter for major countries
df_raw <- data %>%
  filter(!is.na(iso_code) & !grepl("^OWID", iso_code)) %>%
  filter(!is.na(total_cases_per_million) & !is.na(total_deaths_per_million)) %>%
  mutate(date = as.Date(date)) %>%
  filter(population >= 10000000) # Only countries with >10M pop

# Select top 12 countries by final death toll to keep the plot readable
top_countries <- df_raw %>%
  group_by(location) %>%
  summarise(final_deaths = max(total_deaths_per_million, na.rm = TRUE)) %>%
  top_n(12, final_deaths) %>%
  pull(location)

df_filtered <- df_raw %>% 
  filter(location %in% top_countries) %>%
  group_by(location) %>%
  complete(date = seq(min(df_raw$date), max(df_raw$date), by="day")) %>%
  mutate(
    total_cases_per_million = na.approx(total_cases_per_million, na.rm = FALSE, rule = 2),
    total_deaths_per_million = na.approx(total_deaths_per_million, na.rm = FALSE, rule = 2)
  ) %>%
  ungroup()

# 4. Static Bubble Plot
staticplot <- ggplot(df_filtered, aes(x = total_cases_per_million, y = total_deaths_per_million, color = location)) +
  # Draw the 'tail' (history) of each country
  geom_path(aes(group = location), alpha = 0.3, linewidth = 1) +
  # Draw the current position
  geom_point(aes(size = population), alpha = 0.8) +
  geom_text(aes(label = location), hjust = -0.2, vjust = -0.5, size = 5, fontface = "bold") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(range = c(5, 20), labels = comma) +
  scale_color_brewer(palette = "Paired") +
  labs(title = 'COVID-19 Flight Path: {frame_along}',
       subtitle = 'Total Cases (X) vs Total Deaths (Y) per Million',
       x = 'Total Cases per Million',
       y = 'Total Deaths per Million',
       size = 'Population',
       color = 'Country',
       caption = 'Data Source: Our World in Data | Visualized with R & gganimate') +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(linewidth = 0.1, color = "grey30"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white", size = 12),
    axis.title = element_text(color = "white", size = 15, face = "bold"),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic", color = "grey70"),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic", color = "grey50"),
    legend.position = "none"
  )

# 5. Animation Configuration
anim <- staticplot + 
  transition_reveal(date) +
  view_follow()

# 6. Rendering
print("Rendering Bubble Race animation...")
num_days <- as.numeric(max(df_filtered$date) - min(df_filtered$date))
total_frames <- round(num_days / 7) # Render every 7 days for speed

ffmpeg_available <- Sys.which("ffmpeg") != ""

if (ffmpeg_available) {
  for_mp4 <- animate(anim, nframes = total_frames, fps = 20, width = 1280, height = 720, 
                    renderer = ffmpeg_renderer(format = "mp4"))
  anim_save("output/covid19_bubble_race.mp4", for_mp4)
} else {
  for_gif <- animate(anim, nframes = total_frames, fps = 15, width = 800, height = 600, 
                    renderer = gifski_renderer("output/covid19_bubble_race.gif"))
}

print("Bubble Race animation saved successfully.")
