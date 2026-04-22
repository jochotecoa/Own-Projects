# COVID-19 Mortality Waves - Ridgeline Plot (Joyplot)
# Visualizing the peaks and timing of waves across countries

# 1. Setup and Libraries
source('scripts/utils/functions.R')
# ggridges is required for ridgeline plots
forceLibrary(c("dplyr", "ggplot2", "gganimate", "tidyr", "zoo", "scales", "RColorBrewer", "ggridges"))

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
  mutate(date = as.Date(date)) %>%
  filter(population >= 10000000)

# Identify Top 12 countries by final death toll
top_countries <- df_raw %>%
  group_by(location) %>%
  summarise(final_deaths = max(total_deaths_per_million, na.rm = TRUE)) %>%
  top_n(12, final_deaths) %>%
  arrange(final_deaths) %>%
  pull(location)

# Prepare data with numeric Y-axis for ridgeline stability
df_filtered <- df_raw %>% 
  filter(location %in% top_countries) %>%
  mutate(location = factor(location, levels = top_countries)) %>%
  mutate(y_idx = as.numeric(location)) %>%
  group_by(location, y_idx) %>%
  complete(date = seq(min(df_raw$date), max(df_raw$date), by="day")) %>%
  mutate(new_deaths_smoothed_per_million = naToZero(new_deaths_smoothed_per_million)) %>%
  ungroup()

# 4. Create Expanded Dataset for Animation (Manual Reveal)
print("Creating expanded dataset...")
all_dates <- seq(min(df_filtered$date), max(df_filtered$date), by = "day")
frame_dates <- all_dates[seq(1, length(all_dates), by = 10)] # Every 10 days for speed

df_animation <- lapply(frame_dates, function(d) {
  df_filtered %>%
    filter(date <= d) %>%
    mutate(frame_date = d)
}) %>% bind_rows()

# 5. Static Ridgeline Plot
staticplot <- ggplot(df_animation, aes(x = date, y = y_idx, height = new_deaths_smoothed_per_million, fill = location, group = location)) +
  geom_ridgeline(scale = 0.05, alpha = 0.8, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlOrRd"))(12)) +
  scale_y_continuous(breaks = 1:12, labels = top_countries) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = range(df_filtered$date)) +
  labs(title = 'COVID-19 Mortality Waves: {current_frame}',
       subtitle = 'Daily Deaths per Million (7-day smoothed)',
       x = 'Timeline',
       y = NULL,
       caption = 'Data Source: Our World in Data | Visualized with R & gganimate') +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey30"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white", size = 12),
    axis.title = element_text(color = "white", size = 15, face = "bold"),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold", color = "white", margin = margin(b=10)),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic", color = "grey70", margin = margin(b=20)),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic", color = "grey50"),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# 6. Animation Configuration
anim <- staticplot + 
  transition_manual(frame_date)

# 7. Rendering
print(paste("Rendering", length(frame_dates), "frames..."))

ffmpeg_available <- Sys.which("ffmpeg") != ""
if (ffmpeg_available) {
  animate(anim, nframes = length(frame_dates), fps = 20, width = 1280, height = 720, 
          renderer = ffmpeg_renderer(format = "mp4")) %>%
    anim_save("output/covid19_waves_ridgeline.mp4", .)
} else {
  animate(anim, nframes = length(frame_dates), fps = 15, width = 800, height = 600, 
          renderer = gifski_renderer("output/covid19_waves_ridgeline.gif"))
}

print("Ridgeline animation saved successfully.")
