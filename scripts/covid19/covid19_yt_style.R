# Improved COVID-19 Bar Chart Race (YouTube Style)
# Based on original scripts by Juan Ochoteco Asensio

# 1. Setup and Libraries
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "gganimate", "tidyr", "zoo", "scales", "av", "gifski", "httr", "RColorBrewer"))

# 2. Data Fetching (Using Our World in Data Catalog for reliable access)
print("Fetching data from Our World in Data...")
owid_url <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"
data <- read.csv(owid_url, stringsAsFactors = FALSE)

# 3. Data Preprocessing & Interpolation
print("Processing and interpolating data...")
# Note: catalog.ourworldindata.org uses 'country' instead of 'location'
if (!"location" %in% colnames(data) & "country" %in% colnames(data)) {
  data <- data %>% rename(location = country)
}

# 1. Initial Filtering
df_raw <- data %>%
  filter(!is.na(location) & !is.na(total_cases_per_million)) %>%
  # Robust filtering: actual countries have 3-letter ISO codes. 
  # Aggregates like "World", "Europe", "High Income" have OWID_ prefixes or are missing.
  filter(!is.na(iso_code) & !grepl("^OWID", iso_code)) %>%
  mutate(date = as.Date(date))

# Filter by population (now using the existing population column)
df_raw <- df_raw %>% filter(population >= 1000000)

# 2. Interpolate to ensure every country has a value for EVERY day in the range
all_dates <- seq(min(df_raw$date), max(df_raw$date), by="day")
num_days <- length(all_dates)
print(paste("Total days to animate:", num_days))

df_interpolated <- df_raw %>%
  group_by(country = location) %>%
  complete(date = all_dates) %>%
  mutate(total_cases_per_million = na.approx(total_cases_per_million, na.rm = FALSE, rule = 2)) %>%
  fill(total_cases, .direction = "downup") %>%
  ungroup()

# 4. Calculate Ranks
df_processed <- df_interpolated %>%
  group_by(date) %>%
  mutate(rank = rank(-total_cases_per_million, ties.method = "first") * 1) %>%
  ungroup()

# To make the animation "pop", we only show Top 10 at each moment
df_formatted <- df_processed %>%
  group_by(date) %>%
  filter(rank <= 10) %>%
  mutate(Value_lbl = paste0(round(total_cases_per_million / 1000, 1), "k")) %>%
  ungroup()

# 5. Static Plot Template (YouTube-y Style)
# - Dark Theme
# - Large dynamic text
# - Professional colors
staticplot <- ggplot(df_formatted, aes(rank, group = country, fill = country, color = country)) +
  geom_tile(aes(y = total_cases_per_million/2,
                height = total_cases_per_million,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size = 7, fontface = "bold", color = "white") +
  geom_text(aes(y = total_cases_per_million, label = Value_lbl, hjust = 0), size = 8, fontface = "bold", color = "white") +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = comma) +
  scale_x_reverse() +
  guides(color = "none", fill = "none") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey30"),
    panel.grid.minor.x = element_line(linewidth = 0.1, color = "grey30"),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold", color = "white", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic", color = "grey70"),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic", color = "grey50"),
    plot.margin = margin(1, 6, 1, 6, "cm") # Increased right margin to 6cm
  )

# 6. Animation Configuration
anim <- staticplot + 
  transition_time(date) +
  view_follow(fixed_x = TRUE) +
  labs(title = 'COVID-19 Cumulative Cases: {frame_time}',  
       subtitle  = "Top 10 Countries (Cases per Million)",
       caption  = "Data Source: Our World in Data | Visualized with R & gganimate")

# 7. Rendering
print("Rendering animation (this might take a while)...")

# Check if ffmpeg is available
ffmpeg_available <- Sys.which("ffmpeg") != ""

# To make it slow, we use many frames. 
# 1 frame per day is a good baseline.
# For 1500 days:
# MP4 (25 fps): 60 seconds
# GIF (10 fps): 150 seconds (Very slow and smooth)
total_frames <- num_days 

if (ffmpeg_available) {
  print("Using ffmpeg_renderer for MP4 output...")
  for_mp4 <- animate(anim, 
          nframes = total_frames, 
          fps = 25, 
          width = 1280, 
          height = 720, 
          renderer = ffmpeg_renderer(format = "mp4", options = list(pix_fmt = "yuv420p")),
          detail = 5)
  anim_save("output/covid19_youtube_bar_chart_race.mp4", for_mp4)
  print("Animation saved to output/covid19_youtube_bar_chart_race.mp4")
} else {
  print("ffmpeg not found in PATH. Falling back to gifski_renderer for GIF output...")
  # GIF is heavy. We'll use 1 frame per day but keep an eye on size.
  # If the user wants it REALLY slow, we keep nframes = num_days.
  for_gif <- animate(anim, 
          nframes = num_days, 
          fps = 10, 
          width = 800, 
          height = 600, 
          renderer = gifski_renderer("output/covid19_youtube_bar_chart_race.gif"),
          detail = 1)
  print("Animation saved to output/covid19_youtube_bar_chart_race.gif")
}
