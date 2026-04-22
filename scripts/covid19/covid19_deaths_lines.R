# COVID-19 Deaths per Million - Racing Line Chart (YouTube Style)
# Based on original scripts by Juan Ochoteco Asensio

# 1. Setup and Libraries
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "gganimate", "tidyr", "zoo", "scales", "av", "gifski", "httr", "RColorBrewer"))

# 2. Data Fetching
print("Fetching data from Our World in Data...")
owid_url <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"
data <- read.csv(owid_url, stringsAsFactors = FALSE)

# 3. Data Preprocessing & Interpolation
print("Processing and interpolating data...")
if (!"location" %in% colnames(data) & "country" %in% colnames(data)) {
  data <- data %>% rename(location = country)
}

# 1. Initial Filtering
df_raw <- data %>%
  filter(!is.na(iso_code) & !grepl("^OWID", iso_code)) %>%
  filter(!is.na(total_deaths_per_million)) %>%
  mutate(date = as.Date(date)) %>%
  filter(population >= 1000000)

# 2. Identify Top countries and Interpolate daily
# We interpolate for all countries first to determine daily ranks
all_dates <- seq(min(df_raw$date), max(df_raw$date), by="day")
num_days <- length(all_dates)
print(paste("Total days to animate:", num_days))

df_interpolated <- df_raw %>%
  group_by(country = location) %>%
  complete(date = all_dates) %>%
  mutate(total_deaths_per_million = na.approx(total_deaths_per_million, na.rm = FALSE, rule = 2)) %>%
  ungroup()

# 3. Calculate Daily Ranks
print("Calculating daily ranks...")
df_ranked <- df_interpolated %>%
  group_by(date) %>%
  mutate(rank = rank(-total_deaths_per_million, ties.method = "first")) %>%
  ungroup()

# 4. Create Expanded Dataset for Animation
# For each 'frame', we only want the history of the 10 countries that are Top 10 AT THAT MOMENT.
# This forces the axes (view_follow) to only consider the current leaders.
print("Creating expanded dataset for dynamic axes (this may take a moment)...")

# We'll render every 7 days to keep it smooth but performant for such a long period
frame_dates <- all_dates[seq(1, length(all_dates), by = 7)]

# For each frame date, identify the Top 10 and grab their full history up to that date
df_animation <- lapply(frame_dates, function(d) {
  top_10_countries <- df_ranked %>% 
    filter(date == d, rank <= 10) %>% 
    pull(country)
  
  df_ranked %>%
    filter(country %in% top_10_countries, date <= d) %>%
    mutate(frame_date = d)
}) %>% bind_rows()

# 5. Static Plot Template
all_countries <- unique(df_animation$country)
country_colors <- colorRampPalette(brewer.pal(12, "Paired"))(length(all_countries))
names(country_colors) <- all_countries

staticplot <- ggplot(df_animation, aes(x = date, y = total_deaths_per_million, color = country, group = country)) +
  # Draw the full line history for each frame
  geom_line(linewidth = 1.5, alpha = 0.8) +
  # ONLY draw the point and text at the 'head' of the line (where date == frame_date)
  geom_point(data = df_animation %>% filter(date == frame_date), size = 5) +
  geom_text(data = df_animation %>% filter(date == frame_date),
            aes(label = paste0(" ", country, " (", comma(round(total_deaths_per_million)), ")")),
            hjust = 0, vjust = 0.5, size = 6, fontface = "bold") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y") + 
  scale_color_manual(values = country_colors) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(linewidth = 0.1, color = "grey30"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white", size = 12),
    axis.title = element_text(color = "white", size = 15, face = "bold"),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold", color = "white", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 20, hjust = 0.5, face = "italic", color = "grey70"),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic", color = "grey50"),
    legend.position = "none",
    plot.margin = margin(1, 12, 1, 1, "cm") # Even more right margin for the labels
  )

# 6. Animation Configuration
# transition_manual handles the pre-filtered frames
anim <- staticplot + 
  transition_manual(frame_date) +
  view_follow(fixed_x = FALSE, fixed_y = FALSE) +
  labs(title = 'COVID-19 Total Deaths: {current_frame}',  
       subtitle  = "Top 10 Most Impacted Countries at Each Moment (Deaths per Million)",
       y = "Cumulative Deaths per Million",
       x = "Date",
       caption  = "Data Source: Our World in Data | Visualized with R & gganimate")

# 7. Rendering
print(paste("Rendering", length(frame_dates), "frames..."))

total_frames <- length(frame_dates)
ffmpeg_available <- Sys.which("ffmpeg") != ""

if (ffmpeg_available) {
  print("Using ffmpeg_renderer for MP4 output...")
  for_mp4 <- animate(anim, 
          nframes = total_frames, 
          fps = 20, 
          width = 1280, 
          height = 720, 
          renderer = ffmpeg_renderer(format = "mp4", options = list(pix_fmt = "yuv420p")),
          detail = 1)
  anim_save("output/covid19_deaths_line_race.mp4", for_mp4)
} else {
  print("ffmpeg not found in PATH. Using gifski_renderer...")
  for_gif <- animate(anim, 
          nframes = total_frames, 
          fps = 15, 
          width = 800, 
          height = 600, 
          renderer = gifski_renderer("output/covid19_deaths_line_race.gif"),
          detail = 1)
}
print("Animation saved successfully.")
