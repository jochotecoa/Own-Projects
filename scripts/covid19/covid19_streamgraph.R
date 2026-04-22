# Streamgraph of COVID-19 Cases by Continent
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "zoo"))

# Attempt to load ggstream, but don't force it
has_ggstream <- suppressWarnings(require("ggstream", quietly = TRUE))

print("Fetching full OWID data...")
owid_url <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"
data <- read.csv(owid_url, stringsAsFactors = FALSE)

# Handle column name differences in the new catalog
if (!"location" %in% colnames(data) & "country" %in% colnames(data)) {
  data <- data %>% rename(location = country)
}
if (!"iso_code" %in% colnames(data) & "code" %in% colnames(data)) {
  data <- data %>% rename(iso_code = code)
}

df <- data %>%
  filter(!is.na(continent) & continent != "") %>%
  mutate(date = as.Date(date)) %>%
  group_by(continent, date) %>%
  summarize(daily_cases = sum(new_cases, na.rm = TRUE), .groups = 'drop') %>%
  arrange(date) %>%
  group_by(continent) %>%
  mutate(smoothed_cases = rollmean(daily_cases, k = 14, fill = NA, align = "right")) %>%
  filter(!is.na(smoothed_cases) & smoothed_cases >= 0) %>%
  ungroup()

plot <- ggplot(df, aes(x = date, y = smoothed_cases, fill = continent))

if (has_ggstream) {
  plot <- plot + geom_stream(type = "proportional")
} else {
  print("ggstream not available, falling back to geom_area")
  plot <- plot + geom_area(position = "fill")
}

plot <- plot +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Proportion of New COVID-19 Cases by Continent Over Time",
       subtitle = if(!has_ggstream) "Note: Using geom_area as fallback for ggstream" else NULL,
       x = "Date",
       y = "Proportion of Cases",
       fill = "Continent")

print("Saving plot to output/cases_streamgraph.png")
ggsave("output/cases_streamgraph.png", plot, width = 12, height = 7, bg = "white")
print("Done.")
