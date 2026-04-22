# Government Response vs Growth Rates (Stringency Heatmap)
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "tidyr", "viridis"))

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

# Select top 20 countries by total cases to keep the heatmap legible
top_countries <- data %>%
  filter(!is.na(continent) & continent != "") %>%
  group_by(location) %>%
  summarize(total = max(total_cases, na.rm = TRUE)) %>%
  top_n(20, total) %>%
  pull(location)

df <- data %>%
  filter(location %in% top_countries) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(stringency_index))

plot <- ggplot(df, aes(x = date, y = reorder(location, stringency_index, na.rm = TRUE), fill = stringency_index)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  labs(title = "Government Stringency Index Over Time (Top 20 Countries)",
       x = "Date",
       y = "Country",
       fill = "Stringency Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print("Saving plot to output/stringency_heatmap.png")
ggsave("output/stringency_heatmap.png", plot, width = 12, height = 8, bg = "white")
print("Done.")
