# Vaccination Impact Analysis
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "tidyr", "zoo"))

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
  filter(!is.na(people_fully_vaccinated_per_hundred) & !is.na(new_deaths_smoothed_per_million))

plot <- ggplot(df, aes(x = people_fully_vaccinated_per_hundred, y = new_deaths_smoothed_per_million, color = continent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_minimal() +
  labs(title = "Vaccination Rate vs New Deaths",
       x = "People Fully Vaccinated (per hundred)",
       y = "New Deaths Smoothed (per million)",
       color = "Continent")

print("Saving plot to output/vaccination_impact.png")
ggsave("output/vaccination_impact.png", plot, width = 10, height = 6, bg = "white")
print("Done.")
