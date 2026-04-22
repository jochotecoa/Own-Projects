# Excess Mortality vs Confirmed COVID-19 Deaths
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "ggplot2", "tidyr"))

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

# Filter for countries with excess mortality data
df <- data %>%
  filter(!is.na(continent) & continent != "") %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(excess_mortality_cumulative_absolute) & !is.na(total_deaths)) %>%
  group_by(location) %>%
  filter(date == max(date)) %>% # Get latest available data point per country
  ungroup() %>%
  top_n(15, excess_mortality_cumulative_absolute) %>%
  select(location, total_deaths, excess_mortality_cumulative_absolute) %>%
  pivot_longer(cols = c("total_deaths", "excess_mortality_cumulative_absolute"), names_to = "metric", values_to = "count")

plot <- ggplot(df, aes(x = reorder(location, count), y = count, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("total_deaths" = "steelblue", "excess_mortality_cumulative_absolute" = "darkred"),
                    labels = c("Total Confirmed Deaths", "Excess Mortality (Absolute)")) +
  labs(title = "Excess Mortality vs Confirmed COVID-19 Deaths",
       subtitle = "Top 15 Countries by Absolute Excess Mortality",
       x = "Country",
       y = "Number of Deaths",
       fill = "Metric")

print("Saving plot to output/excess_mortality_comparison.png")
ggsave("output/excess_mortality_comparison.png", plot, width = 12, height = 8, bg = "white")
print("Done.")
