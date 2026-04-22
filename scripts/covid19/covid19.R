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

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into "R". The dataset will be called "data".
data <- read.csv(tf)

italy_data = data %>% filter(countriesAndTerritories == 'Italy')

# n_weeks = nrow(italy_data)/ 7 %>% ceiling()

weeks = lapply(1:100, rep, 7) %>% unlist() %>% 
  .[1:nrow(italy_data)] %>% as.factor()

italy_data = data %>% filter(countriesAndTerritories == 'Italy') %>%
  arrange(year, month, day) %>%
  mutate(week = weeks,
         cases_cum = cumsum(cases),
         )


world_data = data %>%  arrange(countriesAndTerritories, year, month, day)

new_world_data = world_data %>% 
  left_join(select(.data = italy_data, dateRep, week)) %>% 
  mutate(cases_per_population = cases / popData2019, 
         deaths_per_population = deaths / popData2019, 
         deaths_per_cases = deaths / cases)

world_pop = new_world_data %>% 
  filter(!duplicated(countriesAndTerritories)) %>% 
  select(countriesAndTerritories, popData2019)
world_pop_sum = world_pop$popData2019  %>% sum(na.rm = T)


agg_country = new_world_data %>% group_by(countriesAndTerritories) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>% select(!popData2019) %>% 
  left_join(world_pop) %>% 
  mutate(cases_per_population = cases / popData2019, 
         deaths_per_population = deaths / popData2019)

agg_world = new_world_data %>% group_by(dateRep) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>% 
  left_join(select(.data = italy_data, dateRep, week)) %>% 
  mutate(cases_per_population = cases / world_pop_sum, 
         deaths_per_population = deaths / world_pop_sum,
         countriesAndTerritories = 'World')

new_world_data = new_world_data %>% bind_rows(agg_world)

world = new_world_data %>% filter(countriesAndTerritories == 'World') %>% 
  arrange(year, month, day)

top10_cases_countries = 
  c('United_States_of_America', 
    'Spain', 
    'Italy', 
    'Netherlands', 
    'France', 
    'World') 
#'United_States_of_America', 'Spain', 'Italy', 'Germany', 'France', 'China'



agg_country = agg_country %>% 
  filter(popData2019 > 10^6)

agg_country %>% 
  select(countriesAndTerritories, cases_per_population) %>% 
  arrange(desc(cases_per_population)) %>% 
  head(10) %>% 
  column_to_rownames('countriesAndTerritories') %>% 
  as.matrix() %>% t() %>% 
  barplot(las=2, main = 'cases_per_population')

agg_country %>% 
  select(countriesAndTerritories, deaths_per_population) %>% 
  arrange(desc(deaths_per_population)) %>% 
  head(10) %>% 
  column_to_rownames('countriesAndTerritories') %>% 
  as.matrix() %>% t() %>% 
  barplot(las=2, main = 'deaths_per_population')


top10_cases_countries = agg_country %>% 
  select(countriesAndTerritories, deaths_per_population) %>% 
  arrange(desc(deaths_per_population)) %>% 
  head(10) %>% 
  select(countriesAndTerritories) %>% 
  unlist()
  
top10_cases = new_world_data %>% 
  filter(countriesAndTerritories %in% top10_cases_countries) %>% 
  negToZero()
  
# top10_cases$deaths_per_population[top10_cases$deaths_per_population < 0] = NA
# top10_cases$deaths_per_population %>% negToZero() %>% head
# top10_cases$deaths_per_population < 0] = NA
# 
# mutate(cases_per_population = negToZero(cases_per_population),
#          deaths_per_population = negToZero(deaths_per_population))
top10_cases %>% 
  filter(cases_per_population < max(cases_per_population, na.rm = T), 
         !week %in% c(1:7)) %>% 
  ggplot(aes(x = week, 
                        y = cases_per_population, 
                        colour = countriesAndTerritories)) + 
  geom_boxplot()

top10_cases %>% 
  filter(deaths_per_population < max(deaths_per_population, na.rm = T), 
         !week %in% c(1:7)) %>% 
  ggplot(aes(x = week, 
                        y = deaths_per_population, 
                        colour = countriesAndTerritories)) + 
  geom_boxplot()

ggplot(world, aes(x = week, 
                  y = cases_per_population, 
                  colour = countriesAndTerritories)) + 
  geom_boxplot()

ggplot(world, aes(x = week, 
                  y = deaths_per_population, 
                  colour = countriesAndTerritories)) + 
  geom_boxplot()


