# Load the package required to read JSON files.
source('functions.R')
forceLibrary(c("rjson", 'dplyr', 'ggplot2'))

library(utils)
library(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into "R". The dataset will be called "data".
data <- read.csv(tf)

spain_data = data %>% filter(countriesAndTerritories == 'Spain')

# n_weeks = nrow(spain_data)/ 7 %>% ceiling()

weeks = lapply(1:100, rep, 7) %>% unlist() %>% 
  .[1:nrow(spain_data)] %>% as.factor()

spain_data = data %>% filter(countriesAndTerritories == 'Spain') %>% 
  arrange(year, month, day) %>% 
  mutate(week = weeks, 
         cases_cum = cumsum(cases), 
         ) 


world_data = data %>%  arrange(countriesAndTerritories, year, month, day)

new_world_data = world_data %>% 
  left_join(select(.data = spain_data, dateRep, week)) %>% 
  mutate(cases_per_population = cases / popData2018, 
         deaths_per_population = deaths / popData2018, 
         deaths_per_cases = deaths / cases)

world_pop = new_world_data %>% filter(dateRep == '15/04/2020') %>% 
  select(countriesAndTerritories, popData2018)
agg_country = new_world_data %>% group_by(countriesAndTerritories) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>% select(!popData2018) %>% 
  left_join(world_pop) %>% 
  mutate(cases_per_population = cases / popData2018, 
         deaths_per_population = deaths / popData2018)

world_pop_sum = world_pop$popData2018  %>% sum(na.rm = T)
agg_world = new_world_data %>% group_by(dateRep) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>% 
  left_join(select(.data = spain_data, dateRep, week)) %>% 
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


top10_cases = new_world_data %>% 
  filter(countriesAndTerritories %in% top10_cases_countries)

ggplot(top10_cases, aes(x = week, 
                        y = cases_per_population, 
                        colour = countriesAndTerritories)) + 
  geom_boxplot()

ggplot(top10_cases, aes(x = week, 
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

