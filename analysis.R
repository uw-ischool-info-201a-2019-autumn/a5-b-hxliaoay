suppressMessages(library(lintr))
#libraries
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyverse))
suppressMessages(library(gapminder))
suppressMessages(library(plyr))
suppressMessages(library(lubridate))
suppressMessages(library(rjson))
suppressMessages(library(jsonlite))
suppressMessages(library(leaflet))
suppressMessages(library(RCurl))
#import csv
shooting_data <- read.csv("data/shootings-2018.csv")

#summary information

#number of mass shootings
num_shooting <- nrow(shooting_data)

#number of deaths
num_death <- sum(shooting_data$num_killed)

#city that is most impacted, by sum of injured and killed
impact_data <- mutate(shooting_data, casualties  = num_injured + num_killed)
highest_impact <- impact_data %>%
  select(city, casualties) %>%
  group_by(city) %>%
  arrange(casualties) %>%
  tail(1)

highest_impact_city <- highest_impact %>%
  select(city)
#state with most cases
state_count <- impact_data %>%
  dplyr::group_by(state, casualties) %>%
  dplyr::summarize(
    state_cases = n()
  ) %>%
  arrange(state_cases) %>%
  tail(1)
most_cases_state <- state_count %>%
  select(state)
#city with most cases
case_count <- impact_data %>%
  dplyr::group_by(city, casualties) %>%
  dplyr::summarize(
    n_cases = n()
  ) %>%
  arrange(n_cases) %>%
  tail(1)

most_cases_city <- case_count %>%
  select(city)

#starting and ending date of data collected
years <- impact_data %>%
  select(date) %>%
  arrange(date)
begin_date <- years %>%
  head(1)
ending_date <- years %>% tail(1)


# table
cases <- shooting_data %>%
  select(city, state, num_killed, num_injured) %>%
  arrange(city) %>%
  group_by(city) %>%
  dplyr::summarise(

    city_cases = n()
  )

deaths <- shooting_data %>%
  select(city, state, num_killed, num_injured) %>%
  arrange(city) %>%
  group_by(city) %>%
  dplyr::summarise(
    city_deaths = sum(num_killed)
  )

injuries <- shooting_data %>%
  select(city, state, num_killed, num_injured) %>%
  arrange(city) %>%
  group_by(city) %>%
  dplyr::summarise(
    city_injuries = sum(num_injured)
  )
states <- shooting_data %>%
  select(city, state) %>%
  distinct()

shooting_table <- full_join(cases, deaths, by = "city") %>%
  full_join(injuries, by = "city") %>%
  full_join(states, by = "city") %>%
  arrange(city_cases, decreasing = TRUE)


# parkland school shooting analysis
parkland_shooting <- shooting_data %>%
  filter(state == "Florida", city == "Pompano Beach (Parkland)")

parkland_date <- parkland_shooting %>%
  select(date)

parkland_state <- parkland_shooting %>%
  select(state)

parkland_city <- parkland_shooting %>%
  select(city)

parkland_address <- parkland_shooting %>%
  select(address)

parkland_deaths <- parkland_shooting %>%
  select(num_killed)

parkland_injured <- parkland_shooting %>%
  select(num_injured)

#shooting map
death_pal <- colorNumeric("darkorchid", shooting_data$num_killed)
shooting_map <- leaflet(data = shooting_data) %>%
  addTiles() %>%
  addCircles(popup = shooting_data$address,
             color = ~death_pal(num_killed)) %>%
  addLegend(pal = death_pal, values = ~num_killed)

#plot
# uses table from above instead of original dataset due to summarized by city
# rather than cases
plot_data <- shooting_table %>%
  select(city, city_cases, city_deaths, city_injuries) %>%
  mutate(lethality = city_deaths / (city_deaths + city_injuries))
plot_data_m <- plot_data %>%
  select(city, lethality, city_deaths, city_injuries)
plot_free <- plot_data_m %>%
  ggplot(aes(x = city_injuries, y = city_deaths, color = lethality)) +
  geom_point() +
  ggtitle("Lethality of Shooting")
