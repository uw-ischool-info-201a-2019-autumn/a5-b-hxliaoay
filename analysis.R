```{r}
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gapminder)
library(plyr)
library(lubridate)
```


```{r}
shooting_data <- read.csv("data/shootings-2018.csv")
glimpse(shooting_data)
colnames(shooting_data)
```
```{r}
num_shooting <- nrow(shooting_data)
num_shooting

```
```{r}
num_death <- sum(shooting_data$num_killed)
num_death
```
```{r}
impact_data <- mutate(shooting_data, casualties  = num_injured + num_killed)
highest_impact <- impact_data %>% 
  select(city, casualties) %>%
  group_by(city) %>%
  arrange(casualties) %>%
  tail(1)
highest_impact
```
```{r}
state_count <- impact_data %>%
  dplyr::group_by(state, casualties) %>%
  dplyr::summarize(
    state_cases = n()
  ) %>%
  arrange(state_cases) %>% 
  tail(1)


state_count

```

```{r}
case_count <- impact_data %>%
  dplyr::group_by(city, casualties) %>%
  dplyr::summarize(
    n_cases = n()
  ) %>%
  arrange(n_cases) %>% 
  tail(1)


case_count

```

```{r}
years <- impact_data %>%
  select(date) %>%
  arrange(date)
begin_date <- years %>%
  head(1)
ending_date <- years %>% tail(1)

begin_date
ending_date
```


