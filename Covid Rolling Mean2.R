library(zoo) # moving averages        
library(tidyverse) # all tidyverse packages
library(hrbrthemes) # themes for graphs
library(socviz) # %nin%
library(geofacet) # maps
library(usmap) # lat and long
library(socviz) # for %nin%
library(ggmap) # mapping
library(dplyr)
library(lubridate)

state<-adults %>%
  group_by(collection_week, state) %>%
  summarise_at(vars(inpatient_beds_used,inpatient_beds_covid,icu_beds_used,icu_beds_covid),
               sum,na.rm=TRUE) %>%
  ungroup()


# Inp Used Rolling mean
inpatient_beds_used_roll <- state %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(inpatient_beds_used_1w = zoo::rollmean(inpatient_beds_used, k = 1, fill = NA),
                inpatient_beds_used_3w = zoo::rollmean(inpatient_beds_used, k = 3, fill = NA),
                inpatient_beds_used_5w = zoo::rollmean(inpatient_beds_used, k = 5, fill = NA),
                inpatient_beds_used_7w = zoo::rollmean(inpatient_beds_used, k = 7, fill = NA))%>% 
  dplyr::ungroup()

#Example IL
IL_inp_used<- inpatient_beds_used_roll %>% 
  dplyr::arrange(collection_week) %>% 
  dplyr::filter(state == "IL") %>% 
  dplyr::select(state,
                collection_week,
                inpatient_beds_used,
                inpatient_beds_used_1w:inpatient_beds_used_7w) 

#visualize data
ggplot(data = IL_inp_used, aes(x = collection_week, y = inpatient_beds_used_1w)) +
  geom_point() +
  labs(x = "Collection Week",
       y = "Inpatient Beds Used",
       title = "Rolling Average 1 week",
       subtitle = "Inpatient Beds Used")

# Inp Covid Rolling mean
inpatient_beds_covid_roll <- state %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(inpatient_beds_covid_1w = zoo::rollmean(inpatient_beds_covid, k = 1, fill = NA),
                inpatient_beds_covid_3w = zoo::rollmean(inpatient_beds_covid, k = 3, fill = NA),
                inpatient_beds_covid_5w = zoo::rollmean(inpatient_beds_covid, k = 5, fill = NA),
                inpatient_beds_covid_7w = zoo::rollmean(inpatient_beds_covid, k = 7, fill = NA))%>% 
  dplyr::ungroup()

#Example IL
IL_inp_covid<- inpatient_beds_covid_roll %>% 
  dplyr::arrange(collection_week) %>% 
  dplyr::filter(state == "IL") %>% 
  dplyr::select(state,
                collection_week,
                inpatient_beds_covid,
                inpatient_beds_covid_1w:inpatient_beds_covid_7w) 

#visualize data
ggplot(data = IL_inp_covid, aes(x = collection_week, y = inpatient_beds_covid_1w)) +
  geom_point() +
  labs(x = "Collection Week",
       y = "Inpatient Beds Covid",
       title = "Rolling Average 1 week",
       subtitle = "Inpatient Beds Covid")

# ICUCovid Rolling mean
icu_beds_covid_roll <- state %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(icu_beds_covid_1w = zoo::rollmean(icu_beds_covid, k = 1, fill = NA),
                icu_beds_covid_3w = zoo::rollmean(icu_beds_covid, k = 3, fill = NA),
                icu_beds_covid_5w = zoo::rollmean(icu_beds_covid, k = 5, fill = NA),
                icu_beds_covid_7w = zoo::rollmean(icu_beds_covid, k = 7, fill = NA))%>% 
  dplyr::ungroup()

#Example IL
IL_icu_covid<- icu_beds_covid_roll %>% 
  dplyr::arrange(collection_week) %>% 
  dplyr::filter(state == "IL") %>% 
  dplyr::select(state,
                collection_week,
                icu_beds_covid,
                icu_beds_covid_1w:icu_beds_covid_7w) 

#visualize data
ggplot(data = IL_icu_covid, aes(x = collection_week, y = icu_beds_covid_1w)) +
  geom_point() +
  labs(x = "Collection Week",
       y = "ICU Beds Covid",
       title = "Rolling Average 1 week",
       subtitle = "ICU Beds Covid")

# ICU Utilization Rolling mean
icu_beds_used_roll <- state %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(icu_beds_used_1w = zoo::rollmean(icu_beds_used, k = 1, fill = NA),
                icu_beds_used_3w = zoo::rollmean(icu_beds_used, k = 3, fill = NA),
                icu_beds_used_5w = zoo::rollmean(icu_beds_used, k = 5, fill = NA),
                icu_beds_used_7w = zoo::rollmean(icu_beds_used, k = 7, fill = NA))%>% 
  dplyr::ungroup()

#Example IL
IL_icu_used<- icu_beds_used_roll %>% 
  dplyr::arrange(collection_week) %>% 
  dplyr::filter(state == "IL") %>% 
  dplyr::select(state,
                collection_week,
                icu_beds_used,
                icu_beds_used_1w:icu_beds_used_7w) 

#visualize data
ggplot(data = IL_icu_used, aes(x = collection_week, y = icu_beds_used_1w)) +
  geom_point() +
  labs(x = "Collection Week",
       y = "ICU Beds used",
       title = "Average 1 week",
       subtitle = "ICU Beds Used")

#visualize to compare against original data-- doesn't work
inpatient_beds_roll%>% 
  dplyr::filter(state == "Illinois") %>% 
  tidyr::pivot_longer(names_to = "rolling_mean_key", 
                      values_to = "rolling_mean_value", 
                      cols = c(inpatient_beds_used, 
                               Inpatient_beds_used_3w, 
                               Inpatient_beds_used_5w)) %>%
  # after July 31, 2020
  dplyr::filter(collection_week >= lubridate::as_date("2020-07-31",format="%Y-%m-%d") &
                  # before Dec 3, 2021
                  collection_week <= lubridate::as_date("2021-12-03",format="%Y-%m-%d")) %>% 
  ggplot2::ggplot(aes(x = collection_week, 
                      y = rolling_mean_value, 
                      color = rolling_mean_key)) +
  ggplot2::geom_line() +   
  ggplot2::labs(title = "Illinois' rolling average total Inpatient Beds Used", 
                subtitle = "Between 2020-07-31 and 2021-12-03",
                y = "Inpatient Beds", 
                color = "Metric",
                x = "Date") + 
  hrbrthemes::theme_ipsum_rc()



