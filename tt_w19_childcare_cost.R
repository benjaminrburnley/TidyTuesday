## tidy tuesday 2023
## week 19 

# libraries 
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(tigris)

# data 
data = tidytuesdayR::tt_load(2023, week  = 19)
childcare_costs <- data$childcare_costs
counties <- data$counties

# wrangle 
va_counties = counties %>% 
  filter(state_abbreviation == "VA")

va_shapes = tigris::counties(state = "VA") %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 6591)

plot(st_geometry(va_shapes))
