## tidy tuesday 2023
## week 19 

# libraries ________
library(tidyverse)
library(tidytuesdayR)
library(sf)
library(tigris)
library(stringr)

# data ________
data = tidytuesdayR::tt_load(2023, week  = 19)
childcare_costs <- data$childcare_costs
counties <- data$counties

# wrangle ________
# get only counties for Virginia 
va_counties = counties %>% 
  filter(state_abbreviation == "VA")

# bring in shapefiles for counties in Virginia 
va_shapes = tigris::counties(state = "VA") %>% 
  janitor::clean_names() %>%
  st_transform(crs = 6591) %>% 
  mutate(geoid = as.numeric(geoid)) %>% 
  select(geoid, name, geometry)

# filter childcare data 
va_cc = childcare_costs %>%
  filter(study_year == 2018) %>% 
  select(county_fips_code, mme_2018, mc_infant, mc_toddler, mc_preschool) %>% 
  mutate(infant = mc_infant * 52,
         toddler = mc_toddler * 52,
         preschool = mc_toddler * 52) %>% 
  pivot_longer(infant:preschool, names_to = "age", values_to = "cost") %>% 
  select(-mc_infant, -mc_toddler, -mc_preschool) %>% 
  mutate(pct_income = cost/mme_2018)

# merging
data = va_shapes %>% 
  inner_join(va_counties, by = c("geoid" = "county_fips_code")) %>% 
  inner_join(va_cc, by = c("geoid" = "county_fips_code")) 




# plot
ggplot(data) +
  geom_sf(aes(fill = pct_income)) +
  facet_wrap(~age) +
  theme_void()




