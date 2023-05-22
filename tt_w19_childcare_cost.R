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
  select(-mc_infant, -mc_toddlert, -mc_preschool) %>% 
  mutate(pct_income = cost/mme_2018)

# merging
data = va_shapes %>% 
  inner_join(va_counties, by = c("geoid" = "county_fips_code")) %>% 
  inner_join(va_cc, by = c("geoid" = "county_fips_code")) %>% 
  mutate(cat_pct = case_when(
    pct_income <= .15 ~ "< 15%",
    pct_income > .15 & pct_income <= .25 ~ "15% to 25%",
    pct_income > .25 & pct_income <= .35 ~ "25% to 35%",
    pct_income > .35 & pct_income <= .45 ~ "35% to 45%",
    pct_income > .45 ~ "> 45%",
    TRUE ~ NA),
  ) %>% 
  mutate(cat_pct = factor(cat_pct, levels = c("< 15%", 
                                              "15% to 25%",
                                              "25% to 35%",
                                              "35% to 45%",
                                              "> 45%")))
data2 = data %>% 
  filter(pct_income < 1.01) %>% 
  mutate(age = case_when(
    age == "infant" ~ "Infant Care",
    age == "preschool" ~ "Preschool",
    age == "toddler" ~ "Toddler Care",
    TRUE ~ NA
  ))

# plot
ggplot(data2) +
  geom_sf(aes(fill = cat_pct)) +
  facet_wrap(~age, nrow = 2, ncol = 2) +
  scale_fill_brewer(palette = "YlGnBl")+
  theme_void()+
  labs(
    title = "Cost of Center-Based Childcare in Virginia",
    subtitle = "Plots below show county average childcare cost as a percentage of county median income",
    fill = "Percentage of County Median Income"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )




