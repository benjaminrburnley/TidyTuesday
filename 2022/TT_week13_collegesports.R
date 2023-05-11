### packages 
library(tidyverse)
library(ggridges)
library(ggthemes)

### import data 
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

### wrangle
select_sports = c("Baseball", "Basketball", "Football", "Golf", "Lacrosse", "Soccer", "Softball", "Tennis", "Track and Field, X-Country", "Volleyball")

male_sports = sports%>%
  filter(sports %in% select_sports & classification_name == "NCAA Division I-FBS")%>%
  select(institution_name, sports, exp_men)%>%
  mutate(male = 1)

female_sports = sports%>%
  filter(sports %in% select_sports & classification_name == "NCAA Division I-FBS")%>%
  select(institution_name, sports, exp_women)%>%
  mutate(male = 0)

  

### plot 
ggplot()+
  geom_density_ridges(data = male_sports, aes(x = exp_men, y = sports, color = factor(male), fill = factor(male)), alpha = .3, scale = 1.33, rel_min_height = 0.001)+
  geom_density_ridges(data = female_sports, aes(x = exp_women, y = sports, color = factor(male), fill = factor(male)), alpha = .3, scale = 1.33, rel_min_height = 0.001)+
  scale_x_log10(breaks = c(10000, 100000, 1000000, 10000000, 100000000), labels = c("$10,000", "$100,000", "$1,000,000", "$10,000,000", "$100,000,000"))+
  scale_y_discrete(limits = unique(rev(select_sports)))+
  scale_color_manual(values = c("#782F40", "#CEB888"), name = "Sport", labels = c("Women's Sport", "Men's Sport"))+
  scale_fill_manual(values = c("#782F40", "#CEB888"), name = "Sport", labels = c("Women's Sport", "Men's Sport"))+
  labs(title = "Expenditure Differences in College Sports by Gender",
       subtitle = "The density curves below show the distribution of expenditures across the top ten most popular in Division I sports by participation.",
       x = "Total Expenditures on Program",
       caption = "Source: Equity in Athletics Data Analysis | #TidyTuesday 2022 W13 | @ben_burnley")+
  theme_ridges()+
  theme(
    plot.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "serif", color = "#666666"),
    axis.text = element_text(family = "serif", color = "#666666"),
    plot.title = element_text(family = "serif", size = 24, color = "#666666"),
    plot.caption = element_text(family = "serif", size = 8, color = "#666666"),
    plot.subtitle = element_text(family = "serif", size = 12, color = "#666666"),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif", size = 12, color = "#666666"),
    legend.position = "top"
  )

ggsave("week13.png")
