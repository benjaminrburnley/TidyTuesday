### tidy tuesday week 12 'baby names'

### packages 
library(tidyverse)


### import data 
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


### wrangle

names = c("John", "Paul", "George", "Ringo")
data = babynames %>%
  group_by(name)%>%
  filter(name %in% names & sex == "M")


### plot 

ggplot(data = data, aes(x = year, y = n, color = name))+
  geom_line()+
  geom_vline(aes(xintercept = 1964), linetype = "dashed")+
  coord_cartesian(xlim = c(1920,2000))+
  annotate("text", x = 1980, y = 75000, label = "Beatles' American Debut - February 7, 1964", family = "serif")+
  annotate("text", x = 1980, y = 72000, label = "The Ed Sullivan Show", family = "serif")+
  annotate(geom = "curve", x = 1980, y = 77000, xend = 1965, yend = 80000, 
           curvature = .3, arrow = arrow(length = unit(2, "mm")))+
  labs(title = "British Invasion Backlash?",
       subtitle = "Beatles names as baby names, 1920 - 2000",
       x = "Year",
       y = "Frequency of Name",
       caption = "Source: babynames by Hadley Wickham | #TidyTuesday 2022 W6 | @ben_burnley",
       color = "Name")+
  theme_classic()+
  theme(text = element_text(family = "serif"))+
  scale_color_manual(values =  c("#ef1921", "#41bcde", "#e67010", "#fbc01a"))

ggsave("beatles_names.png", height = 5.5, width = 9.7)
