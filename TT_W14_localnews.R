### set up 
library(tidyverse)
library(MetBrewer)
library(showtext)

### import 
news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

### plot 
ggplot(data = news_orgs, aes(x = year_founded, fill = tax_status_current))+
  geom_histogram(position = "stack")+
  scale_fill_manual(values = met.brewer("Hiroshige", type = "discrete", direction = -1), name = "Tax Status")+
  labs(x = "Year Founded",
       y = "Number of Publications Founded",
       title = "The Rise and Fall of Local News Online",
       subtitle = "The plot below shows the number of digitally focused, local news organizations started in each year.\nPublications started prior to the internet are included if they are now digital-native.",
       caption = "Source:  Project Oasis and UNC Hussman School of Journalism | #TidyTuesday W14 2022 | @ben_burnley")+
  theme_minimal()+
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(size = 36),
    plot.caption = element_text(hjust = 1),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.position = c(.15,.6),
    legend.background = element_rect(color = "gray"),
    plot.background = element_rect(fill = "#F5F5F5")
  )

  
 ggsave("localnews.png", width = 12.35, height = 6.49)
