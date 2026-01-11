library(dplyr)

data %>%
  select(id, name, city, spending) %>%
  filter(city == "BKK" & spending >= 1000)%>%
  arrange(spending) %>%
  head(100)
  
