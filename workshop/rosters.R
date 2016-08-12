library(dplyr)
aed_r <- read.csv("interested.csv")
aed_r %>% 
  filter(WorkshopDate == "Wed, June 29") %>%
  select(Name, WorkshopDate) %>%
  write.csv("wed_june29_workshop.csv", row.names = F)

aed_r %>% 
  filter(WorkshopDate == "Tues, July 5") %>%
  select(Name, WorkshopDate) %>%
  write.csv("tues_july5_workshop.csv", row.names = F)