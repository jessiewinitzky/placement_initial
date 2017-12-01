library(tidyverse)
#get data
placement <- read.table("H:/My Documents/Secondary Data Research/Placement/Analysis/placement_initial.csv", header = TRUE, sep = ",")

#create new variable: CPT 1 and 2 are reading score; CPT 3,4,5 are math scores
placement <- placement %>% mutate(subject = ifelse(TEST == "CPT1" | TEST == "CPT2", "Reading", "Math"))

#remove LaKiesha Fehoko
placement <- placement %>% filter(ID != "S00048422")
placement %>% filter(ID =="S00048422")

placement %>% dplyr::distinct(ID)
#1,048 unique students

#separate math and english into two different datasets
reading <- placement %>% filter(subject == "Reading")
math <- placement %>% filter(subject == "Math")

reading %>% dplyr::distinct(ID)
#901 unique students went through READING placement

math %>% dplyr::distinct(ID)
#979 unique students went through MATH placement

experiment <- placement %>% filter(TESTING_GROUP == "998" | TESTING_GROUP == "999")
experiment %>% dplyr::distinct(ID)
#618 unique students were randomized into 998 or 999

traditional <- placement %>% filter(TESTING_GROUP == "Traditional")

experiment %>% filter(dplyr::distinct(ID)) %>%
                     table(experiment$TESTING_GROUP)

#I want number of students who were randomized into each group
experiment %>% filter(TESTING_GROUP == 998) %>% dplyr::distinct(ID)
#998 = 327

experiment %>% filter(TESTING_GROUP == 999) %>% dplyr::distinct(ID)
#999 = 292

# number of students total - 998 students - 999 students = number of students randomized into Accuplacer
1048-327-292
#Traditional = 429

#how often do 998/999 students choose to take accuplacer?
#identify 998/999 : experiment dataset
#left join experiment dataset with traditional dataset

#first, scrape top layer of traditional so there's only one observation
trad_unique <- traditional %>% 
  group_by(ID) %>%
  arrange(TEST_DATE) %>%
  slice(1)

exp_join <- left_join(experiment, trad_unique, by = "ID")

retakes <- exp_join %>%
  arrange(PERSON_UID.y) %>%
  slice(1:113)

retakes_998 <- retakes %>%
  filter(TESTING_GROUP.x == 998)

retakes_998 %>% dplyr::distinct(ID)
#40
40/327
# 12.2%

retakes_999 <- retakes %>%
  filter(TESTING_GROUP.x == 999)

retakes_999 %>% dplyr::distinct(ID)
#17
17/292
# 5.8%
