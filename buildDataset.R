library(dplyr)
library(fields)
library(lubridate)

# - load/merge data -
intakes <- read.csv("dat/Austin_Animal_Center_Intakes_20241014.csv")
outcomes <- read.csv("dat/Austin_Animal_Center_Outcomes_20241014.csv")
aac_dataset <- merge(intakes, outcomes, by = "Animal.ID", all.x = TRUE)

# - data wrangling -

# remove redundant columns
aac_dataset[, c("Name.y", "Animal.Type.y", "Breed.y", "Color.y")] <- NULL

# more sensible column names
colnames(aac_dataset) <- c(
  "AID", "name", "inDateTime", "inDate",
  "location", "inType", "inCond", "animalType",
  "inSex", "inAge", "breed", "color",
  "outDateTime", "outDate", "DOB", "outType",
  "outSubtype", "outSex", "outAge"
)

# restrict to time frame of last 5 years
aac_dataset <- aac_dataset[as.numeric(gsub("\\D", "", aac_dataset$inDate)) >= 2019, ]
aac_dataset <- aac_dataset[as.numeric(gsub("\\D", "", aac_dataset$outDate)) >= 2019, ]

# remove entries with no identifier or age metric ability
aac_dataset <- subset(aac_dataset, !is.na(AID) & !is.na(inAge))

# match duplicated entries to the correct intake/outcome pairing
# method: 
# - for each group of duplicates (for example, Champion A416147):
#   - remove any entries that contain an outDateTime that occurs before an inDateTime
#   - starting with the earliest inDateTime
#     - find the entry with an outDateTime that is the least amount of time away
#     - delete all other entries for that inDateTime
#     - repeat for the next inDateTime until through all duplicate entries
#   - TODO: case management for duplicate groups with an outcome w/o an intake due to restricted time frame\
# finding duplicates:
# aac_dataset %>% group_by(AID) %>% filter(n()>1) %>% ungroup()

# set proper column typing
aac_dataset$inDateTime <- mdy_hms(aac_dataset$inDateTime)
aac_dataset$outDateTime <- mdy_hms(aac_dataset$outDateTime)
aac_dataset$DOB <- mdy(aac_dataset$DOB)
