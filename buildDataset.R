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
  "address", "inType", "inCond", "animalType",
  "inSex", "inAge", "breed", "color",
  "outDateTime", "outDate", "DOB", "outType",
  "outSubtype", "outSex", "outAge"
)

# restrict to time frame of last 5 years
aac_dataset <- aac_dataset[
  as.numeric(gsub("\\D", "", aac_dataset$inDate)) >= 2019,
]
aac_dataset <- aac_dataset[
  as.numeric(gsub("\\D", "", aac_dataset$outDate)) >= 2019,
]

# remove entries with no identifier or age metric ability
aac_dataset <- subset(aac_dataset, !is.na(AID) & !is.na(inAge))

# set proper column typing
aac_dataset$inDateTime <- mdy_hms(aac_dataset$inDateTime)
aac_dataset$outDateTime <- mdy_hms(aac_dataset$outDateTime)
aac_dataset$DOB <- mdy(aac_dataset$DOB)

# match duplicated entries to the correct intake/outcome pairing
filter_group <- function(group) {
  entries <- nrow(group)
  actual <- sqrt(entries)

  # this removes any contaminated entries that likely wouldn't be able to be
  # interpreted correctly without human intervention as a result of an outcome
  # entry that is missing its intake entry due to the time frame restriction
  if (actual != floor(actual)) return(data.frame())

  # actual = 2; keep = 1, 4
  # actual = 3; keep = 1, 5, 9
  # actual = 4; keep = 1, 6, 11, 16
  # y = (a*(i-1)) + i
  keep <- numeric(actual)
  for (i in 1:actual) {
    keep[i] <- (actual * (i - 1)) + i
  }

  return(group[keep, ])
}

matched_data <- aac_dataset |>
  group_by(AID) |> # internally group each AID
  filter(n() > 1) |> # keep only duplicates in this pipe
  arrange(AID, inDateTime, outDateTime) |> # sort by these cols, in this order
  group_modify( # apply function that filters out bad duplicates
    ~ filter_group(.x)
  ) |>
  ungroup() # return to standard form w/o groups

# convert ages to usable decimal format
conv_age <- function(ages) {
  ages <- sapply(ages, strsplit, " ")
  ages <- sapply(ages, function(age) {
    if (length(age) != 2) return(-1)

    age_num <- as.numeric(age[[1]])
    age_scope <- age[[2]]

    return(
      switch(
        age_scope,
        "years" = age_num,
        "months" = age_num / 12,
        "weeks" = age_num / 52.143,
        "days" = age_num / 365,
        -1
      )
    )
  })
  ages <- sapply(ages, round, digits = 3)

  return(ages)
}

# perform all modifications on primary dataset
aac_dataset <- aac_dataset |>
  group_by(AID) |>
  filter(n() == 1) |> # keep only unique values in this pipe
  ungroup() |>
  bind_rows(matched_data) |> # append all matched duplicate entries to main data
  arrange(AID) |> # sort after having added duplicates to bottom of dataframe
  mutate(across(c(inAge, outAge), conv_age)) |> # convert ages to decimal format
  filter(inAge != -1 & outAge != -1) # remove invalidated entries

save(aac_dataset, file = "dat/aac_dataset.rda")

# - geocoding -

# create data subset for geocoding, if it doesn't already exist
if (!file.exists("dat/raw_addrs.csv")) {
  geo_subset <- aac_dataset %>%
    mutate(idx = row_number()) %>%
    select(idx, AID, address)
  write.csv(geo_subset, "dat/raw_addrs.csv", row.names = FALSE)
}

# create csv file for geocoded results, if it doesn't already exist
if (!file.exists("dat/geocoded_addrs.csv")) {
  base_df <- data.frame(
    idx = 0, AID = 0, address = "AAC",
    lat = 30.2521117, lon = -97.6872072
  )
  write.csv(base_df, "dat/geocoded_addrs.csv", row.names = FALSE)
}
