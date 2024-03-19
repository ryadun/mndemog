
# Source: Minnesota State Demographic Center

library(readxl)
library(dplyr)
library(stringr)

mndemog_url <- "https://mn.gov/admin/assets/mn-cities-townships-historical-estimates-sdc-2000-2022_tcm36-586679.xlsx"

if (!file.exists("data-raw/mndemog.xlsx")) {
  download.file(mndemog_url, "data-raw/mndemog.xlsx", mode = "wb", cacheOK = FALSE, method = "libcurl")
}


# Import Estimates from MN Demographer
# Last column has junk data, ignored
# Default to first sheet being imported
mncitypop <- read_excel("data-raw/mndemog.xlsx",
                      .name_repair = "universal",
                      na = c(" ", "-", "N/A"),
                      range = cell_cols("A:F")
)

# clean table
mncitypop <- mncitypop %>%
  rename(Persons.Per.Household = Persons.Per.Household..PPH.)


# Add columns for matching to MN Department of Revenue dataset
# Data nuance:
# Multi-County cities will have a record for each county
# The total will be in a record where the county is "Multi-County City"
# Ex: Blaine City (part)
# For the purposes of joining the data, the (PART) can be ignored
# An alternative might be to find the most populated county in the group and
#   assign the sum total population there

mndemog_cleancity_fx <- function(cityname) {
  cityname <- str_replace(cityname, "Norwood Young America city", "Norwood Young America Cit")
  cityname <- str_replace(cityname, "^La ", "La")
  cityname <- str_replace(cityname, "^Le ", "Le")
  cityname <- str_replace(cityname, "^De ", "De")
  cityname <- str_replace(cityname, "^Smoky Hollow", "Smokey Hollow")
  cityname <- str_replace(cityname, "Jordan township", "Jordon township")
  cityname <- str_replace(cityname, "^Agder ", "Agdar ")
  cityname <- str_replace(cityname, "^Rose Hill", "Rosehill")
  cityname <- str_replace(cityname, "South Fork", "Southfork")
  cityname <- str_replace(cityname, "^Mount ", "MT ")
  cityname <- str_replace(cityname, "^Mountain ", "MT ")
  cityname <- str_replace(cityname, "^Minnesota ", "Minn ")
  cityname <- str_replace(cityname, "^International ", "Intl ")
  cityname <- str_replace(cityname, "^Nimrod city", "Nimrod Village Of")
  cityname <- str_replace(cityname, "city$", "city of")
  cityname <- str_replace(cityname, "township$", "town of")
  cityname <- str_replace(cityname, "\\(part\\)$", "of")
  cityname <- str_replace(cityname, "\\(balance\\)$", "of")
  cityname <- str_replace(cityname, "\\.", "")
  cityname <- str_replace(cityname, "\\'", "")
  cityname <- toupper(cityname)
  cityname <- str_replace(cityname, "^OTTER TAIL PENINSULA", "OTTER TAIL PEN.")
  cityname <- str_replace(cityname, "^INVER GROVE HEIGHTS CITY OF", "INVER GROVE HT CITY")
  cityname <- str_replace(cityname, "BLUE EARTH CITY TOWN OF", "BLUE EARTH TOWN OF")
  cityname <- str_replace(cityname, "LITTLE ELBOW TOWN OF", "LITTLE ELBOW")
  cityname <- str_replace(cityname, "WHITE BEAR LAKE CITY OF", "WHITE BEAR LK CITY OF")
  cityname <- str_replace(cityname, "LISBON TOWN OF", "LISBON TOWN")

  return(cityname)
}

mncitypop <- mncitypop %>%
  mutate(Year = as.integer(Year),
         Home.County = str_replace(toupper(County.Within), "\\.", ""),
         City.Town = mndemog_cleancity_fx(City.or.Township))

usethis::use_data(mncitypop, overwrite = TRUE)
