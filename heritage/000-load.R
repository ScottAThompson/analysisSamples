#
# load all csv files from input directory
#

# Assumptions: 
#	files can be held in memory
# 	tables can be manipulated when in memory
#

library(here)
library(readr)
library(tibble)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

readCSV <- function(file) {
	cat(paste0("Reading ", file, "\n"))
	read_csv(file)
}

datatypes <- list.files(here("input"), pattern="*.csv") %>% 
	str_remove(".csv") %>%
	str_remove_all(" ") %>%
	str_to_lower()

files <- list.files(here("input"), pattern="*.csv", full.names=TRUE)

df <- tibble(datatype = datatypes, file = files)

df <- df %>% mutate(data = map(file, readCSV))

save(df, file=here("outputs/load.Rdata"))

