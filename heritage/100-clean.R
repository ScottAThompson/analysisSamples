# clean each of the data sets in df$data column, datatype column is used
# to determine cleaning function, datatype_clean is name of function
library(stringr)
library(here)
library(dplyr)
library(purrr)

# data cleaning functions

cleanSpecificDataType <- function(datatype, data) {
	cleanFn <- str_c(datatype, "_clean")
	cat(paste0("Cleaning ", datatype, " data using ", cleanFn, '\n'))
	eval(call(cleanFn, data))
}

no_cleaning <- function(data) {
	# nothing done here
	data
}

null_clean <- function(data) {
	# returns a NA useful for debugging / coding 
	NA
}

basic_cleaning <- function(data) {
	# minimal work done here
	# id is MemberID converted to integer
	# year is Year with contents changed from Y1, Y2, Y3 to 1,2,3 (integer)
	# dsfs is DSFS changed to numeric by first element of range + 1
	# change column names to lower case
	t1 <- data 
	if ("MemberID" %in% names(data)) {
		t1 <- t1 %>%
			rename(id = MemberID) %>%
			mutate(id = as.integer(id)) 
	}
	if ("Year" %in% names(data)) {
		t1 <- t1 %>%
			rename(year = Year) %>%
			mutate(year = as.integer(str_remove(year, "Y")))
	}
	if ("DSFS" %in% names(data)) {
		t1 <- t1 %>%
			rename(dsfs = DSFS) %>%
			mutate(dsfs = as.integer(str_remove(dsfs, "-.*")) + 1)
	}
	t1 %>% setNames(str_to_lower(names(.)))
}

# aliases
target_clean <- no_cleaning
daysinhospital_y2_clean <- no_cleaning
daysinhospital_y3_clean <- no_cleaning
lookupprimaryconditiongroup_clean <- no_cleaning
lookupproceduregroup_clean <- no_cleaning
claims_clean <- null_clean

count_clean <- function(data) {
	# no missing data
	# count is (Lab|Drug)Count with + removed from right censored data and converted to numeric
	data %>%
		basic_cleaning() %>%
		mutate(
			count = as.integer(str_remove(data %>% select(ends_with("count")) %>% pull(), "\\+"))) %>%
		select(id, year, dsfs, count)
}

labcount_clean <- function(data) {
	# wrapper for count_clean changes count column to nlabs	
	count_clean(data) %>% rename(nlabs = count)
}

drugcount_clean <- function(data) {
	# wrapper for count_clean changes count column to ndrugs
	count_clean(data) %>% rename(ndrugs = count)
}

members_clean <- function(data) {
	# mark the missing data  
	# id is MemberID converted to integer
	# age is AgeAtFirstClaim after conversion to integer at midpoint of range
	# ageMiss is indicator indicating AgeAtFirstClaim was missing
	# sex is Sex with missing values converted to missing
	data %>%
		basic_cleaning() %>%
		mutate(age = ifelse(is.na(ageatfirstclaim), 0, suppressWarnings(as.integer(str_remove(ageatfirstclaim, "-.*")) + 5)),
			ageMiss = as.integer(is.na(ageatfirstclaim)), 
			sex = ifelse(is.na(sex), "unknown", str_to_lower(sex))) %>%
		select(id, age, ageMiss, sex)
}

replaceMissingWithValue <- function(data, value) {
	ifelse(is.na(data), value, data)
}

replaceMissingWithValueAndReturnAsInteger <- function(data, value) {
	as.integer(replaceMissingWithValue(data, value))
}

convertLengthOfStayToDaysAndFillNAwithZero <- function(data) {
	suppressWarnings(case_when(
		str_detect(data, "day") ~ as.integer(str_remove(data, " day.*")),
		data == "1- 2 weeks" ~ 11L,
		data == "2- 4 weeks" ~ 21L,
		data == "4- 8 weeks" ~ 42L,
		data == "26+ weeks" ~ 182L,
		is.na(data) ~ 0L,
		TRUE ~ NA_integer_
	))
}

convertCharlsonIndexToInteger <- function(data) {
	case_when(
		data == "0" ~ 0L,
		data == "1-2" ~ 2L,
		data == "3-4" ~ 4L,
		data == "5+" ~ 6L,
		TRUE ~ NA_integer_
	)
}

claims_clean <- function(data) {
	# basic clean plus
	# providerid - convert to integer and change missing values to 0
	# vendor - convert to integer and change missing values to 0
	# pcp - convert to integer and change missing values to 0
	# specialty - changing missing values to unknown
	# lengthofstay - convert to integer days and fill missing values with zero
   # primaryconditiongroup fill NA with unknown
	# convert charlsonindex to integer
   # proceduregroup fill NA with unknown
   # convert suplos to integer
	data %>%
		basic_cleaning() %>%
		mutate(providerid = replaceMissingWithValueAndReturnAsInteger(providerid, 0),
			vendor = replaceMissingWithValueAndReturnAsInteger(vendor, 0),
			paydelay = as.integer(ifelse(paydelay == "162+", 162, paydelay)),
			pcp = replaceMissingWithValueAndReturnAsInteger(pcp, 0),
			specialty = replaceMissingWithValue(specialty, "unknown"),
			placesvc = replaceMissingWithValue(placesvc, "unknown"),
			lengthofstay = convertLengthOfStayToDaysAndFillNAwithZero(lengthofstay),
			primaryconditiongroup = replaceMissingWithValue(primaryconditiongroup, "unknown"),
			charlsonindex = convertCharlsonIndexToInteger(charlsonindex),
			proceduregroup = replaceMissingWithValue(proceduregroup, "unknown"),
			suplos = as.integer(suplos))
}

cleanData <- function(df, cleanOnly = NA) {
	if (is.na(cleanOnly)) {
		df %>% 
			mutate(cleaned = map2(datatype, data, cleanSpecificDataType)) %>%
			select(datatype, cleaned) %>%
			rename(data = cleaned)
	} else {
		df %>% 
			filter(datatype %in% cleanOnly) %>%
			mutate(cleaned = map2(datatype, data, cleanSpecificDataType)) %>%
			select(datatype, cleaned) %>%
			rename(data = cleaned)
	}
}


### Main code

load(here("outputs/load.Rdata"))
df <- cleanData(df)
save(df, file=here("outputs/clean.Rdata"))

