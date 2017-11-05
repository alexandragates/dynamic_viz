
library(jsonlite)
library(tidyverse)
library(httr)
library(plyr)
library(dplyr)
library(reshape2)
library(RJSONIO)

#get state data
state_info <- read_csv("C:\\Users\\Alix\ Gates\\Documents\\UChicago\\Data\ Visualization\\Git\ Hub\ Repo\\name_to_abrev.csv")

# 2012 data
acs2012g <- read_json("https://api.census.gov/data/2012/acs1?get=NAME,B27001_001E,B27001_002E,B27001_004E,B27001_005E,B27001_007E,B27001_008E,B27001_010E,B27001_011E,B27001_013E,B27001_014E,B27001_016E,B27001_017E,B27001_019E,B27001_020E,B27001_022E,B27001_023E,B27001_030E,B27001_032E,B27001_033E,B27001_035E,B27001_036E,B27001_038E,B27001_039E,B27001_041E,B27001_042E,B27001_044E,B27001_045E,B27001_047E,B27001_048E,B27001_050E,B27001_051E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2012r <- read_json("https://api.census.gov/data/2012/acs1?get=NAME,B27001H_003E,B27001H_004E,B27001H_006E,B27001H_007E,B27001H_009E,B27001H_010E,B27001H_012E,B27001H_013E,B27001H_015E,B27001H_016E,B27001H_018E,B27001H_019E,B27001H_021E,B27001H_022E,B27001I_003E,B27001I_004E,B27001I_006E,B27001I_007E,B27001I_009E,B27001I_010E,B27001I_012E,B27001I_013E,B27001I_015E,B27001I_016E,B27001I_018E,B27001I_019E,B27001I_021E,B27001I_022E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()

#join data
acs2012 <- join(acs2012g, acs2012r, by = "V1")

# change column names
colnames(acs2012) <- c("Name", "Insurance Status Total", "Total Male", "Male Under 6 w Insurance", "Male Under 6 wo Insurance", "Male Under 6-17 w Insurance", "Male Under 6-17 wo Insurance", "Male Under 18-24 w Insurance", "Male Under 18-24 wo Insurance", "Male Under 25-34 w Insurance", "Male Under 25-34 wo Insurance", "Male Under 35-44 w Insurance", "Male Under 35-44 wo Insurance", "Male Under 45-54 w Insurance", "Male Under 45-54 wo Insurance", "Male Under 55-64 w Insurance", "Male Under 55-64 wo Insurance",
                       "Total Female", "Female Under 6 w Insurance", "Female Under 6 wo Insurance", "Female Under 6-17 w Insurance", "Female Under 6-17 wo Insurance", "Female Under 18-24 w Insurance", "Female Under 18-24 wo Insurance", "Female Under 25-34 w Insurance", "Female Under 25-34 wo Insurance", "Female Under 35-44 w Insurance", "Female Under 35-44 wo Insurance", "Female Under 45-54 w Insurance", "Female Under 45-54 wo Insurance", "Female Under 55-64 w Insurance", "Female Under 55-64 wo Insurance", "fips code",
                       "white under 6 w insurance", "white under 6 wo insurance", "white 6-17 w insurance", "white 6-17 wo insurance", "white 18-24 w insurance", "white 18-24 wo insurance", "white 25-34 w insurance", "white 25-34 wo insurance", "white 35-44 w insurance", "white 35-44 wo insurance", "white 45-54 w insurance", "white 45-54 wo insurance", "white 55-64 w insurance", "white 55-64 wo insurance",
                       "hisp under 6 w insurance", "hisp under 6 wo insurance", "hisp 6-17 w insurance", "hisp 6-17 wo insurance", "hisp 18-24 w insurance", "hisp 18-24 wo insurance", "hisp 25-34 w insurance", "hisp 25-34 wo insurance", "hisp 35-44 w insurance", "hisp 35-44 wo insurance", "hisp 45-54 w insurance", "hisp 45-54 wo insurance", "hisp 55-64 w insurance", "hisp 55-64 wo insurance", "fips code2")
# delete first row
acs2012 <- acs2012[-1,]
acs2012 <- acs2012[-52,]

# change all from factors to characters
acs2012[] <- lapply(acs2012[], function(x) as.character(x))

# change all but name and fips codes from characters to numerics
acs2012[2:62] <- lapply(acs2012[2:62], function(x) as.numeric(x))

# join 2012 data with state info to include state abreviation and region
acs2012 <- left_join(acs2012, state_info, by = "Name")

# add a column of the data's year
acs2012$year<- 2012
acs2012$Year<- "2012"

# do additional calculations
acs2012 <- mutate(acs2012,
                  `total male under 65` = `Male Under 6 w Insurance` + `Male Under 6 wo Insurance` + `Male Under 6-17 w Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 w Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 w Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 w Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 w Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 w Insurance` + `Male Under 55-64 wo Insurance`,
                  `total male insured under 65` = `Male Under 6 w Insurance` + `Male Under 6-17 w Insurance` + `Male Under 18-24 w Insurance` + `Male Under 25-34 w Insurance` + `Male Under 35-44 w Insurance` + `Male Under 45-54 w Insurance` + `Male Under 55-64 w Insurance`,
                  `total male uninsured under 65` = `Male Under 6 wo Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 wo Insurance`,
                  `total female under 65` = `Female Under 6 w Insurance` + `Female Under 6 wo Insurance` + `Female Under 6-17 w Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 w Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 w Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 w Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 w Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 w Insurance` + `Female Under 55-64 wo Insurance`,
                  `total female insured under 65` = `Female Under 6 w Insurance` + `Female Under 6-17 w Insurance` + `Female Under 18-24 w Insurance` + `Female Under 25-34 w Insurance` + `Female Under 35-44 w Insurance` + `Female Under 45-54 w Insurance` + `Female Under 55-64 w Insurance`,
                  `total female uninsured under 65` = `Female Under 6 wo Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 wo Insurance`,
                  `total uninsured under 65` = `total male uninsured under 65` + `total female uninsured under 65`,
                  `percent uninsured under 65` = (`total male uninsured under 65` + `total female uninsured under 65`) / (`total male under 65` + `total female under 65`) * 100,
                  `percent of males under 65 who are insured` = (`total male insured under 65` / `total male under 65`) * 100,
                  `percent of males under 65 who are uninsured` = (`total male uninsured under 65` / `total male under 65`) * 100,
                  `percent of females under 65 who are insured` = (`total female insured under 65` / `total female under 65`) * 100,
                  `percent of females under 65 who are uninsured` = (`total female uninsured under 65` / `total female under 65`) * 100,
                  `percent of uninsured under 65 who are male` = (`total male uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of uninsured under 65 who are female` = (`total female uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of insured under 65 who are male` = (`total male insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `percent of insured under 65 who are female` = (`total female insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `total under 65` = `total male under 65` + `total female under 65`,
                  `total white under 65` = `white under 6 w insurance` + `white under 6 wo insurance`+ `white 6-17 w insurance`+ `white 6-17 wo insurance`+ `white 18-24 w insurance`+ `white 18-24 wo insurance`+ `white 25-34 w insurance`+ `white 25-34 wo insurance`+ `white 35-44 w insurance`+ `white 35-44 wo insurance`+ `white 45-54 w insurance`+ `white 45-54 wo insurance`+ `white 55-64 w insurance`+ `white 55-64 wo insurance`,
                  `total white uninsured under 65` = `white under 6 wo insurance`+ `white 6-17 wo insurance`+ `white 18-24 wo insurance`+ `white 25-34 wo insurance`+ `white 35-44 wo insurance`+ `white 45-54 wo insurance`+ `white 55-64 wo insurance`,
                  `percent white uninsured under 65` = (`total white uninsured under 65` / `total white under 65`) * 100,
                  `total hisp under 65` = `hisp under 6 w insurance`+ `hisp under 6 wo insurance`+ `hisp 6-17 w insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 w insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 w insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 w insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 w insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 w insurance`+ `hisp 55-64 wo insurance`,
                  `total hisp uninsured under 65` = `hisp under 6 wo insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 wo insurance`,
                  `percent hisp uninsured under 65` = (`total hisp uninsured under 65` / `total hisp under 65`) * 100)

# 2013 data - all the same actions as 2012 data
acs2013g <- read_json("https://api.census.gov/data/2013/acs1?get=NAME,B27001_001E,B27001_002E,B27001_004E,B27001_005E,B27001_007E,B27001_008E,B27001_010E,B27001_011E,B27001_013E,B27001_014E,B27001_016E,B27001_017E,B27001_019E,B27001_020E,B27001_022E,B27001_023E,B27001_030E,B27001_032E,B27001_033E,B27001_035E,B27001_036E,B27001_038E,B27001_039E,B27001_041E,B27001_042E,B27001_044E,B27001_045E,B27001_047E,B27001_048E,B27001_050E,B27001_051E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2013r <- read_json("https://api.census.gov/data/2013/acs1?get=NAME,B27001H_003E,B27001H_004E,B27001H_006E,B27001H_007E,B27001H_009E,B27001H_010E,B27001H_012E,B27001H_013E,B27001H_015E,B27001H_016E,B27001H_018E,B27001H_019E,B27001H_021E,B27001H_022E,B27001I_003E,B27001I_004E,B27001I_006E,B27001I_007E,B27001I_009E,B27001I_010E,B27001I_012E,B27001I_013E,B27001I_015E,B27001I_016E,B27001I_018E,B27001I_019E,B27001I_021E,B27001I_022E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2013 <- join(acs2013g, acs2013r, by = "V1")
colnames(acs2013) <- c("Name", "Insurance Status Total", "Total Male", "Male Under 6 w Insurance", "Male Under 6 wo Insurance", "Male Under 6-17 w Insurance", "Male Under 6-17 wo Insurance", "Male Under 18-24 w Insurance", "Male Under 18-24 wo Insurance", "Male Under 25-34 w Insurance", "Male Under 25-34 wo Insurance", "Male Under 35-44 w Insurance", "Male Under 35-44 wo Insurance", "Male Under 45-54 w Insurance", "Male Under 45-54 wo Insurance", "Male Under 55-64 w Insurance", "Male Under 55-64 wo Insurance",
                       "Total Female", "Female Under 6 w Insurance", "Female Under 6 wo Insurance", "Female Under 6-17 w Insurance", "Female Under 6-17 wo Insurance", "Female Under 18-24 w Insurance", "Female Under 18-24 wo Insurance", "Female Under 25-34 w Insurance", "Female Under 25-34 wo Insurance", "Female Under 35-44 w Insurance", "Female Under 35-44 wo Insurance", "Female Under 45-54 w Insurance", "Female Under 45-54 wo Insurance", "Female Under 55-64 w Insurance", "Female Under 55-64 wo Insurance", "fips code",
                       "white under 6 w insurance", "white under 6 wo insurance", "white 6-17 w insurance", "white 6-17 wo insurance", "white 18-24 w insurance", "white 18-24 wo insurance", "white 25-34 w insurance", "white 25-34 wo insurance", "white 35-44 w insurance", "white 35-44 wo insurance", "white 45-54 w insurance", "white 45-54 wo insurance", "white 55-64 w insurance", "white 55-64 wo insurance",
                       "hisp under 6 w insurance", "hisp under 6 wo insurance", "hisp 6-17 w insurance", "hisp 6-17 wo insurance", "hisp 18-24 w insurance", "hisp 18-24 wo insurance", "hisp 25-34 w insurance", "hisp 25-34 wo insurance", "hisp 35-44 w insurance", "hisp 35-44 wo insurance", "hisp 45-54 w insurance", "hisp 45-54 wo insurance", "hisp 55-64 w insurance", "hisp 55-64 wo insurance", "fips code2")
acs2013 <- acs2013[-1,]
acs2013 <- acs2013[-52,]
acs2013[] <- lapply(acs2013[], function(x) as.character(x))
acs2013[2:62] <- lapply(acs2013[2:62], function(x) as.numeric(x))
acs2013 <- left_join(acs2013, state_info, by = "Name")
acs2013$year<- 2013
acs2013$Year<- "2013"
acs2013 <- mutate(acs2013,
                  `total male under 65` = `Male Under 6 w Insurance` + `Male Under 6 wo Insurance` + `Male Under 6-17 w Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 w Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 w Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 w Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 w Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 w Insurance` + `Male Under 55-64 wo Insurance`,
                  `total male insured under 65` = `Male Under 6 w Insurance` + `Male Under 6-17 w Insurance` + `Male Under 18-24 w Insurance` + `Male Under 25-34 w Insurance` + `Male Under 35-44 w Insurance` + `Male Under 45-54 w Insurance` + `Male Under 55-64 w Insurance`,
                  `total male uninsured under 65` = `Male Under 6 wo Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 wo Insurance`,
                  `total female under 65` = `Female Under 6 w Insurance` + `Female Under 6 wo Insurance` + `Female Under 6-17 w Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 w Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 w Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 w Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 w Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 w Insurance` + `Female Under 55-64 wo Insurance`,
                  `total female insured under 65` = `Female Under 6 w Insurance` + `Female Under 6-17 w Insurance` + `Female Under 18-24 w Insurance` + `Female Under 25-34 w Insurance` + `Female Under 35-44 w Insurance` + `Female Under 45-54 w Insurance` + `Female Under 55-64 w Insurance`,
                  `total female uninsured under 65` = `Female Under 6 wo Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 wo Insurance`,
                  `total uninsured under 65` = `total male uninsured under 65` + `total female uninsured under 65`,
                  `percent uninsured under 65` = (`total male uninsured under 65` + `total female uninsured under 65`) / (`total male under 65` + `total female under 65`) * 100,
                  `percent of males under 65 who are insured` = (`total male insured under 65` / `total male under 65`) * 100,
                  `percent of males under 65 who are uninsured` = (`total male uninsured under 65` / `total male under 65`) * 100,
                  `percent of females under 65 who are insured` = (`total female insured under 65` / `total female under 65`) * 100,
                  `percent of females under 65 who are uninsured` = (`total female uninsured under 65` / `total female under 65`) * 100,
                  `percent of uninsured under 65 who are male` = (`total male uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of uninsured under 65 who are female` = (`total female uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of insured under 65 who are male` = (`total male insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `percent of insured under 65 who are female` = (`total female insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `total under 65` = `total male under 65` + `total female under 65`,
                  `total white under 65` = `white under 6 w insurance` + `white under 6 wo insurance`+ `white 6-17 w insurance`+ `white 6-17 wo insurance`+ `white 18-24 w insurance`+ `white 18-24 wo insurance`+ `white 25-34 w insurance`+ `white 25-34 wo insurance`+ `white 35-44 w insurance`+ `white 35-44 wo insurance`+ `white 45-54 w insurance`+ `white 45-54 wo insurance`+ `white 55-64 w insurance`+ `white 55-64 wo insurance`,
                  `total white uninsured under 65` = `white under 6 wo insurance`+ `white 6-17 wo insurance`+ `white 18-24 wo insurance`+ `white 25-34 wo insurance`+ `white 35-44 wo insurance`+ `white 45-54 wo insurance`+ `white 55-64 wo insurance`,
                  `percent white uninsured under 65` = (`total white uninsured under 65` / `total white under 65`) * 100,
                  `total hisp under 65` = `hisp under 6 w insurance`+ `hisp under 6 wo insurance`+ `hisp 6-17 w insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 w insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 w insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 w insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 w insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 w insurance`+ `hisp 55-64 wo insurance`,
                  `total hisp uninsured under 65` = `hisp under 6 wo insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 wo insurance`,
                  `percent hisp uninsured under 65` = (`total hisp uninsured under 65` / `total hisp under 65`) * 100)

# 2014 data - all the same actions as 2012 data
acs2014g <- read_json("https://api.census.gov/data/2014/acs1?get=NAME,B27001_001E,B27001_002E,B27001_004E,B27001_005E,B27001_007E,B27001_008E,B27001_010E,B27001_011E,B27001_013E,B27001_014E,B27001_016E,B27001_017E,B27001_019E,B27001_020E,B27001_022E,B27001_023E,B27001_030E,B27001_032E,B27001_033E,B27001_035E,B27001_036E,B27001_038E,B27001_039E,B27001_041E,B27001_042E,B27001_044E,B27001_045E,B27001_047E,B27001_048E,B27001_050E,B27001_051E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2014r <- read_json("https://api.census.gov/data/2014/acs1?get=NAME,B27001H_003E,B27001H_004E,B27001H_006E,B27001H_007E,B27001H_009E,B27001H_010E,B27001H_012E,B27001H_013E,B27001H_015E,B27001H_016E,B27001H_018E,B27001H_019E,B27001H_021E,B27001H_022E,B27001I_003E,B27001I_004E,B27001I_006E,B27001I_007E,B27001I_009E,B27001I_010E,B27001I_012E,B27001I_013E,B27001I_015E,B27001I_016E,B27001I_018E,B27001I_019E,B27001I_021E,B27001I_022E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2014 <- join(acs2014g, acs2014r, by = "V1")
colnames(acs2014) <- c("Name", "Insurance Status Total", "Total Male", "Male Under 6 w Insurance", "Male Under 6 wo Insurance", "Male Under 6-17 w Insurance", "Male Under 6-17 wo Insurance", "Male Under 18-24 w Insurance", "Male Under 18-24 wo Insurance", "Male Under 25-34 w Insurance", "Male Under 25-34 wo Insurance", "Male Under 35-44 w Insurance", "Male Under 35-44 wo Insurance", "Male Under 45-54 w Insurance", "Male Under 45-54 wo Insurance", "Male Under 55-64 w Insurance", "Male Under 55-64 wo Insurance",
                       "Total Female", "Female Under 6 w Insurance", "Female Under 6 wo Insurance", "Female Under 6-17 w Insurance", "Female Under 6-17 wo Insurance", "Female Under 18-24 w Insurance", "Female Under 18-24 wo Insurance", "Female Under 25-34 w Insurance", "Female Under 25-34 wo Insurance", "Female Under 35-44 w Insurance", "Female Under 35-44 wo Insurance", "Female Under 45-54 w Insurance", "Female Under 45-54 wo Insurance", "Female Under 55-64 w Insurance", "Female Under 55-64 wo Insurance", "fips code",
                       "white under 6 w insurance", "white under 6 wo insurance", "white 6-17 w insurance", "white 6-17 wo insurance", "white 18-24 w insurance", "white 18-24 wo insurance", "white 25-34 w insurance", "white 25-34 wo insurance", "white 35-44 w insurance", "white 35-44 wo insurance", "white 45-54 w insurance", "white 45-54 wo insurance", "white 55-64 w insurance", "white 55-64 wo insurance",
                       "hisp under 6 w insurance", "hisp under 6 wo insurance", "hisp 6-17 w insurance", "hisp 6-17 wo insurance", "hisp 18-24 w insurance", "hisp 18-24 wo insurance", "hisp 25-34 w insurance", "hisp 25-34 wo insurance", "hisp 35-44 w insurance", "hisp 35-44 wo insurance", "hisp 45-54 w insurance", "hisp 45-54 wo insurance", "hisp 55-64 w insurance", "hisp 55-64 wo insurance", "fips code2")
acs2014 <- acs2014[-1,]
acs2014 <- acs2014[-52,]
acs2014[] <- lapply(acs2014[], function(x) as.character(x))
acs2014[2:62] <- lapply(acs2014[2:62], function(x) as.numeric(x))
acs2014 <- left_join(acs2014, state_info, by = "Name")
acs2014$year<- 2014
acs2014$Year<- "2014"
acs2014 <- mutate(acs2014,
                  `total male under 65` = `Male Under 6 w Insurance` + `Male Under 6 wo Insurance` + `Male Under 6-17 w Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 w Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 w Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 w Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 w Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 w Insurance` + `Male Under 55-64 wo Insurance`,
                  `total male insured under 65` = `Male Under 6 w Insurance` + `Male Under 6-17 w Insurance` + `Male Under 18-24 w Insurance` + `Male Under 25-34 w Insurance` + `Male Under 35-44 w Insurance` + `Male Under 45-54 w Insurance` + `Male Under 55-64 w Insurance`,
                  `total male uninsured under 65` = `Male Under 6 wo Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 wo Insurance`,
                  `total female under 65` = `Female Under 6 w Insurance` + `Female Under 6 wo Insurance` + `Female Under 6-17 w Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 w Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 w Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 w Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 w Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 w Insurance` + `Female Under 55-64 wo Insurance`,
                  `total female insured under 65` = `Female Under 6 w Insurance` + `Female Under 6-17 w Insurance` + `Female Under 18-24 w Insurance` + `Female Under 25-34 w Insurance` + `Female Under 35-44 w Insurance` + `Female Under 45-54 w Insurance` + `Female Under 55-64 w Insurance`,
                  `total female uninsured under 65` = `Female Under 6 wo Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 wo Insurance`,
                  `total uninsured under 65` = `total male uninsured under 65` + `total female uninsured under 65`,
                  `percent uninsured under 65` = (`total male uninsured under 65` + `total female uninsured under 65`) / (`total male under 65` + `total female under 65`) * 100,
                  `percent of males under 65 who are insured` = (`total male insured under 65` / `total male under 65`) * 100,
                  `percent of males under 65 who are uninsured` = (`total male uninsured under 65` / `total male under 65`) * 100,
                  `percent of females under 65 who are insured` = (`total female insured under 65` / `total female under 65`) * 100,
                  `percent of females under 65 who are uninsured` = (`total female uninsured under 65` / `total female under 65`) * 100,
                  `percent of uninsured under 65 who are male` = (`total male uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of uninsured under 65 who are female` = (`total female uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of insured under 65 who are male` = (`total male insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `percent of insured under 65 who are female` = (`total female insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `total under 65` = `total male under 65` + `total female under 65`,
                  `total white under 65` = `white under 6 w insurance` + `white under 6 wo insurance`+ `white 6-17 w insurance`+ `white 6-17 wo insurance`+ `white 18-24 w insurance`+ `white 18-24 wo insurance`+ `white 25-34 w insurance`+ `white 25-34 wo insurance`+ `white 35-44 w insurance`+ `white 35-44 wo insurance`+ `white 45-54 w insurance`+ `white 45-54 wo insurance`+ `white 55-64 w insurance`+ `white 55-64 wo insurance`,
                  `total white uninsured under 65` = `white under 6 wo insurance`+ `white 6-17 wo insurance`+ `white 18-24 wo insurance`+ `white 25-34 wo insurance`+ `white 35-44 wo insurance`+ `white 45-54 wo insurance`+ `white 55-64 wo insurance`,
                  `percent white uninsured under 65` = (`total white uninsured under 65` / `total white under 65`) * 100,
                  `total hisp under 65` = `hisp under 6 w insurance`+ `hisp under 6 wo insurance`+ `hisp 6-17 w insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 w insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 w insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 w insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 w insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 w insurance`+ `hisp 55-64 wo insurance`,
                  `total hisp uninsured under 65` = `hisp under 6 wo insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 wo insurance`,
                  `percent hisp uninsured under 65` = (`total hisp uninsured under 65` / `total hisp under 65`) * 100)

# 2015 data - all the same actions as 2012 data
acs2015g <- read_json("https://api.census.gov/data/2015/acs1?get=NAME,B27001_001E,B27001_002E,B27001_004E,B27001_005E,B27001_007E,B27001_008E,B27001_010E,B27001_011E,B27001_013E,B27001_014E,B27001_016E,B27001_017E,B27001_019E,B27001_020E,B27001_022E,B27001_023E,B27001_030E,B27001_032E,B27001_033E,B27001_035E,B27001_036E,B27001_038E,B27001_039E,B27001_041E,B27001_042E,B27001_044E,B27001_045E,B27001_047E,B27001_048E,B27001_050E,B27001_051E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2015r <- read_json("https://api.census.gov/data/2015/acs1?get=NAME,B27001H_003E,B27001H_004E,B27001H_006E,B27001H_007E,B27001H_009E,B27001H_010E,B27001H_012E,B27001H_013E,B27001H_015E,B27001H_016E,B27001H_018E,B27001H_019E,B27001H_021E,B27001H_022E,B27001I_003E,B27001I_004E,B27001I_006E,B27001I_007E,B27001I_009E,B27001I_010E,B27001I_012E,B27001I_013E,B27001I_015E,B27001I_016E,B27001I_018E,B27001I_019E,B27001I_021E,B27001I_022E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2015 <- join(acs2015g, acs2015r, by = "V1")
colnames(acs2015) <- c("Name", "Insurance Status Total", "Total Male", "Male Under 6 w Insurance", "Male Under 6 wo Insurance", "Male Under 6-17 w Insurance", "Male Under 6-17 wo Insurance", "Male Under 18-24 w Insurance", "Male Under 18-24 wo Insurance", "Male Under 25-34 w Insurance", "Male Under 25-34 wo Insurance", "Male Under 35-44 w Insurance", "Male Under 35-44 wo Insurance", "Male Under 45-54 w Insurance", "Male Under 45-54 wo Insurance", "Male Under 55-64 w Insurance", "Male Under 55-64 wo Insurance",
                       "Total Female", "Female Under 6 w Insurance", "Female Under 6 wo Insurance", "Female Under 6-17 w Insurance", "Female Under 6-17 wo Insurance", "Female Under 18-24 w Insurance", "Female Under 18-24 wo Insurance", "Female Under 25-34 w Insurance", "Female Under 25-34 wo Insurance", "Female Under 35-44 w Insurance", "Female Under 35-44 wo Insurance", "Female Under 45-54 w Insurance", "Female Under 45-54 wo Insurance", "Female Under 55-64 w Insurance", "Female Under 55-64 wo Insurance", "fips code",
                       "white under 6 w insurance", "white under 6 wo insurance", "white 6-17 w insurance", "white 6-17 wo insurance", "white 18-24 w insurance", "white 18-24 wo insurance", "white 25-34 w insurance", "white 25-34 wo insurance", "white 35-44 w insurance", "white 35-44 wo insurance", "white 45-54 w insurance", "white 45-54 wo insurance", "white 55-64 w insurance", "white 55-64 wo insurance",
                       "hisp under 6 w insurance", "hisp under 6 wo insurance", "hisp 6-17 w insurance", "hisp 6-17 wo insurance", "hisp 18-24 w insurance", "hisp 18-24 wo insurance", "hisp 25-34 w insurance", "hisp 25-34 wo insurance", "hisp 35-44 w insurance", "hisp 35-44 wo insurance", "hisp 45-54 w insurance", "hisp 45-54 wo insurance", "hisp 55-64 w insurance", "hisp 55-64 wo insurance", "fips code2")
acs2015 <- acs2015[-1,]
acs2015 <- acs2015[-52,]
acs2015[] <- lapply(acs2015[], function(x) as.character(x))
acs2015[2:62] <- lapply(acs2015[2:62], function(x) as.numeric(x))
acs2015 <- left_join(acs2015, state_info, by = "Name")
acs2015$year<- 2015
acs2015$Year<- "2015"
acs2015 <- mutate(acs2015,
                  `total male under 65` = `Male Under 6 w Insurance` + `Male Under 6 wo Insurance` + `Male Under 6-17 w Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 w Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 w Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 w Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 w Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 w Insurance` + `Male Under 55-64 wo Insurance`,
                  `total male insured under 65` = `Male Under 6 w Insurance` + `Male Under 6-17 w Insurance` + `Male Under 18-24 w Insurance` + `Male Under 25-34 w Insurance` + `Male Under 35-44 w Insurance` + `Male Under 45-54 w Insurance` + `Male Under 55-64 w Insurance`,
                  `total male uninsured under 65` = `Male Under 6 wo Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 wo Insurance`,
                  `total female under 65` = `Female Under 6 w Insurance` + `Female Under 6 wo Insurance` + `Female Under 6-17 w Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 w Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 w Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 w Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 w Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 w Insurance` + `Female Under 55-64 wo Insurance`,
                  `total female insured under 65` = `Female Under 6 w Insurance` + `Female Under 6-17 w Insurance` + `Female Under 18-24 w Insurance` + `Female Under 25-34 w Insurance` + `Female Under 35-44 w Insurance` + `Female Under 45-54 w Insurance` + `Female Under 55-64 w Insurance`,
                  `total female uninsured under 65` = `Female Under 6 wo Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 wo Insurance`,
                  `total uninsured under 65` = `total male uninsured under 65` + `total female uninsured under 65`,
                  `percent uninsured under 65` = (`total male uninsured under 65` + `total female uninsured under 65`) / (`total male under 65` + `total female under 65`) * 100,
                  `percent of males under 65 who are insured` = (`total male insured under 65` / `total male under 65`) * 100,
                  `percent of males under 65 who are uninsured` = (`total male uninsured under 65` / `total male under 65`) * 100,
                  `percent of females under 65 who are insured` = (`total female insured under 65` / `total female under 65`) * 100,
                  `percent of females under 65 who are uninsured` = (`total female uninsured under 65` / `total female under 65`) * 100,
                  `percent of uninsured under 65 who are male` = (`total male uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of uninsured under 65 who are female` = (`total female uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of insured under 65 who are male` = (`total male insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `percent of insured under 65 who are female` = (`total female insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `total under 65` = `total male under 65` + `total female under 65`,
                  `total white under 65` = `white under 6 w insurance` + `white under 6 wo insurance`+ `white 6-17 w insurance`+ `white 6-17 wo insurance`+ `white 18-24 w insurance`+ `white 18-24 wo insurance`+ `white 25-34 w insurance`+ `white 25-34 wo insurance`+ `white 35-44 w insurance`+ `white 35-44 wo insurance`+ `white 45-54 w insurance`+ `white 45-54 wo insurance`+ `white 55-64 w insurance`+ `white 55-64 wo insurance`,
                  `total white uninsured under 65` = `white under 6 wo insurance`+ `white 6-17 wo insurance`+ `white 18-24 wo insurance`+ `white 25-34 wo insurance`+ `white 35-44 wo insurance`+ `white 45-54 wo insurance`+ `white 55-64 wo insurance`,
                  `percent white uninsured under 65` = (`total white uninsured under 65` / `total white under 65`) * 100,
                  `total hisp under 65` = `hisp under 6 w insurance`+ `hisp under 6 wo insurance`+ `hisp 6-17 w insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 w insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 w insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 w insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 w insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 w insurance`+ `hisp 55-64 wo insurance`,
                  `total hisp uninsured under 65` = `hisp under 6 wo insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 wo insurance`,
                  `percent hisp uninsured under 65` = (`total hisp uninsured under 65` / `total hisp under 65`) * 100)

# 2016 data
acs2016g <- read_json("https://api.census.gov/data/2016/acs/acs1?get=NAME,B27001_001E,B27001_002E,B27001_004E,B27001_005E,B27001_007E,B27001_008E,B27001_010E,B27001_011E,B27001_013E,B27001_014E,B27001_016E,B27001_017E,B27001_019E,B27001_020E,B27001_022E,B27001_023E,B27001_030E,B27001_032E,B27001_033E,B27001_035E,B27001_036E,B27001_038E,B27001_039E,B27001_041E,B27001_042E,B27001_044E,B27001_045E,B27001_047E,B27001_048E,B27001_050E,B27001_051E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2016r <- read_json("https://api.census.gov/data/2016/acs/acs1?get=NAME,B27001H_003E,B27001H_004E,B27001H_006E,B27001H_007E,B27001H_009E,B27001H_010E,B27001H_012E,B27001H_013E,B27001H_015E,B27001H_016E,B27001H_018E,B27001H_019E,B27001H_021E,B27001H_022E,B27001I_003E,B27001I_004E,B27001I_006E,B27001I_007E,B27001I_009E,B27001I_010E,B27001I_012E,B27001I_013E,B27001I_015E,B27001I_016E,B27001I_018E,B27001I_019E,B27001I_021E,B27001I_022E&for=state:*&key=17c33afc69e74a76256559f11768a4005763e816", simplifyVector = TRUE) %>% as.data.frame()
acs2016 <- join(acs2016g, acs2016r, by = "V1")
colnames(acs2016) <- c("Name", "Insurance Status Total", "Total Male", "Male Under 6 w Insurance", "Male Under 6 wo Insurance", "Male Under 6-17 w Insurance", "Male Under 6-17 wo Insurance", "Male Under 18-24 w Insurance", "Male Under 18-24 wo Insurance", "Male Under 25-34 w Insurance", "Male Under 25-34 wo Insurance", "Male Under 35-44 w Insurance", "Male Under 35-44 wo Insurance", "Male Under 45-54 w Insurance", "Male Under 45-54 wo Insurance", "Male Under 55-64 w Insurance", "Male Under 55-64 wo Insurance",
                       "Total Female", "Female Under 6 w Insurance", "Female Under 6 wo Insurance", "Female Under 6-17 w Insurance", "Female Under 6-17 wo Insurance", "Female Under 18-24 w Insurance", "Female Under 18-24 wo Insurance", "Female Under 25-34 w Insurance", "Female Under 25-34 wo Insurance", "Female Under 35-44 w Insurance", "Female Under 35-44 wo Insurance", "Female Under 45-54 w Insurance", "Female Under 45-54 wo Insurance", "Female Under 55-64 w Insurance", "Female Under 55-64 wo Insurance", "fips code",
                       "white under 6 w insurance", "white under 6 wo insurance", "white 6-17 w insurance", "white 6-17 wo insurance", "white 18-24 w insurance", "white 18-24 wo insurance", "white 25-34 w insurance", "white 25-34 wo insurance", "white 35-44 w insurance", "white 35-44 wo insurance", "white 45-54 w insurance", "white 45-54 wo insurance", "white 55-64 w insurance", "white 55-64 wo insurance",
                       "hisp under 6 w insurance", "hisp under 6 wo insurance", "hisp 6-17 w insurance", "hisp 6-17 wo insurance", "hisp 18-24 w insurance", "hisp 18-24 wo insurance", "hisp 25-34 w insurance", "hisp 25-34 wo insurance", "hisp 35-44 w insurance", "hisp 35-44 wo insurance", "hisp 45-54 w insurance", "hisp 45-54 wo insurance", "hisp 55-64 w insurance", "hisp 55-64 wo insurance", "fips code2")
acs2016 <- acs2016[-1,]
acs2016 <- acs2016[-52,]
acs2016[] <- lapply(acs2016[], function(x) as.character(x))
acs2016[2:62] <- lapply(acs2016[2:62], function(x) as.numeric(x))
acs2016 <- left_join(acs2016, state_info, by = "Name")
acs2016$year<- 2016
acs2016$Year<- "2016"
acs2016 <- mutate(acs2016,
                  `total male under 65` = `Male Under 6 w Insurance` + `Male Under 6 wo Insurance` + `Male Under 6-17 w Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 w Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 w Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 w Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 w Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 w Insurance` + `Male Under 55-64 wo Insurance`,
                  `total male insured under 65` = `Male Under 6 w Insurance` + `Male Under 6-17 w Insurance` + `Male Under 18-24 w Insurance` + `Male Under 25-34 w Insurance` + `Male Under 35-44 w Insurance` + `Male Under 45-54 w Insurance` + `Male Under 55-64 w Insurance`,
                  `total male uninsured under 65` = `Male Under 6 wo Insurance` + `Male Under 6-17 wo Insurance` + `Male Under 18-24 wo Insurance` + `Male Under 25-34 wo Insurance` + `Male Under 35-44 wo Insurance` + `Male Under 45-54 wo Insurance` + `Male Under 55-64 wo Insurance`,
                  `total female under 65` = `Female Under 6 w Insurance` + `Female Under 6 wo Insurance` + `Female Under 6-17 w Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 w Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 w Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 w Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 w Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 w Insurance` + `Female Under 55-64 wo Insurance`,
                  `total female insured under 65` = `Female Under 6 w Insurance` + `Female Under 6-17 w Insurance` + `Female Under 18-24 w Insurance` + `Female Under 25-34 w Insurance` + `Female Under 35-44 w Insurance` + `Female Under 45-54 w Insurance` + `Female Under 55-64 w Insurance`,
                  `total female uninsured under 65` = `Female Under 6 wo Insurance` + `Female Under 6-17 wo Insurance` + `Female Under 18-24 wo Insurance` + `Female Under 25-34 wo Insurance` + `Female Under 35-44 wo Insurance` + `Female Under 45-54 wo Insurance` + `Female Under 55-64 wo Insurance`,
                  `total uninsured under 65` = `total male uninsured under 65` + `total female uninsured under 65`,
                  `percent uninsured under 65` = (`total male uninsured under 65` + `total female uninsured under 65`) / (`total male under 65` + `total female under 65`) * 100,
                  `percent of males under 65 who are insured` = (`total male insured under 65` / `total male under 65`) * 100,
                  `percent of males under 65 who are uninsured` = (`total male uninsured under 65` / `total male under 65`) * 100,
                  `percent of females under 65 who are insured` = (`total female insured under 65` / `total female under 65`) * 100,
                  `percent of females under 65 who are uninsured` = (`total female uninsured under 65` / `total female under 65`) * 100,
                  `percent of uninsured under 65 who are male` = (`total male uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of uninsured under 65 who are female` = (`total female uninsured under 65` / (`total male uninsured under 65` + `total female uninsured under 65`)) * 100,
                  `percent of insured under 65 who are male` = (`total male insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `percent of insured under 65 who are female` = (`total female insured under 65` / (`total male insured under 65` + `total female insured under 65`)) * 100,
                  `total under 65` = `total male under 65` + `total female under 65`,
                  `total white under 65` = `white under 6 w insurance` + `white under 6 wo insurance`+ `white 6-17 w insurance`+ `white 6-17 wo insurance`+ `white 18-24 w insurance`+ `white 18-24 wo insurance`+ `white 25-34 w insurance`+ `white 25-34 wo insurance`+ `white 35-44 w insurance`+ `white 35-44 wo insurance`+ `white 45-54 w insurance`+ `white 45-54 wo insurance`+ `white 55-64 w insurance`+ `white 55-64 wo insurance`,
                  `total white uninsured under 65` = `white under 6 wo insurance`+ `white 6-17 wo insurance`+ `white 18-24 wo insurance`+ `white 25-34 wo insurance`+ `white 35-44 wo insurance`+ `white 45-54 wo insurance`+ `white 55-64 wo insurance`,
                  `percent white uninsured under 65` = (`total white uninsured under 65` / `total white under 65`) * 100,
                  `total hisp under 65` = `hisp under 6 w insurance`+ `hisp under 6 wo insurance`+ `hisp 6-17 w insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 w insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 w insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 w insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 w insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 w insurance`+ `hisp 55-64 wo insurance`,
                  `total hisp uninsured under 65` = `hisp under 6 wo insurance`+ `hisp 6-17 wo insurance`+ `hisp 18-24 wo insurance`+ `hisp 25-34 wo insurance`+ `hisp 35-44 wo insurance`+ `hisp 45-54 wo insurance`+ `hisp 55-64 wo insurance`,
                  `percent hisp uninsured under 65` = (`total hisp uninsured under 65` / `total hisp under 65`) * 100)

# combine tables
all <- bind_rows(acs2012, acs2013, acs2014, acs2015, acs2016)

#agg hisp per year
agghispyr <- aggregate(`total hisp under 65` ~ year, all, sum)
agghispunyr <- aggregate(`total hisp uninsured under 65` ~ year, all, sum)
joinhisp <- join(agghispyr, agghispunyr)
joinhisp <- mutate(joinhisp,
                   `percent hisp uninsured year` = `total hisp uninsured under 65`/`total hisp under 65`)
drops <- c("total hisp under 65", "total hisp uninsured under 65")
hisp_uninsured_rates_json <- toJSON(joinhisp[ , !(names(joinhisp) %in% drops)])

write_json(hisp_uninsured_rates_json, "C:\\Users\\Alix\ Gates\\Documents\\GitHub\\dynamic_viz\\hisp_uninsured_rates_json.json")