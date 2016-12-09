#poverty-places-tracts-five-year.R
#Script to convert Census APIs into poverty tables for Bay Area places and tracts
#SI
#08/18/16
################################################################
# Variables to edit - Begin

ACS_year="2015"
ACS_product="5"
county="01,13,41,55,75,81,85,95,97"
state="06"
city=paste0("00562,00674,01640,02252,03092,05108,05164,05290,06000,08142,08310,09066,",
            "09892,10345,13882,14190,14736,16000,16462,16560,17610,17918,17988,",
            "19402,20018,20956,21796,22594,23168,23182,25338,26000,29504,31708,",
            "33000,33056,33308,33798,39122,40438,41992,43280,43294,44112,46114,",
            "46870,47710,47486,47766,48956,49187,49278,49670,50258,50916,52582,",
            "53000,53070,54232,54806,55282,56784,56938,57288,57456,57764,57792,",
            "58380,60102,60620,60984,62546,62980,64434,65028,65070,67000,68000,",
            "68084,68252,68294,68364,68378,69084,70098,70280,70364,70770,72646,",
            "73262,64140,75630,77000,78666,81204,81554,81666,83346,85922,86440,86930")
source="ACS15_C17002_5YR"


# Set up destination to save

output_csv=paste0("C:/Users/sisrae/Box Sync/Data/3_Equity/EQ4_Poverty (former EC11)/2015/",ACS_year,"_")
City_csv="5Year_City.csv"
Tract_csv="5Year_Tract.csv"


# Census API Key

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"

# End variables to edit
################################################################

# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(httr)
library(reshape2)

# Import census API data for poverty, Bay Area counties and specified metros

city_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,
              "?get=NAME,C17002_001E,C17002_002E,C17002_003E,C17002_008E&in=state:06&for=place:",
              city,"&key=",key)

tract_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,
                    "?get=NAME,C17002_001E,C17002_002E,C17002_003E,C17002_008E&in=state:",state,
                    "+county:",county,"&for=tract:*&key=",key)


# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."

f.poverty <- function(url){  
  furl <- content(GET(url))
  for (i in 1:length(furl)){
    if (i==1) header <- furl [[i]]
    if (i==2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      poverty <- data.frame(temp, stringsAsFactors=FALSE)
      names (poverty) <- header
    }
    if (i>2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      tempdf <- data.frame(temp, stringsAsFactors=FALSE)
      names (tempdf) <- header
      poverty <- rbind (poverty,tempdf)
    }
  }
  return (poverty)
}

#Bring in API data

city_poverty_in <- f.poverty(city_url)
tract_poverty_in <- f.poverty(tract_url)

#Convert all columns to numeric format (don't covert geography columns at the end)

for(i in 2:(ncol(city_poverty_in)-2)) {
  city_poverty_in[,i] <- as.numeric(city_poverty_in[,i])
}

for(i in 2:(ncol(tract_poverty_in)-3))  {
  tract_poverty_in[,i] <- as.numeric(tract_poverty_in[,i])
}

# Create County categories

city_poverty <- city_poverty_in %>% mutate(
  GeoID = paste0("1400000US06",place),
  GeoID2 = paste0("6",place),
  temp = sapply((strsplit(as.character(NAME),'city,')),function(x) x[1]),
  City = sapply((strsplit(as.character(temp),'town,')),function(x) x[1]),
  PovPop = C17002_001E,
  Pov100 = C17002_002E + C17002_003E,
  Pov200 = C17002_001E - C17002_008E,
  Povper100 = Pov100/PovPop,
  Povper200 = Pov200/PovPop,
  Year = ACS_year,
  Source = source
  ) %>%
  select(GeoID,GeoID2,City,PovPop,Pov100,Pov200,Povper100, Povper200,Year,Source)

# Now poverty by tract

tract_poverty <- tract_poverty_in %>% mutate(
  Id = paste0("1400000US06",county,tract),
  Id2 = paste0("6",county,tract),
  Geography = NAME,
  Year = ACS_year,
  PovPop = C17002_001E,
  Pov100 = C17002_002E + C17002_003E,
  Pov200 = C17002_001E - C17002_008E,
  Povper100 = Pov100/PovPop,
  Povper200 = Pov200/PovPop,
  Source = source
  ) %>%
  select(Id,Id2,Geography,Year,PovPop,Pov100,Pov200,Povper100,Povper200,Source)

# Write out CSV 

write.csv(city_poverty, paste0(output_csv, City_csv), row.names = FALSE, quote = T)
write.csv(tract_poverty, paste0(output_csv, Tract_csv), row.names = FALSE, quote = T)