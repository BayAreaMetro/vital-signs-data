#commute-share-places-tracts-five-year.R
#Script to convert Census APIs into commute share tables for Bay Area places and tracts
#SI
#01/11/17
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
source1="B08301_ACS14_5YR"
source2="B08601_ACS14_5YR"
timesource1="B08013_ACS14_5YR"
timesource2="B08136_ACS14_5YR"
work_timesource1="B08536_ACS14_5YR"
work_source1="B08601_ACS14_5YR"


# Set up destination to save

share_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/1A_Transportation/T1_Commute Mode Share (Home)/",ACS_year,"/",ACS_year,"_")
work_share_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/1A_Transportation/T2_Commute Mode Share (Work)/",ACS_year,"/",ACS_year,"_")
time_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/1A_Transportation/T3_Commute Time (Home)/",ACS_year,"/",ACS_year,"_")
work_time_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/1A_Transportation/T4_Commute Time (Work)",ACS_year,"/",ACS_year,"_")

#City_csv="5Year_City.csv"
#Tract_csv="5Year_Tract.csv"


# Census API Key

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"

# End variables to edit
################################################################
# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(httr)
library(reshape2)


# API Calls

# Residence Geographies

mode_city_url= paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&in=state:",state,"&for=place:",city,"&key=",key)

#Now Work

mode_city_work_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08601_001E,B08601_002E,B08601_003E,B08601_004E,B08601_010E,B08601_019E,B08601_018E,B08601_016E,B08601_017E,B08601_020E,B08601_021E&in=state:",state,"&for=place:",city,"&key=",key)

# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."
# Geography_fields = # of geo fields at end of API call (to keep non-numeric) = metro(2), county(2), place(3),tract(3) 

f.data <- function(url,geography_fields){  
  furl <- content(GET(url))
  for (i in 1:length(furl)){
    if (i==1) header <- furl [[i]]
    if (i==2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      output_data <- data.frame(temp, stringsAsFactors=FALSE)
      names (output_data) <- header
    }
    if (i>2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      tempdf <- data.frame(temp, stringsAsFactors=FALSE)
      names (tempdf) <- header
      output_data <- rbind (output_data,tempdf)
    }
  }
    for(i in 2:(ncol(output_data)-geography_fields)) {
    output_data[,i] <- as.numeric(output_data[,i])
  }
  return (output_data)
}

city_commute_residence <- f.data(mode_city_url,2)

city_commute_residence2 <- cbind(city_commute_residence,ACS_year,source1)
names(city_commute_residence2) <-  c("Residence_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "State", "City_ID", "Year", "Source")


f.geography <- function (geography) {
  geography <- geography %>% mutate(
    Othertot = as.numeric(Taxi) + as.numeric(Motorcycle) + as.numeric(Other),
    Other_w_Bike = as.numeric(Othertot)+as.numeric(Bike),
    DriveTot_Est = as.numeric(Drive_Total) / as.numeric(Workers_Est),
    DriveAlone_Est = as.numeric(DAWorkers_Est) / as.numeric(Workers_Est),
    Carpool_Est = as.numeric(CPWorkers_Est) / as.numeric(Workers_Est),
    Transit_Est = as.numeric(PTWorkers_Est) / as.numeric(Workers_Est),
    Walk_Est = as.numeric(Walk) / as.numeric(Workers_Est),
    Other_w_Bike_Est = as.numeric(Other_w_Bike) / as.numeric(Workers_Est),
    Bike_Est = as.numeric(Bike) / as.numeric(Workers_Est),
    Other_Est = as.numeric(Othertot) / as.numeric(Workers_Est),
    Telework_Est = as.numeric(AtHome) / as.numeric(Workers_Est)
  )
}

city_residence_mode <- f.geography(city_commute_residence2)
