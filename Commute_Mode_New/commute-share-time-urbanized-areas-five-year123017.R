#commute-share-time-urbanized-areas-five-year123017.R
#Script to convert Census APIs into commute share tables for Bay Area urbanized areas
#SI
#12/30/17
################################################################
# Variables to edit - Begin

ACS_year="2016"
ACS_product="5"
urbanized="02683,19504,28657,33328,50527,61057,68887,78904,79039,79498,89866,90028"

#02683 Antioch
#19504 Concord
#28657 Fairfield
#33328 Gilroy--Morgan Hill
#50527 Livermore 
#61057 Napa
#68887 Petaluma
#78904 San Francisco - Oakland
#79039 San Jose
#79498 Santa Rosa
#89866 Vacaville
#90028 Vallejo

source="B08301_ACS16_5YR"

# Set up destination to save

share_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T1-2_Commute Mode Choice/T1_Commute Mode Share (Home)/",ACS_year,"/",ACS_year,"_")


# Census API Key

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"

# End variables to edit
################################################################
# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(httr)
library(reshape2)

# Create variable indices for later conversion

index <- c("DriveTot_Est","DriveAlone_Est","Carpool_Est","Transit_Est","Walk_Est","Other_w_Bike_Est","Bike_Est","Other_Est","Telework_Est", "OverallTime_Est", "DATime_Est", "CPTime_Est", "PTTime_Est") # Transport_Mode index for later reference
values <- c("Share Total Auto","Share Drive Alone","Share Carpool", "Share Transit", "Share Walk", "Share Other With Bike","Share Bike", "Share Other", "Share Work at Home", "Total Mean Travel Time", "Drive Alone Mean Travel Time", "Carpool Mean Travel Time", "Transit Mean Travel Time") #Transport_Mode_Label values for later


# API Calls

# Urbanized areas. Years prior to 2016 have a different url (one less "acs" in the string)

#mode_residence_ua_url= paste0("https://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&for=urban%20area:",urbanized,"&key=",key)
mode_residence_ua_url= paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&for=urban%20area:",urbanized,"&key=",key)



# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."
# Geography_fields = # of geo fields at end of API call (to keep non-numeric) = metro(2), county(2), place(2),tract(3) 

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

mode_residence_urbanized <- f.data(mode_residence_ua_url,1)

mode_residence_urbanized2 <- cbind(mode_residence_urbanized,ACS_year,source)
names(mode_residence_urbanized2) <-  c("Residence_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "UZA_ID", "Year", "Source")
mode_residence_urbanized3 <- mode_residence_urbanized2 %>%
  mutate(Urbanized_Area = sapply(strsplit(Residence_Geo,','),function(x) x[1]))

f.shares <- function (input_table) {
  input_table <- input_table %>% mutate(
    Othertot = Taxi + Motorcycle + Other,
    Other_w_Bike = Othertot + Bike,
    DriveTot_Est = Drive_Total / Workers_Est,
    DriveAlone_Est = DAWorkers_Est / Workers_Est,
    Carpool_Est = CPWorkers_Est / Workers_Est,
    Transit_Est = PTWorkers_Est / Workers_Est,
    Walk_Est = Walk / Workers_Est,
    Other_w_Bike_Est = Other_w_Bike / Workers_Est,
    Bike_Est = Bike / Workers_Est,
    Other_Est = Othertot / Workers_Est,
    Telework_Est = AtHome / Workers_Est
  )
}



#Melt data into proper shape for exporting CSVs

mode_residence_urbanized_share <- f.shares(mode_residence_urbanized3) %>%
  select (Urbanized_Area, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)


mode_residence_urbanized_share_melt_2016 <- melt(mode_residence_urbanized_share, 
                                  id.vars=c("Urbanized_Area","Year", "Workers_Est", "Source"),
                                  variable.name="Transport_Mode",
                                  value.name="Share"
) %>% mutate (
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>% 
  select (Urbanized_Area, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share, Source)


rm(mode_residence_urbanized,mode_residence_urbanized2,mode_residence_urbanized3,mode_residence_urbanized_share)

# Concantenate multiple years. Can remove the below step if running a single year.

mode_residence_urbanized_share_melt<-rbind(mode_residence_urbanized_share_melt_2010,mode_residence_urbanized_share_melt_2011,mode_residence_urbanized_share_melt_2012,mode_residence_urbanized_share_melt_2013,mode_residence_urbanized_share_melt_2014,mode_residence_urbanized_share_melt_2015,mode_residence_urbanized_share_melt_2016)


# Export CSV

write.csv(mode_residence_urbanized_share_melt, paste0(share_output_csv, "5Year_Urbanized_Mode_Share.csv"), row.names = FALSE, quote = T)

