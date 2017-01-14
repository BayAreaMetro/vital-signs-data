#commute-share-places-tracts-five-year.R
#Script to convert Census APIs into commute share tables for Bay Area places and tracts
#SI
#01/11/17
#Both an aggregate time for all modes and a sub-mode table are used because suppression means only totals available in some geos.
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
source1="B08301_ACS15_5YR"
source2="B08601_ACS15_5YR"
timesource1="B08013_ACS15_5YR"
timesource2="B08136_ACS15_5YR"
work_timesource1="B08536_ACS15_5YR"
work_source1="B08601_ACS15_5YR"


# Set up destination to save

share_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T1_Commute Mode Share (Home)/",ACS_year,"/",ACS_year,"_")
work_share_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T2_Commute Mode Share (Work)/",ACS_year,"/",ACS_year,"_")
time_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T3_Commute Time (Home)/",ACS_year,"/",ACS_year,"_")
work_time_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T4_Commute Time (Work)",ACS_year,"/",ACS_year,"_")

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

# Create variable indices for later conversion

index <- c("DriveTot_Est","DriveAlone_Est","Carpool_Est","Transit_Est","Walk_Est","Other_w_Bike_Est","Bike_Est","Other_Est","Telework_Est", "OverallTime_Est", "DATime_Est", "CPTime_Est", "PTTime_Est") # Transport_Mode index for later reference
values <- c("Share Total Auto","Share Drive Alone","Share Carpool", "Share Transit", "Share Walk", "Share Other With Bike","Share Bike", "Share Other", "Share Work at Home", "Total Mean Travel Time", "Drive Alone Mean Travel Time", "Carpool Mean Travel Time", "Transit Mean Travel Time") #Transport_Mode_Label values for later


# API Calls

# Residence Geographies
# Places

mode_residence_place_url= paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&in=state:",state,"&for=place:",city,"&key=",key)

# Tracts

mode_residence_tract_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&in=state:",state,"+county:",county,"&for=tract:*&key=",key)

# Now Work
# Places

mode_work_place_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08601_001E,B08601_002E,B08601_003E,B08601_004E,B08601_010E,B08601_019E,B08601_018E,B08601_016E,B08601_017E,B08601_020E,B08601_021E&in=state:",state,"&for=place:",city,"&key=",key)

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

mode_residence_place <- f.data(mode_residence_place_url,2)

mode_residence_place2 <- cbind(mode_residence_place,ACS_year,source1)
names(mode_residence_place2) <-  c("Residence_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "State", "City_ID", "Year", "Source")


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

mode_residence_place3 <- f.shares(mode_residence_place2) %>% mutate(
  Id = paste0("1600000US06",City_ID),
  Id2 = paste0("6",City_ID),
  Residence_Geo=sapply(strsplit(as.character(Residence_Geo),'city,'),function(x) x[1]),
  Residence_Geo=sapply(strsplit(as.character(Residence_Geo),'town,'),function(x) x[1])) 

#Melt data into proper shape for exporting CSVs

mode_residence_place_share <- mode_residence_place3 %>%
  select (Id, Id2, Residence_Geo, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)


mode_residence_place_share_melt <- melt(mode_residence_place_share, 
                                  id.vars=c("Id", "Id2", "Residence_Geo","Year", "Workers_Est", "Source"),
                                  variable.name="Transport_Mode",
                                  value.name="Share"
)

mode_residence_place_share_melt$Transport_Mode_Label <- values[match(mode_residence_place_share_melt$Transport_Mode, index)]
mode_residence_place_share_melt2 <- mode_residence_place_share_melt %>% 
  select (Id, Id2, Residence_Geo, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share, Source)

#Now tracts

mode_residence_tract <- f.data(mode_residence_tract_url,3)
mode_residence_tract2 <- cbind(mode_residence_tract, ACS_year,source1)
names(mode_residence_tract2) <-  c("Residence_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "State", "County_Code", "Census_Tract", "Year", "Source")

mode_residence_tract3 <- mode_residence_tract2 %>% mutate(
Id = paste0 ("1400000US", State, County_Code, Census_Tract),
Id2 = paste0 (as.numeric(state),County_Code, Census_Tract),
County = sapply(strsplit(Residence_Geo,','),function(x) x[2]),
Tract = sapply(strsplit(Residence_Geo,','),function(x) x[1])
)

mode_residence_tract4 <- f.shares(mode_residence_tract3)

#Melting Tracts for Export

mode_residence_tract_share <- mode_residence_tract4 %>% 
  select (Id, Id2, County, Tract, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)


mode_residence_tract_share_melt <- melt(mode_residence_tract_share, 
                                        id.vars=c("Id", "Id2","County", "Tract", "Year", "Workers_Est", "Source"),
                                        variable.name="Transport_Mode",
                                        value.name="Share"
)

mode_residence_tract_share_melt2 <- mode_residence_tract_share_melt %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Id, Id2, County, Tract, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share, Source)

# Work

mode_work_place <- f.data(mode_work_place_url,2) 
names(mode_work_place) <- c("Workplace_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome" , "State", "Place")

mode_work_place2 <- mode_work_place %>% mutate(
  Id = paste0("1600000US06",Place),
  Id2 = paste0(as.numeric(State),Place),
  Year = ACS_year,
  Source = source2
)

mode_work_place_share <- f.shares(mode_work_place2) %>%
select (Id, Id2, Workplace_Geo, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)

mode_work_place_share_melt <- melt(mode_work_place_share, 
                             id.vars=c("Id", "Id2", "Workplace_Geo","Year", "Workers_Est", "Source"),
                             variable.name="Transport_Mode",
                             value.name="Share"
)

mode_work_place_share_melt$Transport_Mode_Label <- values[match(mode_work_place_share_melt$Transport_Mode, index)]

mode_work_place_share_melt2 <- mode_work_place_share_melt %>%
  select (Id, Id2, Workplace_Geo, Year, Workers_Est, Transport_Mode, Transport_Mode_Label, Share, Source)

# Export CSVs

write.csv(mode_residence_place_share_melt2, paste0(share_output_csv, "5Year_City_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_residence_tract_share_melt2, paste0(share_output_csv, "5Year_City_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_work_place_share_melt2, paste0(work_share_output_csv, "5Year_City_Mode_Share.csv"), row.names = FALSE, quote = T)

# Travel Time Total API Calls

timeall_city_residence_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08013_001E&in=state:",state,"&for=place:",city,"&key=", key)
timesub_city_residence_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08136_003E,B08136_004E,B08136_007E&in=state:",state,"&for=place:",city,"&key=", key)
time_city_work_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08536_001E,B08536_003E,B08536_004E,B08536_007E&in=state:",state,"&for=place:",city,"&key=", key)

# Calculate Travel Times

timeall_city_residence <- f.data(timeall_city_residence_url,2) %>% mutate(
  Residence_Geo=sapply(strsplit(as.character(NAME),'city,'),function(x) x[1]),
  Residence_Geo=sapply(strsplit(as.character(Residence_Geo),'town,'),function(x) x[1])) %>%
  rename(Aggregate_Minutes=B08013_001E) %>%
  select(Residence_Geo,Aggregate_Minutes)

timesub_city_residence <- f.data(timesub_city_residence_url,2) %>% mutate(
  Residence_Geo=sapply(strsplit(as.character(NAME),'city,'),function(x) x[1]),
  Residence_Geo=sapply(strsplit(as.character(Residence_Geo),'town,'),function(x) x[1])) %>%
  rename(Drive_Alone_Aggregate=B08136_003E,Carpool_Aggregate=B08136_004E,Transit_Aggregate=B08136_007E) %>%
  select(Residence_Geo,Drive_Alone_Aggregate,Carpool_Aggregate,Transit_Aggregate)


mode_residence_place4 <- mode_residence_place3 %>% 
  select(Id,Id2,Residence_Geo, Year, Workers_Est, DAWorkers_Est, CPWorkers_Est, PTWorkers_Est, AtHome)

alltime_city_residence <- merge(mode_residence_place4,timeall_city_residence,by="Residence_Geo") %>% mutate(
  NotHome = Workers_Est-AtHome,
  OverallTime_Est = Aggregate_Minutes / NotHome,
  Source1 = timesource1,
  Source2 = source1) %>%
  select(Id,Id2,Residence_Geo,Year,Workers_Est,OverallTime_Est,Source1,Source2)
  
  

alltime_city_residence_melt <- melt(alltime_city_residence, 
                       id.vars=c("Id","Id2","Residence_Geo","Year", "Workers_Est", "Source1", "Source2"),
                       variable.name="Transport_Mode",
                       value.name="Time_Est"
)

alltime_city_residence_melt2 <- alltime_city_residence_melt %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select(Id,Id2,Residence_Geo,Year,Workers_Est,Transport_Mode,Transport_Mode_Label,Time_Est,Source1,Source2)

subtime_city_residence <- merge(mode_residence_place4,timesub_city_residence,by="Residence_Geo") %>% mutate(
  DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est,
  CPTime_Est = Carpool_Aggregate / CPWorkers_Est,
  PTTime_Est = Transit_Aggregate / PTWorkers_Est,
  Source1=timesource2,
  Source2=source1) %>%
  select (Id,Id2,Residence_Geo, Year, Workers_Est, DATime_Est, CPTime_Est, PTTime_Est, Source1, Source2)


subtime_city_residence_melt <- melt(subtime_city_residence, 
                       id.vars=c("Id","Id2","Residence_Geo","Year", "Workers_Est", "Source1", "Source2"),
                       variable.name="Transport_Mode",
                       value.name="Time_Est"
)

subtime_city_residence_melt2 <- subtime_city_residence_melt %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select(Id,Id2,Residence_Geo,Year,Workers_Est,Transport_Mode,Transport_Mode_Label,Time_Est,Source1,Source2)

time_city_residence_melt <- rbind (alltime_city_residence_melt2,subtime_city_residence_melt2)

# Export CSV

write.csv(time_city_residence_melt, paste0(time_output_csv, "5Year_City_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)

# Now Work

mode_work_place3 <- mode_work_place2 %>%
  select(Id,Id2,Workplace_Geo, Year, Workers_Est,DAWorkers_Est, CPWorkers_Est, PTWorkers_Est, AtHome)

time_city_work <- f.data(time_city_work_url,2) %>%
  rename(Workplace_Geo=NAME,Total_Aggregate=B08536_001E,Drive_Alone_Aggregate=B08536_003E,Carpool_Aggregate=B08536_004E,Transit_Aggregate=B08536_007E) %>%
  select(Workplace_Geo,Total_Aggregate,Drive_Alone_Aggregate,Carpool_Aggregate,Transit_Aggregate)

work_city_aggregate_frame <- work_city_aggregate_frame %>%
  mutate (NotHome = Workers_Est-AtHome) %>%
  mutate (OverallTime_Est = Total_Aggregate / NotHome) %>%
  mutate (DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est) %>%
  mutate (CPTime_Est = Carpool_Aggregate / CPWorkers_Est) %>%
  mutate (PTTime_Est = Transit_Aggregate / PTWorkers_Est)



```


```{r data - melt data frame for work city travel time data for submodes}
work_city_mean_frame <- work_city_aggregate_frame %>%
  select (Id,Id2,Workplace_Geo, Year, Workers_Est, OverallTime_Est, DATime_Est, CPTime_Est, PTTime_Est, Source1, Source2)


time_city_work_url

work_city_aggregate_frame <- work_city_aggregate_frame %>%
  mutate (NotHome = Workers_Est-AtHome) %>%
  mutate (OverallTime_Est = Total_Aggregate / NotHome) %>%
  mutate (DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est) %>%
  mutate (CPTime_Est = Carpool_Aggregate / CPWorkers_Est) %>%
  mutate (PTTime_Est = Transit_Aggregate / PTWorkers_Est)

work_city_mean_melt <- melt(work_city_mean_frame, 
                            id.vars=c("Id","Id2","Workplace_Geo","Year", "Workers_Est", "Source1", "Source2"),
                            variable.name="Transport_Mode",
                            value.name="Time_Est"
)

work_city_mean_melt$Transport_Mode_Label <- values[match(work_city_mean_melt$Transport_Mode, index)]

work_city_mean_melt <- work_city_mean_melt[c(1,2,3,4,5,8,10,9,6,7)]


write.csv(work_city_mean_melt , paste0(time_output_csv, "5Year_City_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
