#commute-share-time-metros-counties-one-year021017.R
#Script to convert Census APIs into commute share tables for national metros and Bay Area counties
#SI
#01/03/18
#Both an aggregate time for all modes and a sub-mode table are used because suppression means only totals available in some geos.
################################################################
# Variables to edit - Begin

ACS_year="2016"
ACS_product="1"
county="001,013,041,055,075,081,085,095,097"
state="06"
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"
source1="B08301_ACS16_1YR"
work_source1="B08601_ACS16_1YR"
timesource1="C08136_ACS16_1YR"
work_timesource1="C08536_ACS16_1YR"
bins_all_source="B08303_ACS_16_1YR"
bins_mode_source="C08134_ACS_16_1YR"


# Set up destination to save

share_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T1-2_Commute Mode Choice/T1_Commute Mode Share (Home)/",ACS_year,"/",ACS_year,"_")
work_share_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T1-2_Commute Mode Choice/T2_Commute Mode Share (Work)/",ACS_year,"/",ACS_year,"_")
time_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T3-4_Commute Time/T3_Commute Time (Home)/",ACS_year,"/",ACS_year,"_")
work_time_output_csv=paste0("C:/Users/sisrae/Box Sync/Data/1A_Transportation/T3-4_Commute Time/T4_Commute Time (Work)/",ACS_year,"/",ACS_year,"_")

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

index <- c("DriveTot_Est","DriveAlone_Est","Carpool_Est","Transit_Est","Walk_Est","Other_w_Bike_Est","Bike_Est","Other_Est","Telework_Est", "OverallTime_Est", "DATime_Est", "CPTime_Est", "PTTime_Est","Tot_lt15_Est","Tot_15to30_Est","Tot_30to45_Est","Tot_45to60_Est","Tot_60to90_Est","Tot_gt90_Est","DA_lt15_Est","DA_15to30_Est","DA_30to45_Est","DA_45to60_Est","DA_gt60_Est","CP_lt15_Est","CP_15to30_Est","CP_30to45_Est","CP_45to60_Est","CP_gt60_Est","Transit_lt15_Est","Transit_15to30_Est","Transit_30to45_Est","Transit_45to60_Est","Transit_gt60_Est") # Transport_Mode index for later reference
values <- c("Share Total Auto","Share Drive Alone","Share Carpool", "Share Transit", "Share Walk", "Share Other With Bike","Share Bike", "Share Other", "Share Work at Home", "Total Mean Travel Time", "Drive Alone Mean Travel Time", "Carpool Mean Travel Time", "Transit Mean Travel Time","Share Total Less Than 15 Minutes","Share Total 15 to 29 Minutes","Share Total 30 to 44 Minutes", "Share Total 45 to 59 Minutes","Share Total 60 to 89 Minutes", "Share Total 90 Minutes and Greater","Share Drive Alone Less Than 15 Minutes","Share Drive Alone 15 to 29 Minutes","Share Drive Alone 30 to 44 Minutes", "Share Drive Alone 45 to 59 Minutes","Share Drive Alone 60 Minutes and Greater","Share Carpool Less Than 15 Minutes","Share Carpool 15 to 29 Minutes","Share Carpool 30 to 44 Minutes", "Share Carpool 45 to 59 Minutes","Share Carpool 60 Minutes and Greater","Share Transit Less Than 15 Minutes","Share Transit 15 to 29 Minutes","Share Transit 30 to 44 Minutes", "Share Transit 45 to 59 Minutes","Share Transit 60 Minutes and Greater") #Transport_Mode_Label values for later

# API Calls

# Residence Geographies, Counties and Metros

mode_residence_county_url= paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&in=state:",state,"&for=county:",county,"&key=", key)
mode_residence_metro_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

# Now Work, Counties and Metros

mode_work_county_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,B08601_001E,B08601_002E,B08601_003E,B08601_004E,B08601_010E,B08601_019E,B08601_018E,B08601_016E,B08601_017E,B08601_020E,B08601_021E&in=state:",state,"&for=county:",county,"&key=", key)
mode_work_metro_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,B08601_001E,B08601_002E,B08601_003E,B08601_004E,B08601_010E,B08601_019E,B08601_018E,B08601_016E,B08601_017E,B08601_020E,B08601_021E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

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

mode_residence_county <- f.data(mode_residence_county_url,2)

mode_residence_county2 <- cbind(mode_residence_county,ACS_year,source1)
names(mode_residence_county2) <-  c("Residence_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "State", "County_ID", "Year", "Source")

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

mode_residence_county_share <- f.shares(mode_residence_county2) %>% 
  select (Residence_Geo, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)
  

#Melt data into proper shape for exporting CSVs

mode_residence_county_share_melt <- melt(mode_residence_county_share, 
                                  id.vars=c("Residence_Geo","Year", "Workers_Est", "Source"),
                                  variable.name="Transport_Mode",
                                  value.name="Share"
) %>% mutate (
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>% 
  select (Residence_Geo, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share, Source)

#Now Bay Area

mode_residence_bay <- mode_residence_county2 %>%
  summarize(
    Residence_Geo="San Francisco Bay Area", 
    Workers_Est = sum(Workers_Est),
    Drive_Total = sum(Drive_Total),
    DAWorkers_Est = sum(DAWorkers_Est),
    CPWorkers_Est = sum(CPWorkers_Est),
    PTWorkers_Est = sum(PTWorkers_Est),
    Walk = sum(Walk),
    Bike = sum(Bike),
    Taxi = sum(Taxi),
    Motorcycle = sum(Motorcycle),
    Other = sum(Other),
    AtHome = sum(AtHome),
    Source=source1, 
    Metro_Name="Bay Area",
    Year=ACS_year
  )

#Now Metros

mode_residence_metro_only <- f.data(mode_residence_metro_url,1) %>% mutate(
  Source=source1,
  Metro_Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),
  Year=ACS_year
)

names(mode_residence_metro_only) <-  c("Residence_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "Metro_Code", "Source","Metro_Name", "Year")
mode_residence_metro_only$Metro_Code <- NULL # Remove this field
mode_residence_metro <- rbind(mode_residence_metro_only,mode_residence_bay)

mode_residence_metro_share <- f.shares(mode_residence_metro) %>% 
  select (Residence_Geo, Metro_Name, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)

mode_residence_metro_share_melt <- melt(mode_residence_metro_share, 
                                        id.vars=c("Residence_Geo", "Metro_Name","Year", "Workers_Est", "Source"),
                                        variable.name="Transport_Mode",
                                        value.name="Share"
) %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Residence_Geo, Metro_Name, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share, Source)

# Now Work

mode_work_county <- f.data(mode_work_county_url,2) 
names(mode_work_county) <- c("Workplace_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome" , "State", "County")

mode_work_county2 <- mode_work_county %>% mutate(
  Year = ACS_year,
  Source = work_source1
)

mode_work_county_share <- f.shares(mode_work_county2) %>%
select (Workplace_Geo, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)

mode_work_county_share_melt <- melt(mode_work_county_share, 
                             id.vars=c("Workplace_Geo","Year", "Workers_Est", "Source"),
                             variable.name="Transport_Mode",
                             value.name="Share"
) %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Workplace_Geo, Year, Workers_Est, Transport_Mode, Transport_Mode_Label, Share, Source)

# Work Bay Area

mode_work_bay <- mode_work_county2 %>%
  summarize(
    Workplace_Geo="San Francisco Bay Area", 
    Workers_Est = sum(Workers_Est),
    Drive_Total = sum(Drive_Total),
    DAWorkers_Est = sum(DAWorkers_Est),
    CPWorkers_Est = sum(CPWorkers_Est),
    PTWorkers_Est = sum(PTWorkers_Est),
    Walk = sum(Walk),
    Bike = sum(Bike),
    Taxi = sum(Taxi),
    Motorcycle = sum(Motorcycle),
    Other = sum(Other),
    AtHome = sum(AtHome),
    Source=work_source1, 
    Metro_Name="Bay Area",
    Year=ACS_year
  )

#Now Metros

mode_work_metro_only <- f.data(mode_work_metro_url,1) %>% mutate(
  Source=work_source1,
  Metro_Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),
  Year=ACS_year
)

names(mode_work_metro_only) <-  c("Workplace_Geo", "Workers_Est", "Drive_Total","DAWorkers_Est", "CPWorkers_Est", "PTWorkers_Est", "Walk", "Bike", "Taxi", "Motorcycle", "Other", "AtHome", "Metro_Code", "Source","Metro_Name", "Year")
mode_work_metro_only$Metro_Code <- NULL # Remove this field
mode_work_metro <- rbind(mode_work_metro_only,mode_work_bay)

mode_work_metro_share <- f.shares(mode_work_metro) %>% 
  select (Workplace_Geo, Metro_Name, Year, Workers_Est, DriveTot_Est, DriveAlone_Est, Carpool_Est, Transit_Est, Walk_Est, Other_w_Bike_Est, Bike_Est, Other_Est, Telework_Est, Source)

mode_work_metro_share_melt <- melt(mode_work_metro_share, 
                                        id.vars=c("Workplace_Geo", "Metro_Name","Year", "Workers_Est", "Source"),
                                        variable.name="Transport_Mode",
                                        value.name="Share"
) %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Workplace_Geo, Metro_Name, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share, Source)

# Travel Time Total API Calls - Both totals (topcoded at 90 minutes travel) and mode tables (topcoded at 60 minutes) are accessed

time_residence_county_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,C08136_001E,C08136_002E,C08136_003E,C08136_004E&in=state:",state,"&for=county:",county,"&key=", key)
time_work_county_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,C08536_001E,C08536_002E,C08536_003E,C08536_004E&in=state:",state,"&for=county:",county,"&key=", key)
time_residence_metro_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,C08136_001E,C08136_002E,C08136_003E,C08136_004E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)
time_work_metro_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,C08536_001E,C08536_002E,C08536_003E,C08536_004E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

cmode_time_bins = paste0("C08134_011E,C08134_012E,C08134_013E,",
                   "C08134_014E,C08134_015E,C08134_016E,C08134_017E,C08134_018E,C08134_019E,C08134_020E,C08134_021E,C08134_022E,C08134_023E,C08134_024E,C08134_025E,C08134_026E,",
                   "C08134_027E,C08134_028E,C08134_029E,C08134_030E,C08134_031E,C08134_032E,C08134_033E,C08134_034E,C08134_035E,C08134_036E,C08134_037E,C08134_038E,C08134_039E,",
                   "C08134_040E")

all_time_bins = paste0("B08303_001E,B08303_002E,B08303_003E,B08303_004E,B08303_005E,B08303_006E,B08303_007E,B08303_008E,",
                       "B08303_009E,B08303_010E,B08303_011E,B08303_012E,B08303_013E")


time_residence_bins_mode_county_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,",cmode_time_bins,"&in=state:",state,"&for=county:",county,"&key=", key)
time_residence_bins_mode_metro_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,",cmode_time_bins,"&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

time_residence_bins_all_county_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,",all_time_bins,"&in=state:",state,"&for=county:",county,"&key=", key)
time_residence_bins_all_metro_url = paste0("https://api.census.gov/data/",ACS_year,"/acs/acs",ACS_product,"?get=NAME,",all_time_bins,"&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

# Calculate average travel times (time bins for residence county and metro calculated later)

# Residence County and Metro

time_residence_county <- f.data(time_residence_county_url,2) 
names(time_residence_county) <- c("Residence_Geo", "Total_Aggregate", "Drive_Alone_Aggregate", "Carpool_Aggregate", "Transit_Aggregate", "State", "County")
time_residence_county2 <- merge(time_residence_county,mode_residence_county2,by="Residence_Geo") %>% mutate (
  NotHome = Workers_Est-AtHome,
  OverallTime_Est = Total_Aggregate / NotHome,
  DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est,
  CPTime_Est = Carpool_Aggregate / CPWorkers_Est,
  PTTime_Est = Transit_Aggregate / PTWorkers_Est,
  Year=ACS_year,
  Source1=timesource1,
  Source2=source1) %>%
  select (Residence_Geo, Year, Workers_Est, OverallTime_Est,DATime_Est, CPTime_Est, PTTime_Est, Source1, Source2)

time_residence_county_melt <- melt(time_residence_county2, 
                                     id.vars=c("Residence_Geo","Year", "Workers_Est", "Source1", "Source2"),
                                     variable.name="Transport_Mode",
                                     value.name="Time_Est"
) %>% mutate (
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select(Residence_Geo,Year,Workers_Est,Transport_Mode,Transport_Mode_Label,Time_Est,Source1,Source2)

# Bay Area

time_residence_bay <- time_residence_county %>%
  summarize(
    Residence_Geo="San Francisco Bay Area", 
    Total_Aggregate = sum(Total_Aggregate),
    Drive_Alone_Aggregate = sum(Drive_Alone_Aggregate),
    Carpool_Aggregate = sum(Carpool_Aggregate),
    Transit_Aggregate = sum(Transit_Aggregate),
    Metro_Name="Bay Area"
  )

# Metro

time_residence_metro_only <- f.data(time_residence_metro_url,1) %>% mutate(
  Metro_Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1])
)

names(time_residence_metro_only) <-  c("Residence_Geo", "Total_Aggregate", "Drive_Alone_Aggregate", "Carpool_Aggregate", "Transit_Aggregate", "Metro_Code", "Metro_Name")
time_residence_metro_only$Metro_Code <- NULL # Remove this field
time_residence_metro <- rbind(time_residence_metro_only,time_residence_bay)

time_residence_metro2 <- merge(time_residence_metro,mode_residence_metro,by=c("Residence_Geo","Metro_Name")) %>% mutate (
  NotHome = Workers_Est-AtHome,
  OverallTime_Est = Total_Aggregate / NotHome,
  DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est,
  CPTime_Est = Carpool_Aggregate / CPWorkers_Est,
  PTTime_Est = Transit_Aggregate / PTWorkers_Est,
  Year=ACS_year,
  Source1=timesource1,
  Source2=source1) %>%
  select (Residence_Geo, Metro_Name, Year, Workers_Est, OverallTime_Est,DATime_Est, CPTime_Est, PTTime_Est, Source1, Source2)

time_residence_metro_melt <- melt(time_residence_metro2, 
                                        id.vars=c("Residence_Geo", "Metro_Name","Year", "Workers_Est", "Source1", "Source2"),
                                        variable.name="Transport_Mode",
                                        value.name="Time_Est"
) %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Residence_Geo, Metro_Name, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Time_Est, Source1, Source2)

# Now Work

# County

time_work_county <- f.data(time_work_county_url,2) 
names(time_work_county) <- c("Workplace_Geo", "Total_Aggregate", "Drive_Alone_Aggregate", "Carpool_Aggregate", "Transit_Aggregate", "State", "County")
time_work_county2 <- merge(time_work_county,mode_work_county,by="Workplace_Geo") %>% mutate (
  NotHome = Workers_Est-AtHome,
  OverallTime_Est = Total_Aggregate / NotHome,
  DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est,
  CPTime_Est = Carpool_Aggregate / CPWorkers_Est,
  PTTime_Est = Transit_Aggregate / PTWorkers_Est,
  Year=ACS_year,
  Source1=work_timesource1,
  Source2=work_source1) %>%
  select (Workplace_Geo, Year, Workers_Est, OverallTime_Est,DATime_Est, CPTime_Est, PTTime_Est, Source1, Source2)

time_work_county_melt <- melt(time_work_county2, 
                                   id.vars=c("Workplace_Geo","Year", "Workers_Est", "Source1", "Source2"),
                                   variable.name="Transport_Mode",
                                   value.name="Time_Est"
) %>% mutate (
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select(Workplace_Geo,Year,Workers_Est,Transport_Mode,Transport_Mode_Label,Time_Est,Source1,Source2)

# Bay Area

time_work_bay <- time_work_county %>%
  summarize(
    Workplace_Geo="San Francisco Bay Area", 
    Total_Aggregate = sum(Total_Aggregate),
    Drive_Alone_Aggregate = sum(Drive_Alone_Aggregate),
    Carpool_Aggregate = sum(Carpool_Aggregate),
    Transit_Aggregate = sum(Transit_Aggregate),
    Metro_Name="Bay Area"
  )

# Metro

time_work_metro_only <- f.data(time_work_metro_url,1) %>% mutate(
  Metro_Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1])
)

names(time_work_metro_only) <-  c("Workplace_Geo", "Total_Aggregate", "Drive_Alone_Aggregate", "Carpool_Aggregate", "Transit_Aggregate", "Metro_Code", "Metro_Name")
time_work_metro_only$Metro_Code <- NULL # Remove this field
time_work_metro <- rbind(time_work_metro_only,time_work_bay)

time_work_metro2 <- merge(time_work_metro,mode_work_metro,by=c("Workplace_Geo","Metro_Name")) %>% mutate (
  NotHome = Workers_Est-AtHome,
  OverallTime_Est = Total_Aggregate / NotHome,
  DATime_Est = Drive_Alone_Aggregate / DAWorkers_Est,
  CPTime_Est = Carpool_Aggregate / CPWorkers_Est,
  PTTime_Est = Transit_Aggregate / PTWorkers_Est,
  Year=ACS_year,
  Source1=work_timesource1,
  Source2=work_source1) %>%
  select (Workplace_Geo, Metro_Name, Year, Workers_Est, OverallTime_Est,DATime_Est, CPTime_Est, PTTime_Est, Source1, Source2)

time_work_metro_melt <- melt(time_work_metro2, 
                                  id.vars=c("Workplace_Geo", "Metro_Name","Year", "Workers_Est", "Source1", "Source2"),
                                  variable.name="Transport_Mode",
                                  value.name="Time_Est"
) %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Workplace_Geo, Metro_Name, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Time_Est, Source1, Source2)

# Appending time bin values to residences
## County

time_residence_bins_all_county <- f.data(time_residence_bins_all_county_url,2)
names(time_residence_bins_all_county) <-  c("NAME", 
                                           "Tot_tot","Tot_lt5", "Tot_5to9","Tot_10to14", "Tot_15to19","Tot_20to24",
                                           "Tot_25to29", "Tot_30to34","Tot_35to39","Tot_40to44","Tot_45to59","Tot_60to89",
                                           "Tot_gt90","State", "County")

time_residence_bins_mode_county <- f.data(time_residence_bins_mode_county_url,2)
names(time_residence_bins_mode_county) <-  c("NAME", 
                                        "DA_tot","DA_lt10", "DA_10to14", "DA_15to19", "DA_20to24", "DA_25to29", "DA_30to34","DA_35to44","DA_45to59","DA_gt60",
                                        "CP_tot","CP_lt10", "CP_10to14", "CP_15to19", "CP_20to24", "CP_25to29", "CP_30to34","CP_35to44","CP_45to59","CP_gt60",
                                        "Transit_tot","Transit_lt10", "Transit_10to14", "Transit_15to19", "Transit_20to24", "Transit_25to29", "Transit_30to34","Transit_35to44","Transit_45to59","Transit_gt60",
                                        "State", "County")

time_residence_bins_county <- left_join(time_residence_bins_all_county,time_residence_bins_mode_county, by = c("NAME", "State", "County"))
rm(time_residence_bins_all_county,time_residence_bins_mode_county)

time_residence_bins_county <- time_residence_bins_county %>% mutate(
  
  Tot_lt15 = Tot_lt5+Tot_5to9+Tot_10to14,
  Tot_15to30 = Tot_15to19+Tot_20to24+Tot_25to29,
  Tot_30to45 = Tot_30to34+Tot_35to39+Tot_40to44,
  Tot_45to60 = Tot_45to59, 
  Tot_60to90 = Tot_60to89,
  Tot_gt90 = Tot_gt90,
  Tot_lt15_Est = Tot_lt15/Tot_tot,
  Tot_15to30_Est = Tot_15to30/Tot_tot,
  Tot_30to45_Est = Tot_30to45/Tot_tot,
  Tot_45to60_Est = Tot_45to60/Tot_tot,
  Tot_60to90_Est = Tot_60to90/Tot_tot,
  Tot_gt90_Est = Tot_gt90/Tot_tot,
  
  DA_lt15 = DA_lt10+DA_10to14,
  DA_15to30 = DA_15to19+DA_20to24+DA_25to29,
  DA_30to45 = DA_30to34+DA_35to44,
  DA_45to60 = DA_45to59,
  DA_lt15_Est = DA_lt15/DA_tot,
  DA_15to30_Est = DA_15to30/DA_tot,
  DA_30to45_Est = DA_30to45/DA_tot,
  DA_45to60_Est = DA_45to60/DA_tot,
  DA_gt60_Est = DA_gt60/DA_tot,
  
  CP_lt15 = CP_lt10+CP_10to14,
  CP_15to30 = CP_15to19+CP_20to24+CP_25to29,
  CP_30to45 = CP_30to34+CP_35to44,
  CP_45to60 = CP_45to59,
  CP_lt15_Est = CP_lt15/CP_tot,
  CP_15to30_Est = CP_15to30/CP_tot,
  CP_30to45_Est = CP_30to45/CP_tot,
  CP_45to60_Est = CP_45to60/CP_tot,
  CP_gt60_Est = CP_gt60/CP_tot,
  
  Transit_lt15 = Transit_lt10+Transit_10to14,
  Transit_15to30 = Transit_15to19+Transit_20to24+Transit_25to29,
  Transit_30to45 = Transit_30to34+Transit_35to44,
  Transit_45to60 = Transit_45to59,
  Transit_lt15_Est = Transit_lt15/Transit_tot,
  Transit_15to30_Est = Transit_15to30/Transit_tot,
  Transit_30to45_Est = Transit_30to45/Transit_tot,
  Transit_45to60_Est = Transit_45to60/Transit_tot,
  Transit_gt60_Est = Transit_gt60/Transit_tot,
  
  Year = ACS_year,
  Workers_Est = Tot_tot,
  Source1=bins_all_source,
  Source2=bins_mode_source,
  Residence_Geo=NAME) 

time_residence_bins_county1 <- time_residence_bins_county %>%
  select(Residence_Geo,Year,Workers_Est, Source1,Source2,
         Tot_lt15_Est,Tot_15to30_Est,Tot_30to45_Est,Tot_45to60_Est,Tot_60to90_Est,Tot_gt90_Est,
         DA_lt15_Est,DA_15to30_Est,DA_30to45_Est,DA_45to60_Est,DA_gt60_Est,
         CP_lt15_Est,CP_15to30_Est,CP_30to45_Est,CP_45to60_Est,CP_gt60_Est,
         Transit_lt15_Est,Transit_15to30_Est,Transit_30to45_Est,Transit_45to60_Est,Transit_gt60_Est)
         
time_residence_bins_county_melt <- melt(time_residence_bins_county1, 
                             id.vars=c("Residence_Geo","Year", "Workers_Est", "Source1", "Source2"),
                             variable.name="Transport_Mode",
                             value.name="Share_Est") %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Residence_Geo, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share_Est, Source1, Source2)

# Bay Area Bins

time_residence_bins_bay <- time_residence_bins_county %>%
  summarize(
    Residence_Geo="San Francisco Bay Area",
    
    Tot_tot = sum(Tot_tot),
    Tot_lt15 = sum(Tot_lt15),
    Tot_15to30 = sum(Tot_15to30),
    Tot_30to45 = sum(Tot_30to45),
    Tot_45to60 = sum(Tot_45to60),
    Tot_60to90 = sum(Tot_60to90),
    Tot_gt90 = sum(Tot_gt90),
    
    DA_tot = sum(DA_tot),
    DA_lt15 = sum(DA_lt15),
    DA_15to30 = sum(DA_15to30),
    DA_30to45 = sum(DA_30to45),
    DA_45to60 = sum(DA_45to60),
    DA_gt60 = sum(DA_gt60),
    
    CP_tot = sum(CP_tot),
    CP_lt15 = sum(CP_lt15),
    CP_15to30 = sum(CP_15to30),
    CP_30to45 = sum(CP_30to45),
    CP_45to60 = sum(CP_45to60),
    CP_gt60 = sum(CP_gt60),
    
    Transit_tot = sum(Transit_tot),
    Transit_lt15 = sum(Transit_lt15),
    Transit_15to30 = sum(Transit_15to30),
    Transit_30to45 = sum(Transit_30to45),
    Transit_45to60 = sum(Transit_45to60),
    Transit_gt60 = sum(Transit_gt60),
    
    Metro_Name="Bay Area")

# Metro Bins

time_residence_bins_all_metro_only <- f.data(time_residence_bins_all_metro_url,1) %>% mutate(
  Metro_Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1])
)

names(time_residence_bins_all_metro_only) <-  c("NAME", 
                                            "Tot_tot","Tot_lt5", "Tot_5to9","Tot_10to14", "Tot_15to19","Tot_20to24",
                                            "Tot_25to29", "Tot_30to34","Tot_35to39","Tot_40to44","Tot_45to59","Tot_60to89",
                                            "Tot_gt90","Metro_Code", "Metro_Name")

time_residence_bins_mode_metro_only <- f.data(time_residence_bins_mode_metro_url,1) %>% mutate(
  Metro_Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1])
)

names(time_residence_bins_mode_metro_only) <-  c("NAME", 
                                             "DA_tot","DA_lt10", "DA_10to14", "DA_15to19", "DA_20to24", "DA_25to29", "DA_30to34","DA_35to44","DA_45to59","DA_gt60",
                                             "CP_tot","CP_lt10", "CP_10to14", "CP_15to19", "CP_20to24", "CP_25to29", "CP_30to34","CP_35to44","CP_45to59","CP_gt60",
                                             "Transit_tot","Transit_lt10", "Transit_10to14", "Transit_15to19", "Transit_20to24", "Transit_25to29", "Transit_30to34","Transit_35to44","Transit_45to59","Transit_gt60",
                                             "Metro_Code", "Metro_Name")

time_residence_bins_metro_only <- left_join(time_residence_bins_all_metro_only,time_residence_bins_mode_metro_only, by = c("NAME", "Metro_Code", "Metro_Name"))
rm(time_residence_bins_all_metro_only,time_residence_bins_mode_metro_only)

time_residence_bins_metro_only$Metro_Code <- NULL # Remove this field

time_residence_bins_metro_only <- time_residence_bins_metro_only %>% mutate(

  Tot_lt15 = Tot_lt5+Tot_5to9+Tot_10to14,
  Tot_15to30 = Tot_15to19+Tot_20to24+Tot_25to29,
  Tot_30to45 = Tot_30to34+Tot_35to39+Tot_40to44,
  Tot_45to60 = Tot_45to59, 
  Tot_60to90 = Tot_60to89,
  Tot_gt90 = Tot_gt90,

  DA_lt15 = DA_lt10+DA_10to14,
  DA_15to30 = DA_15to19+DA_20to24+DA_25to29,
  DA_30to45 = DA_30to34+DA_35to44,
  DA_45to60 = DA_45to59,

  CP_lt15 = CP_lt10+CP_10to14,
  CP_15to30 = CP_15to19+CP_20to24+CP_25to29,
  CP_30to45 = CP_30to34+CP_35to44,
  CP_45to60 = CP_45to59,

  Transit_lt15 = Transit_lt10+Transit_10to14,
  Transit_15to30 = Transit_15to19+Transit_20to24+Transit_25to29,
  Transit_30to45 = Transit_30to34+Transit_35to44,
  Transit_45to60 = Transit_45to59,
  Residence_Geo = NAME) %>%
  select(Residence_Geo,
         Tot_tot,Tot_lt15,Tot_15to30,Tot_30to45,Tot_45to60,Tot_60to90,Tot_gt90,
         DA_tot,DA_lt15,DA_15to30,DA_30to45,DA_45to60,DA_gt60,  
         CP_tot,CP_lt15,CP_15to30,CP_30to45,CP_45to60,CP_gt60,
         Transit_tot,Transit_lt15,Transit_15to30,Transit_30to45,Transit_45to60,Transit_gt60,
         Metro_Name)

time_residence_bins_metro <- rbind(time_residence_bins_metro_only,time_residence_bins_bay) %>% mutate(

  Tot_lt15_Est = Tot_lt15/Tot_tot,
  Tot_15to30_Est = Tot_15to30/Tot_tot,
  Tot_30to45_Est = Tot_30to45/Tot_tot,
  Tot_45to60_Est = Tot_45to60/Tot_tot,
  Tot_60to90_Est = Tot_60to90/Tot_tot,
  Tot_gt90_Est = Tot_gt90/Tot_tot,
  
  DA_lt15_Est = DA_lt15/DA_tot,
  DA_15to30_Est = DA_15to30/DA_tot,
  DA_30to45_Est = DA_30to45/DA_tot,
  DA_45to60_Est = DA_45to60/DA_tot,
  DA_gt60_Est = DA_gt60/DA_tot,
  
  CP_lt15_Est = CP_lt15/CP_tot,
  CP_15to30_Est = CP_15to30/CP_tot,
  CP_30to45_Est = CP_30to45/CP_tot,
  CP_45to60_Est = CP_45to60/CP_tot,
  CP_gt60_Est = CP_gt60/CP_tot,
  
  Transit_lt15_Est = Transit_lt15/Transit_tot,
  Transit_15to30_Est = Transit_15to30/Transit_tot,
  Transit_30to45_Est = Transit_30to45/Transit_tot,
  Transit_45to60_Est = Transit_45to60/Transit_tot,
  Transit_gt60_Est = Transit_gt60/Transit_tot,
  
  Year = ACS_year,
  Workers_Est = Tot_tot,
  Source1=bins_all_source,
  Source2=bins_mode_source) %>%
  
  select(Residence_Geo,Metro_Name, Year,Workers_Est, Source1,Source2, 
         Tot_lt15_Est,Tot_15to30_Est,Tot_30to45_Est,Tot_45to60_Est,Tot_60to90_Est,Tot_gt90_Est,
         DA_lt15_Est,DA_15to30_Est,DA_30to45_Est,DA_45to60_Est,DA_gt60_Est,
         CP_lt15_Est,CP_15to30_Est,CP_30to45_Est,CP_45to60_Est,CP_gt60_Est,
         Transit_lt15_Est,Transit_15to30_Est,Transit_30to45_Est,Transit_45to60_Est,Transit_gt60_Est)
  
time_residence_bins_metro_melt <- melt(time_residence_bins_metro, 
                             id.vars=c("Residence_Geo", "Metro_Name","Year", "Workers_Est", "Source1", "Source2"),
                             variable.name="Transport_Mode",
                             value.name="Share_Est"
) %>% mutate(
  Transport_Mode_Label = values[match(Transport_Mode, index)]) %>%
  select (Residence_Geo, Metro_Name, Year, Workers_Est, Transport_Mode,Transport_Mode_Label,Share_Est, Source1, Source2)


# Export CSVs

write.csv(mode_residence_county_share_melt, paste0(share_output_csv, "1Year_County_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_residence_metro_share_melt, paste0(share_output_csv, "1Year_Metro_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_work_county_share_melt, paste0(work_share_output_csv, "1Year_Work_County_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_work_metro_share_melt, paste0(work_share_output_csv, "1Year_Work_Metro_Mode_Share.csv"), row.names = FALSE, quote = T)

write.csv(time_residence_county_melt, paste0(time_output_csv, "1Year_County_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
write.csv(time_residence_metro_melt , paste0(time_output_csv, "1Year_Metro_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
write.csv(time_work_county_melt , paste0(work_time_output_csv, "1Year_Work_County_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
write.csv(time_work_metro_melt , paste0(work_time_output_csv, "1Year_Metro_County_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)

write.csv(time_residence_bins_county_melt, paste0(time_output_csv, "1Year_County_Travel_Time_Bins.csv"), row.names = FALSE, quote = T)
write.csv(time_residence_bins_metro_melt , paste0(time_output_csv, "1Year_Metro_Travel_Time_Bins.csv"), row.names = FALSE, quote = T)


