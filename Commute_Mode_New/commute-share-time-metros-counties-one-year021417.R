#commute-share-time-metros-counties-one-year021017.R
#Script to convert Census APIs into commute share tables for national metros and Bay Area counties
#SI
#02/10/17
#Both an aggregate time for all modes and a sub-mode table are used because suppression means only totals available in some geos.
################################################################
# Variables to edit - Begin

ACS_year="2015"
ACS_product="1"
county="01,13,41,55,75,81,85,95,97"
state="06"
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"
source1="B08301_ACS15_1YR"
work_source1="B08601_ACS15_1YR"
timesource1="C08136_ACS15_1YR"
work_timesource1="C08536_ACS15_1YR"


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

index <- c("DriveTot_Est","DriveAlone_Est","Carpool_Est","Transit_Est","Walk_Est","Other_w_Bike_Est","Bike_Est","Other_Est","Telework_Est", "OverallTime_Est", "DATime_Est", "CPTime_Est", "PTTime_Est") # Transport_Mode index for later reference
values <- c("Share Total Auto","Share Drive Alone","Share Carpool", "Share Transit", "Share Walk", "Share Other With Bike","Share Bike", "Share Other", "Share Work at Home", "Total Mean Travel Time", "Drive Alone Mean Travel Time", "Carpool Mean Travel Time", "Transit Mean Travel Time") #Transport_Mode_Label values for later


# API Calls

# Residence Geographies, Counties and Metros

mode_residence_county_url= paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&in=state:",state,"&for=county:",county,"&key=", key)
mode_residence_metro_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08301_001E,B08301_002E,B08301_003E,B08301_004E,B08301_010E,B08301_019E,B08301_018E,B08301_016E,B08301_017E,B08301_020E,B08301_021E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

# Now Work, COunties and Metros

mode_work_county_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08601_001E,B08601_002E,B08601_003E,B08601_004E,B08601_010E,B08601_019E,B08601_018E,B08601_016E,B08601_017E,B08601_020E,B08601_021E&in=state:",state,"&for=county:",county,"&key=", key)
mode_work_metro_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B08601_001E,B08601_002E,B08601_003E,B08601_004E,B08601_010E,B08601_019E,B08601_018E,B08601_016E,B08601_017E,B08601_020E,B08601_021E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)

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

# Travel Time Total API Calls

time_residence_county_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,C08136_001E,C08136_002E,C08136_003E,C08136_004E&in=state:",state,"&for=county:",county,"&key=", key)
time_work_county_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,C08536_001E,C08536_002E,C08536_003E,C08536_004E&in=state:",state,"&for=county:",county,"&key=", key)
time_residence_metro_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,C08136_001E,C08136_002E,C08136_003E,C08136_004E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)
time_work_metro_url = paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,C08536_001E,C08536_002E,C08536_003E,C08536_004E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=", key)



# Calculate Travel Times

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


# Export CSVs

write.csv(mode_residence_county_share_melt, paste0(share_output_csv, "1Year_County_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_residence_metro_share_melt, paste0(share_output_csv, "1Year_Metro_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_work_county_share_melt, paste0(work_share_output_csv, "1Year_Work_County_Mode_Share.csv"), row.names = FALSE, quote = T)
write.csv(mode_work_metro_share_melt, paste0(work_share_output_csv, "1Year_Work_Metro_Mode_Share.csv"), row.names = FALSE, quote = T)

write.csv(time_residence_county_melt, paste0(time_output_csv, "1Year_County_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
write.csv(time_residence_metro_melt , paste0(time_output_csv, "1Year_Metro_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
write.csv(time_work_county_melt , paste0(work_time_output_csv, "1Year_Work_County_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)
write.csv(time_work_metro_melt , paste0(work_time_output_csv, "1Year_Metro_County_Mean_Travel_Time.csv"), row.names = FALSE, quote = T)

