#poverty-county-metros-one-year.R
#Script to convert Census APIs into poverty tables for region and metros
#SI
#08/17/16
################################################################
# Variables to edit - Begin

ACS_year="2014"
ACS_product="1"
county="01,13,41,55,75,81,85,95,97"
state="06"
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"
source="ACS14_C17002_1YR"


# Set up destination to save

output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC11_Poverty/2014/",ACS_year,"_")
Region_csv="1Year_Region.csv"
County_csv="1Year_County.csv"
Metro_csv="1Year_Metro.csv"

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

county_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                     ACS_product,"?get=NAME,C17002_001E,C17002_002E,C17002_003E,C17002_008E","&in=state:06&for=county:",
                     county,"&key=",key)

metro_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                          ACS_product,"?get=NAME,C17002_001E,C17002_002E,C17002_003E,C17002_008E",                          
                          "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
                          metro,"&key=",key)


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

county_poverty_in <- f.poverty(county_url)
metro_poverty_in <- f.poverty(metro_url)

#Convert all columns to numeric format

for(i in 2:ncol(county_poverty_in)) {
  county_poverty_in[,i] <- as.numeric(county_poverty_in[,i])
}

for(i in 2:ncol(metro_poverty_in)) {
  metro_poverty_in[,i] <- as.numeric(metro_poverty_in[,i])
}

# Create County categories

county_poverty <- county_poverty_in %>% mutate(
  Geo.Name = sapply((strsplit(as.character(NAME),'County')),function(x) x[1]),
  Year = ACS_year,
  PovPop = C17002_001E,
  Pov100 = C17002_002E + C17002_003E,
  Pov200 = C17002_001E - C17002_008E,
  Povper100 = Pov100/PovPop,
  Povper200 = Pov200/PovPop,
  Source = source
  ) %>%
  select(Geo.Name,Year,PovPop,Pov100,Pov200,Povper100,Povper200,Source)

# Now regional poverty

region_poverty <- county_poverty %>% 
  summarise(Povper200 = sum(Pov200)/sum(PovPop),Povper100 = sum(Pov100)/sum(PovPop)) %>% mutate(
  Geo.Name = "Bay Area",
  Year = ACS_year,
  Source = source)%>%
  select(Geo.Name, Year, Povper200,Povper100,Source)

# Now metro poverty
  
metro_poverty_nobay <- metro_poverty_in %>% mutate(
  Year = ACS_year,
  Geography = NAME,
  Name = sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),
  PovPop = C17002_001E,
  Pov100 = C17002_002E + C17002_003E,
  Pov200 = C17002_001E - C17002_008E,
  Povper100 = Pov100/PovPop,
  Povper200 = Pov200/PovPop,
  Source = source
  ) %>%
  select(Year,Geography,Name,PovPop,Pov100,Pov200,Povper100,Povper200,Source)

# Add Bay Area to regional

region_poverty_append <- county_poverty %>% 
  summarise(PovPop=sum(PovPop),Pov100=sum(Pov100),Pov200=sum(Pov200),Povper100 = sum(Pov100)/sum(PovPop), Povper200 = sum(Pov200)/sum(PovPop)) %>%
  mutate(
  Year = ACS_year,
  Geography = "Bay Area",
  Name = "Bay Area",
  Source = source
  ) %>%
  select(Year,Geography,Name,PovPop,Pov100,Pov200,Povper100,Povper200,Source)

metro_poverty <- rbind(metro_poverty_nobay,region_poverty_append)
  

# Write out CSV 

write.csv(region_poverty, paste0(output_csv, Region_csv), row.names = FALSE, quote = T)
write.csv(county_poverty, paste0(output_csv, County_csv), row.names = FALSE, quote = T)
write.csv(metro_poverty, paste0(output_csv, Metro_csv), row.names = FALSE, quote = T)