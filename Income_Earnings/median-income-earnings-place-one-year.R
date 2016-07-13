
# Import Libraries

library(knitr)
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(RCurl)
library(RJSONIO)
library(reshape2)
library(httr)

# Set up census variables and directories for saving final files

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"
ACS_year="2014"
ACS_product="1"
county="01,13,41,55,75,81,85,95,97"
state="06"
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"

source_residence="B19013_ACS15_1YR"
source_work="B08521_ACS15_1YR"

residence_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC4_Income by Place of Residence/",ACS_year,"_")
work_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC5_Income by Place of Work/",ACS_year,"_")

# Import census API data for income and earnings for county and metro of work, respectively.

county_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B19013_001E,B08521_001E&in=state:06&for=county:",county,"&key=",key)
metro_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B19013_001E,B08521_001E&for=metropolitan+statistical+area/micropolitan+statistical+area:",metro,"&key=",key)

# Function for bringing in data
# Put API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."

f.income <- function(url){  
  furl <- content(GET(url))
  for (i in 1:length(furl)){
    if (i==1) header <- furl [[i]]
    if (i==2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      income <- data.frame(temp, stringsAsFactors=FALSE)
      names (income) <- header
    }
    if (i>2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      tempdf <- data.frame(temp, stringsAsFactors=FALSE)
      names (tempdf) <- header
      income <- rbind (income,tempdf)
    }
  }
  return (income)
}

# First counties

income_county <- f.income(county_url)

# Now metros

income_metro <- f.income(metro_url)

# For counties - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_county$Geo <- sapply((strsplit(as.character(income_county$NAME),',')),function(x) x[1])
income_county$Year <- ACS_year
income_county$Median_Income <- income_county$B19013_001E
income_county$Median_Earnings <- income_county$B08521_001E
income_county$Residence_Source <- source_residence
income_county$Work_Source <- source_work

residence_income_county <- income_county %>% 
  select(Geo, Median_Income, Residence_Source)
names(residence_income_county)[1]<-"Residence_Geo"
names(residence_income_county)[3]<-"Source"

workplace_earnings_county <- income_county %>% 
  select(Geo, Median_Earnings, Work_Source)
names(workplace_earnings)[1]<-"Workplace_Geo"
names(workplace_earnings)[3]<-"Source"

# For metros - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_metro$Geo <- paste(sapply((strsplit(as.character(income_metro$NAME),'-')),function(x) x[1]),"MSA")
income_metro$Year <- ACS_year
income_metro$Median_Income <- income_metro$B19013_001E
income_metro$Median_Earnings <- income_metro$B08521_001E
income_metro$Residence_Source <- source_residence
income_metro$Work_Source <- source_work

residence_income_metro <- income_metro %>% 
  select(Geo, Median_Income, Residence_Source)
names(residence_income_metro)[1]<-"Residence_Geo"
names(residence_income_metro)[3]<-"Source"

workplace_earnings_metro <- income_metro %>% 
  select(Geo, Median_Earnings, Work_Source)
names(workplace_earnings)[1]<-"Workplace_Geo"
names(workplace_earnings)[3]<-"Source"

# Write out CSV 

#write.csv(residence_income , paste0(residence_output_csv, "5Year_Residence_City_Income.csv"), row.names = FALSE, quote = T)
#write.csv(workplace_earnings , paste0(work_output_csv, "5Year_Workplace_City_Earnings.csv"), row.names = FALSE, quote = T)








response_county <- content(GET(county_url))
response_metro <- content(GET(metro_url))



income.f <- function(url){  
  furl <- content(GET(url))
  for (i in 1:length(furl)){
    if (i==1) header <- furl [[i]]
    if (i==2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      income <- data.frame(temp, stringsAsFactors=FALSE)
      names (income) <- header
    }
    if (i>2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      tempdf <- data.frame(temp, stringsAsFactors=FALSE)
      names (tempdf) <- header
      income <- rbind (income,tempdf)
      }
  }
return (income)
}

bigtime <- trial.f(metro_url)





