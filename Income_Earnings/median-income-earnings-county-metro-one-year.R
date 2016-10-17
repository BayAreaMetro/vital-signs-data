
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
ACS_year="2015"
ACS_product="1"
county="01,13,41,55,75,81,85,95,97"
state="06"

#if 2014 or after metro data:
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"

#if 2013 or before metro data:
# metro="37980,47900,26420,31100,31080,16980,19100,35620,12060" 

source_residence="B19013_ACS15_1YR"
source_work="B08521_ACS15_1YR"

residence_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC4_Income by Place of Residence/",ACS_year,"_")
work_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC5_Income by Place of Work/",ACS_year,"_")

# Import census API data for income and earnings for county and metro of work, respectively.

county_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
  ACS_product,"?get=NAME,B19013_001E,B19013_001M,B08521_001E,B08521_001M,B25009_001E,B08604_001E&in=state:06&for=county:",county,"&key=",key)
metro_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
  ACS_product,"?get=NAME,B19013_001E,B19013_001M,B08521_001E,B08521_001M&for=metropolitan+statistical+area/micropolitan+statistical+area:",
  metro,"&key=",key)

# Function for bringing in data
# Puts API data into list file
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

income_county <- income_county %>% mutate (
  Geo=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  Median_Income = as.numeric(B19013_001E),
  Median_Income_MOE = as.numeric (B19013_001M),
  Median_Earnings = as.numeric(B08521_001E),
  Median_Earnings_MOE = as.numeric(B08521_001M),
  Households = as.numeric(B25009_001E),
  Workers = as.numeric(B08604_001E),
  Income_x_Households = Median_Income*Households,
  Earnings_x_Workers = Median_Earnings*Workers,
  Residence_Source = source_residence,
  Work_Source = source_work
  )

# Calculate a Bay Area weighted average of household income/worker earnings to compare with other metros

bay_median_income = round(sum(income_county$Income_x_Households)/sum(income_county$Households))
bay_median_earnings = round(sum(income_county$Earnings_x_Workers)/sum(income_county$Workers))

income_metro <- rbind(income_metro,"10" = c("Bay Area", bay_median_income,"NA",bay_median_earnings,"NA","NA"))


# Create separate county files for income and earnings

residence_income_county <- income_county %>% 
  select(Geo, Median_Income, Median_Income_MOE, Residence_Source) %>% 
  rename(Residence_Geo=Geo, Source=Residence_Source)

workplace_earnings_county <- income_county %>% 
  select(Geo, Median_Earnings, Median_Earnings_MOE, Work_Source) %>%
  rename(Workplace_Geo=Geo, Source=Work_Source)

# For metros - append geography variables, apply source variables, separate residence and workplace files, and rename some variables

income_metro <- income_metro %>% mutate (
  Geo=ifelse(NAME=="Bay Area","Bay Area",paste(sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),"MSA")),
  Year=ACS_year,
  Median_Income = B19013_001E,
  Median_Income_MOE = B19013_001M,
  Median_Earnings = B08521_001E,
  Median_Earnings_MOE = B08521_001M,
  Residence_Source = source_residence,
  Work_Source = source_work
)

residence_income_metro <- income_metro %>% 
  select(Geo, Median_Income, Median_Income_MOE, Residence_Source) %>%
  rename(Residence_Geo=Geo,Source=Residence_Source)

workplace_earnings_metro <- income_metro %>% 
  select(Geo, Median_Earnings, Median_Earnings_MOE, Work_Source) %>%
  rename(Workplace_Geo=Geo,Source=Work_Source)

# Write out CSV 
write.csv(residence_income_county, paste0(residence_output_csv, "1Year_Residence_County_Income.csv"), row.names = FALSE, quote = T)
write.csv(residence_income_metro, paste0(residence_output_csv, "1Year_Residence_Metro_Income.csv"), row.names = FALSE, quote = T)
write.csv(workplace_earnings_county, paste0(work_output_csv, "1Year_Workplace_County_Earnings.csv"), row.names = FALSE, quote = T)
write.csv(workplace_earnings_metro, paste0(work_output_csv, "1Year_Workplace_Metro_Earnings.csv"), row.names = FALSE, quote = T)



