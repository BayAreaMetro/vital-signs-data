# median-contract-rent-county-metro-one-year.R
# Median rents of counties, Bay Area, and select metros
# SI
# August 18, 2016

################################################################
# Variables to edit

ACS_year="2015"
ACS_product="1"
county="01,13,41,55,75,81,85,95,97"
state="06"
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"
source="ACS15_B25058_1YR"

# Set up destination to save

output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC8_Rent/2015/",ACS_year,"_")
county_csv="1Year_County_Rent.csv"
region_csv="1Year_Region_Rent.csv"
metro_csv="1Year_Metro_Rent.csv"

# Census API Key

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"

# End variables to edit
################################################################

# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(httr)

# Import census API data for rent and earnings for place of residence and workplace, respectively.

county_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                     ACS_product,"?get=NAME,B25058_001E,B25058_001M,B25054_001E","&in=state:06&for=county:",
                     county,"&key=",key)

metro_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                    ACS_product,"?get=NAME,B25058_001E,B25058_001M",                          
                    "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
                    metro,"&key=",key)

# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."

f.rent <- function(url){  
  furl <- content(GET(url))
  for (i in 1:length(furl)){
    if (i==1) header <- furl [[i]]
    if (i==2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      rent <- data.frame(temp, stringsAsFactors=FALSE)
      names (rent) <- header
    }
    if (i>2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      tempdf <- data.frame(temp, stringsAsFactors=FALSE)
      names (tempdf) <- header
      rent <- rbind (rent,tempdf)
    }
  }
  return (rent)
}

county_rent_in <- f.rent(county_url)
metro_rent_in <- f.rent(metro_url)

#Convert all columns to numeric format excepting geographic variables

for(i in 2:(ncol(county_rent_in)-2)) {
  county_rent_in[,i] <- as.numeric(county_rent_in[,i])
}

for(i in 2:(ncol(metro_rent_in)-1)) {
  metro_rent_in[,i] <- as.numeric(metro_rent_in[,i])
}

# County output

county_rent_interim <- county_rent_in %>% mutate(
  County = sapply((strsplit(as.character(NAME),'County')),function(x) x[1]),
  Year = ACS_year,
  Median_Contract_Rent = B25058_001E,
  Median_Contract_Rent_MOE = B25058_001M,
  Total_Rentals = B25054_001E,
  Source = source
  ) 

county_rent <- county_rent_interim %>%
  select(County,Year,Median_Contract_Rent,Median_Contract_Rent_MOE,Source)

# Metro output
# Start with calculating Bay Area median contract rent with a weighted average

region_rent <- county_rent_interim %>% mutate(
  median_x_total = Median_Contract_Rent*Total_Rentals
  ) %>%
  summarize(weighted_median=sum(median_x_total)/sum(Total_Rentals)) %>% mutate(
  Metro = "Bay Area",
  Year = ACS_year,
  Median_Contract_Rent = round(weighted_median),
  Source = source
  ) %>%
  select(Metro,Year,Median_Contract_Rent,Source)

metro_nobay <- metro_rent_in %>% mutate(
  Metro = sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),
  Year = ACS_year, 
  Median_Contract_Rent = B25058_001E,
  Source = source
  ) %>%
  select(Metro,Year,Median_Contract_Rent,Source)

metro_rent <- rbind(metro_nobay,region_rent)
 
# Write out CSV 

write.csv(county_rent, paste0(output_csv, county_csv), row.names = FALSE, quote = T)
write.csv(region_rent, paste0(output_csv, region_csv), row.names = FALSE, quote = T)
write.csv(metro_rent, paste0(output_csv, metro_csv), row.names = FALSE, quote = T)