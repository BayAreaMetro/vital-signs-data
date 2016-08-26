################################################################
# Variables to edit

ACS_year="2014"
ACS_product="5"
city="00562,00674,01640,02252,03092,05108,05164,05290,06000,08142,08310,09066,09892,10345,13882,14190,14736,16000,16462,16560,17610,17918,17988,19402,20018,20956,21796,22594,23168,23182,25338,26000,29504,31708,33000,33056,33308,33798,39122,40438,41992,43280,43294,44112,46114,46870,47710,47486,47766,48956,49187,49278,49670,50258,50916,52582,53000,53070,54232,54806,55282,56784,56938,57288,57456,57764,57792,58380,60102,60620,60984,62546,62980,64434,65028,65070,67000,68000,68084,68252,68294,68364,68378,69084,70098,70280,70364,70770,72646,73262,64140,75630,77000,78666,81204,81554,81666,83346,85922,86440,86930"
county="01,13,41,55,75,81,85,95,97"
state="06"
source_residence="ACS14_B25058_5YR"

# Set up destination to save

residence_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/2A_Economy/EC8_Rent/",ACS_year,"_")
name_city_csv="5Year_City_Rent.csv"
name_tract_csv="5Year_Tract_Rent.csv"

# Census API Key

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"

# End variables to edit
################################################################

# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(httr)

# Import census API data for rent and earnings for place of residence and workplace, respectively.

city_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B25058_001E,B25058_001M&in=state:",state,"&for=place:",city,"&key=",key)
tract_url <- paste0("http://api.census.gov/data/",ACS_year,"/acs",ACS_product,"?get=NAME,B25058_001E,B25058_001M&in=state:",state,"+county:",county,"&for=tract:*&key=",key)


#http://api.census.gov/data/2013/acs5?get=NAME,B01001_001E&for=tract:*&in=state:01&key=YOUR_KEY_GOES_HERE


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

city_rent <- f.rent(city_url)
tract_rent <- f.rent(tract_url)

# City Output

city_rent_final <- city_rent %>% mutate(
  Id = paste0 ("1400000US06",place),
  Id2 = paste0 ("6",place),
  temp = sapply((strsplit(as.character(NAME),'city,')),function(x) x[1]),
  Residence_Geo = sapply((strsplit(as.character(temp),'town,')),function(x) x[1]),
  Year = ACS_year,
  Median_Rent = B25058_001E,
  Median_Rent_MOE = B25058_001M,
  Source = source_residence
  ) %>%
  select(Id,Id2,Residence_Geo,Year,Median_Rent,Median_Rent_MOE,Source)

# Tract Output

tract_rent_final <- tract_rent %>% mutate(
  Id = paste0 ("1400000US06",county,tract),
  Id2 = paste0 ("6",county,tract),
  Year = ACS_year,
    County = sapply((strsplit(as.character(NAME),',')),function(x) x[2]),
  Tract = sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Median_Rent = B25058_001E,
  Median_Rent_MOE = B25058_001M,
  Source = source_residence
  ) %>%
  select(Id, Id2,County,Tract,Year,Median_Rent,Median_Rent_MOE,Source)



# Write out CSV 

write.csv(city_rent_final, paste0(residence_output_csv, name_city_csv), row.names = FALSE, quote = T)
write.csv(tract_rent_final, paste0(residence_output_csv, name_tract_csv), row.names = FALSE, quote = T)