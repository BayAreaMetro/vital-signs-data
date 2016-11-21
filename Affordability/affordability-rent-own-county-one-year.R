#affordability-rent-own-county-one-year.R
#Script to convert Census APIs into rent/own affodability tables for region and metros
#SI
#08/17/16, revised November 21, 2016
################################################################
# Variables to edit - Begin

ACS_year="2015"
ACS_product="1"
county="01,13,41,55,75,81,85,95,97"
state="06"
metro="37980,47900,26420,33100,31080,16980,19100,35620,12060"
source1="ACS15_B25074_1YR"
source2="ACS15_B25095_1YR"

# Set up destination to save

residence_output_csv=paste0("C:/Users/sisrae.MTC/Box Sync/Data/3_Equity/EQ2_H (former EC10)/EC10_H/2015/",ACS_year,"_")
Region_Overall_csv="1Year_Region_Overall.csv"
Region_byIncome_csv="1Year_Region_byIncome.csv"
County_Overall_csv="1Year_County_Overall.csv"
County_byIncome_csv="1Year_County_byIncome.csv"
Metro_csv="1Year_Metro.csv"

# Filter Napa out (often table is suppressed due to too low sample for rentals)

Filter_Napa = "Yes"

# Census API Key

key="b901231133cf7da9e4ae3dea1af2470e87b3b9e7"

# End variables to edit
################################################################

# Import Libraries

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(httr)
library(reshape2)
'%notin%' <- Negate('%in%') # Creates function to search if an element is not within a vector, array, etc.

# Import census API data for rent and earnings for place of residence and workplace, respectively. API limit is 50, so two batches are needed.

county_rent_url1 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                     ACS_product,"?get=NAME,B25074_001E,B25074_002E,B25074_003E,B25074_004E,",
                     "B25074_005E,B25074_006E,B25074_007E,B25074_008E,B25074_009E,B25074_010E,",
                     "B25074_011E,B25074_012E,B25074_013E,B25074_014E,B25074_015E,B25074_016E,",
                     "B25074_017E,B25074_018E,B25074_019E,B25074_020E,B25074_021E,B25074_022E,",
                     "B25074_023E,B25074_024E,B25074_025E,B25074_026E,B25074_027E,B25074_028E,",
                     "B25074_029E,B25074_030E,B25074_031E,B25074_032E","&in=state:06&for=county:",
                     county,"&key=",key)
                     
county_rent_url2 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                     ACS_product,"?get=NAME,B25074_033E,B25074_034E,",
                     "B25074_035E,B25074_036E,B25074_037E,B25074_038E,B25074_039E,B25074_040E,",
                     "B25074_041E,B25074_042E,B25074_043E,B25074_044E,B25074_045E,B25074_046E,",
                     "B25074_047E,B25074_048E,B25074_049E,B25074_050E,B25074_051E,B25074_052E,",
                     "B25074_053E,B25074_054E,B25074_055E,B25074_056E,B25074_057E,B25074_058E,",
                     "B25074_059E,B25074_060E,B25074_061E,B25074_062E,B25074_063E,B25074_064E",
                     "&in=state:06&for=county:",county,"&key=",key)

county_own_url1 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                    ACS_product,"?get=NAME,B25095_001E,B25095_002E,B25095_003E,B25095_004E,",
                    "B25095_005E,B25095_006E,B25095_007E,B25095_008E,B25095_009E,B25095_010E,",
                    "B25095_011E,B25095_012E,B25095_013E,B25095_014E,B25095_015E,B25095_016E,",
                    "B25095_017E,B25095_018E,B25095_019E,B25095_020E,B25095_021E,B25095_022E,",
                    "B25095_023E,B25095_024E,B25095_025E,B25095_026E,B25095_027E,B25095_028E,",
                    "B25095_029E,B25095_030E,B25095_031E,B25095_032E","&in=state:06&for=county:",
                    county,"&key=",key)

county_own_url2 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                    ACS_product,"?get=NAME,B25095_033E,B25095_034E,",
                    "B25095_035E,B25095_036E,B25095_037E,B25095_038E,B25095_039E,B25095_040E,",
                    "B25095_041E,B25095_042E,B25095_043E,B25095_044E,B25095_045E,B25095_046E,",
                    "B25095_047E,B25095_048E,B25095_049E,B25095_050E,B25095_051E,B25095_052E,",
                    "B25095_053E,B25095_054E,B25095_055E,B25095_056E,B25095_057E,B25095_058E,",
                    "B25095_059E,B25095_060E,B25095_061E,B25095_062E,B25095_063E,B25095_064E,",
                    "B25095_065E,B25095_066E,B25095_067E,B25095_068E,B25095_069E,B25095_070E,",
                    "B25095_071E,B25095_072E,B25095_073E",
                    "&in=state:06&for=county:",county,"&key=",key)

# Now metros

metro_rent_url1 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                           ACS_product,"?get=NAME,B25074_001E,B25074_002E,B25074_003E,B25074_004E,",
                           "B25074_005E,B25074_006E,B25074_007E,B25074_008E,B25074_009E,B25074_010E,",
                           "B25074_011E,B25074_012E,B25074_013E,B25074_014E,B25074_015E,B25074_016E,",
                           "B25074_017E,B25074_018E,B25074_019E,B25074_020E,B25074_021E,B25074_022E,",
                           "B25074_023E,B25074_024E,B25074_025E,B25074_026E,B25074_027E,B25074_028E,",
                           "B25074_029E,B25074_030E,B25074_031E,B25074_032E",                          
                            "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
                            metro,"&key=",key)

metro_rent_url2 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                           ACS_product,"?get=NAME,B25074_033E,B25074_034E,",
                           "B25074_035E,B25074_036E,B25074_037E,B25074_038E,B25074_039E,B25074_040E,",
                           "B25074_041E,B25074_042E,B25074_043E,B25074_044E,B25074_045E,B25074_046E,",
                           "B25074_047E,B25074_048E,B25074_049E,B25074_050E,B25074_051E,B25074_052E,",
                           "B25074_053E,B25074_054E,B25074_055E,B25074_056E,B25074_057E,B25074_058E,",
                           "B25074_059E,B25074_060E,B25074_061E,B25074_062E,B25074_063E,B25074_064E",
                          "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
                          metro,"&key=",key)

metro_own_url1 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                          ACS_product,"?get=NAME,B25095_001E,B25095_002E,B25095_003E,B25095_004E,",
                          "B25095_005E,B25095_006E,B25095_007E,B25095_008E,B25095_009E,B25095_010E,",
                          "B25095_011E,B25095_012E,B25095_013E,B25095_014E,B25095_015E,B25095_016E,",
                          "B25095_017E,B25095_018E,B25095_019E,B25095_020E,B25095_021E,B25095_022E,",
                          "B25095_023E,B25095_024E,B25095_025E,B25095_026E,B25095_027E,B25095_028E,",
                          "B25095_029E,B25095_030E,B25095_031E,B25095_032E",
                          "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
                          metro,"&key=",key)

metro_own_url2 <- paste0("http://api.census.gov/data/",ACS_year,"/acs",
                          ACS_product,"?get=NAME,B25095_033E,B25095_034E,",
                          "B25095_035E,B25095_036E,B25095_037E,B25095_038E,B25095_039E,B25095_040E,",
                          "B25095_041E,B25095_042E,B25095_043E,B25095_044E,B25095_045E,B25095_046E,",
                          "B25095_047E,B25095_048E,B25095_049E,B25095_050E,B25095_051E,B25095_052E,",
                          "B25095_053E,B25095_054E,B25095_055E,B25095_056E,B25095_057E,B25095_058E,",
                          "B25095_059E,B25095_060E,B25095_061E,B25095_062E,B25095_063E,B25095_064E,",
                          "B25095_065E,B25095_066E,B25095_067E,B25095_068E,B25095_069E,B25095_070E,",
                          "B25095_071E,B25095_072E,B25095_073E",
                          "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
                          metro,"&key=",key)
                     
# Function for bringing in data
# Puts API data into list file
# For length of list file, use first row as header
# Create and append data to data frame. Change null values to "NA."

f.afford <- function(url){  
  furl <- content(GET(url))
  for (i in 1:length(furl)){
    if (i==1) header <- furl [[i]]
    if (i==2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      afford <- data.frame(temp, stringsAsFactors=FALSE)
      names (afford) <- header
    }
    if (i>2){
      temp <- lapply(furl[[i]], function(x) ifelse(is.null(x), NA, x))
      tempdf <- data.frame(temp, stringsAsFactors=FALSE)
      names (tempdf) <- header
      afford <- rbind (afford,tempdf)
    }
  }
  return (afford)
}

#Rent

county_rent1 <- f.afford(county_rent_url1)
county_rent2 <- f.afford(county_rent_url2)
county_rent <- left_join(county_rent1,county_rent2,by=c("NAME", "state", "county")) %>%
  mutate(
  Geography=sapply((strsplit(as.character(NAME),',')),function(x) x[1])) %>%
  select(-state,-county,-NAME) %>%
  filter(if(Filter_Napa=="Yes"){Geography!="Napa County"})

county_rent_merging <- left_join(county_rent1,county_rent2,by=c("NAME", "state", "county")) %>% 
  select(-state,-county) %>% mutate(
  Geography=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  Household_Type="Renter",

  r_lt20=as.numeric(B25074_003E)+as.numeric(B25074_012E)+as.numeric(B25074_021E)+as.numeric(B25074_030E)+
    as.numeric(B25074_039E)+as.numeric(B25074_048E)+as.numeric(B25074_057E),
  
  r_20to34=as.numeric(B25074_004E)+as.numeric(B25074_005E)+as.numeric(B25074_006E)+
    as.numeric(B25074_013E)+as.numeric(B25074_014E)+as.numeric(B25074_015E)+
    as.numeric(B25074_022E)+as.numeric(B25074_023E)+as.numeric(B25074_024E)+
    as.numeric(B25074_031E)+as.numeric(B25074_032E)+as.numeric(B25074_033E)+
    as.numeric(B25074_040E)+as.numeric(B25074_041E)+as.numeric(B25074_042E)+
    as.numeric(B25074_049E)+as.numeric(B25074_050E)+as.numeric(B25074_051E)+
    as.numeric(B25074_058E)+as.numeric(B25074_059E)+as.numeric(B25074_060E),
  
  r_gt35=as.numeric(B25074_007E)+as.numeric(B25074_008E)+as.numeric(B25074_009E)+
    as.numeric(B25074_016E)+as.numeric(B25074_017E)+as.numeric(B25074_018E)+
    as.numeric(B25074_025E)+as.numeric(B25074_026E)+as.numeric(B25074_027E)+
    as.numeric(B25074_034E)+as.numeric(B25074_035E)+as.numeric(B25074_036E)+
    as.numeric(B25074_043E)+as.numeric(B25074_044E)+as.numeric(B25074_045E)+
    as.numeric(B25074_052E)+as.numeric(B25074_053E)+as.numeric(B25074_054E)+
    as.numeric(B25074_061E)+as.numeric(B25074_062E)+as.numeric(B25074_063E),
    
  r_total=as.numeric(B25074_001E)-(as.numeric(B25074_010E)+as.numeric(B25074_019E)+
    as.numeric(B25074_028E)+as.numeric(B25074_037E)+as.numeric(B25074_046E)+as.numeric(B25074_055E)+
      as.numeric(B25074_064E)),
  
  H_Share_lessthan20percent=r_lt20/r_total,
  H_Share_20to34percent=r_20to34/r_total,
  H_Share_morethan35percent=r_gt35/r_total
  ) 
county_rent_appending <- county_rent_merging %>%
  select(Geography,Year,Household_Type,H_Share_lessthan20percent,H_Share_20to34percent,H_Share_morethan35percent)



#Own

county_own1 <- f.afford(county_own_url1)
county_own2 <- f.afford(county_own_url2)
county_own <- left_join(county_own1,county_own2,by=c("NAME", "state", "county")) %>%
  mutate(
    Geography=sapply((strsplit(as.character(NAME),',')),function(x) x[1])) %>%
  select(-state,-county,-NAME) %>%
  filter(if(Filter_Napa=="Yes"){Geography!="Napa County"})

county_own_merging <- left_join(county_own1,county_own2,by=c("NAME", "state", "county")) %>% mutate(
  Geography=sapply((strsplit(as.character(NAME),',')),function(x) x[1]),
  Year=ACS_year,
  Household_Type="Owner",
  
  o_lt20=as.numeric(B25095_003E)+as.numeric(B25095_012E)+as.numeric(B25095_021E)+as.numeric(B25095_030E)+
    as.numeric(B25095_039E)+as.numeric(B25095_048E)+as.numeric(B25095_057E)+as.numeric(B25095_066E),
  
  o_20to34=as.numeric(B25095_004E)+as.numeric(B25095_005E)+as.numeric(B25095_006E)+
    as.numeric(B25095_013E)+as.numeric(B25095_014E)+as.numeric(B25095_015E)+
    as.numeric(B25095_022E)+as.numeric(B25095_023E)+as.numeric(B25095_024E)+
    as.numeric(B25095_031E)+as.numeric(B25095_032E)+as.numeric(B25095_033E)+
    as.numeric(B25095_040E)+as.numeric(B25095_041E)+as.numeric(B25095_042E)+
    as.numeric(B25095_049E)+as.numeric(B25095_050E)+as.numeric(B25095_051E)+
    as.numeric(B25095_058E)+as.numeric(B25095_059E)+as.numeric(B25095_060E)+
    as.numeric(B25095_067E)+as.numeric(B25095_068E)+as.numeric(B25095_069E),
  
  o_gt35=as.numeric(B25095_007E)+as.numeric(B25095_008E)+as.numeric(B25095_009E)+
    as.numeric(B25095_016E)+as.numeric(B25095_017E)+as.numeric(B25095_018E)+
    as.numeric(B25095_025E)+as.numeric(B25095_026E)+as.numeric(B25095_027E)+
    as.numeric(B25095_034E)+as.numeric(B25095_035E)+as.numeric(B25095_036E)+
    as.numeric(B25095_043E)+as.numeric(B25095_044E)+as.numeric(B25095_045E)+
    as.numeric(B25095_052E)+as.numeric(B25095_053E)+as.numeric(B25095_054E)+
    as.numeric(B25095_061E)+as.numeric(B25095_062E)+as.numeric(B25095_063E)+
    as.numeric(B25095_070E)+as.numeric(B25095_071E)+as.numeric(B25095_072E),
  
  o_total=as.numeric(B25095_001E)-(as.numeric(B25095_010E)+as.numeric(B25095_019E)+
    as.numeric(B25095_028E)+as.numeric(B25095_037E)+as.numeric(B25095_046E)+as.numeric(B25095_055E)+
    as.numeric(B25095_064E)+as.numeric(B25095_073E)),
  
  H_Share_lessthan20percent=o_lt20/o_total,
  H_Share_20to34percent=o_20to34/o_total,
  H_Share_morethan35percent=o_gt35/o_total
  ) 

county_own_appending <- county_own_merging %>%
  select(Geography,Year,Household_Type,H_Share_lessthan20percent,H_Share_20to34percent,H_Share_morethan35percent)

#All

temp_rent <- county_rent_merging %>%
  select(-Household_Type)
temp_own <- county_own_merging %>%
  select(-Household_Type)
county_all <- left_join(temp_rent,temp_own,by=c("NAME", "Geography", "Year")) %>% mutate(
  Household_Type="All",
  a_lt20=r_lt20+o_lt20,
  a_20to34=r_20to34+o_20to34,
  a_gt35=r_gt35+o_gt35,
  a_total=r_total+o_total,
  H_Share_lessthan20percent=a_lt20/a_total,
  H_Share_20to34percent=a_20to34/a_total,
  H_Share_morethan35percent=a_gt35/a_total
  ) %>%
  select(Geography,Year,Household_Type,H_Share_lessthan20percent,H_Share_20to34percent,H_Share_morethan35percent)

County_Overall <- rbind(county_rent_appending,county_own_appending,county_all) %>% mutate(
  Source1=source1,
  Source2=source2
  )

# County_byIncome

# Income Value Indices for Naming Brackets

index <- c("0k_10k","10k_19k","20k_34k","35k_49k","50k_74k","75k_99k","more_than_100k")
values <- c("Less than $10,000", "$10,000 to $20,000","$20,000 to $35,000","$35,000 to $50,000","$50,000 to $75,000","$75,000 to $100,000","More than $100,000")


county_byincome_raw <- left_join(county_own, county_rent,by=c("Geography")) %>%
  melt(id.vars=c("Geography"),variable.name="Cell_Name",value.name="HH") %>% mutate(
    Table = sapply((strsplit(as.character(Cell_Name),'_')),function(x) x[1]),
    Temp = sapply((strsplit(as.character(Cell_Name),'_')),function(x) x[2]),
    Cell = as.numeric(sapply((strsplit(as.character(Temp),'E')),function(x) x[1]))) %>%
    filter(Cell %notin% c(1,2,11,20,29,38,47,56,65,10,19,28,37,46,55,64,73)) %>% mutate(
    Income = ifelse (Cell %in% 3:9, "0k_10k",
                    ifelse (Cell %in% 12:18, "10k_19k",
                    ifelse (Cell %in% 21:27, "20k_34k",
                    ifelse (Cell %in% 30:36, "35k_49k",
                    ifelse (Cell %in% 39:45, "50k_74k",
                    ifelse (Cell %in% 48:54, "75k_99k",
                    ifelse (Cell %in% 57:63, "more_than_100k",
                    ifelse (Cell %in% 66:72, "more_than_100k",NA)))))))), 
    Share = ifelse (Cell %in% c(3,12,21,30,39,48,57,66), "Share1_LT20",
                    ifelse (Cell %in% c(4,13,22,31,40,49,58,67), "Share20to24",
                    ifelse (Cell %in% c(5,14,23,32,41,50,59,68), "Share25to29",
                    ifelse (Cell %in% c(6,15,24,33,42,51,60,69), "Share30to34",
                    ifelse (Cell %in% c(7,16,25,34,43,52,61,70), "Share35to39",
                    ifelse (Cell %in% c(8,17,26,35,44,53,62,71), "Share40to49",
                    ifelse (Cell %in% c(9,18,27,36,45,54,63,72), "ShareGT50",NA)))))))
  ) 

County_byIncome <- county_byincome_raw %>%
  select(-Cell_Name,-Table,-Cell,-Temp)%>%
  group_by(Geography,Income,Share) %>%
  summarize(Tot_HH=sum(as.numeric(HH))) %>%
  group_by(Geography,Income) %>%
  mutate(freq = Tot_HH/sum(Tot_HH)) %>%
  dcast(Geography+Income~Share, value.var="freq") %>% 
  mutate(
    Income_Bracket=Income,
    Income_Bracket_Label = values[match(Income, index)],
    H_Share_lessthan20percent = Share1_LT20,
    H_Share_20to24percent = Share20to24,
    H_Share_25to29percent = Share25to29,
    H_Share_30to34percent = Share30to34,
    H_Share_35to39percent = Share35to39,
    H_Share_40to49percent = Share40to49,
    H_Share_morethan50percent = ShareGT50,
    Year=ACS_year,
    Source1=source1,
    Source2=source2) %>%
  select(Geography,Year,Income_Bracket,Income_Bracket_Label,H_Share_lessthan20percent,H_Share_20to24percent,
         H_Share_25to29percent,H_Share_30to34percent,H_Share_35to39percent,H_Share_40to49percent,H_Share_morethan50percent,
         Source1,Source2)

#Now Metro Analysis

#First summarize Bay Area by income and share

Region_byIncome <- county_byincome_raw %>%
  select(-Cell_Name,-Table,-Cell,-Temp)%>%
  group_by(Income,Share) %>%
  summarize(Tot_HH=sum(as.numeric(HH))) %>%
  group_by(Income) %>%
  mutate(freq = Tot_HH/sum(Tot_HH)) %>%
  dcast(Income~Share, value.var="freq") %>% 
  mutate(
    Geography="Bay Area",
    Income_Bracket=Income,
    Income_Bracket_Label = values[match(Income, index)],
    H_Share_lessthan20percent = Share1_LT20,
    H_Share_20to24percent = Share20to24,
    H_Share_25to29percent = Share25to29,
    H_Share_30to34percent = Share30to34,
    H_Share_35to39percent = Share35to39,
    H_Share_40to49percent = Share40to49,
    H_Share_morethan50percent = ShareGT50,
    Year=ACS_year,
    Source1=source1,
    Source2=source2) %>%
  select(Geography,Year,Income_Bracket,Income_Bracket_Label,H_Share_lessthan20percent,H_Share_20to24percent,
         H_Share_25to29percent,H_Share_30to34percent,H_Share_35to39percent,H_Share_40to49percent,H_Share_morethan50percent,
         Source1,Source2)
  
#Next summarize Bay Area by just share  

region_own_rent <- county_byincome_raw %>%
  mutate(
    Household_Type=ifelse(Table=="B25074","Renter",
                      ifelse(Table=="B25095","Owner",NA))) %>%
  select(-Cell_Name,-Table,-Cell,-Temp)%>%
  group_by(Household_Type,Share) %>%
  summarize(Tot_HH=sum(as.numeric(HH))) %>%
  mutate(freq = Tot_HH/sum(Tot_HH)) %>%
  dcast(Household_Type~Share, value.var="freq") 

region_all <- county_byincome_raw %>%
  select(-Cell_Name,-Table,-Cell,-Temp)%>%
  group_by(Share) %>%
  summarize(Tot_HH=sum(as.numeric(HH))) %>%
  mutate(freq = Tot_HH/sum(Tot_HH), Household_Type="All")%>%
  dcast(Household_Type~Share, value.var="freq") 

Region_Overall <- rbind(region_own_rent,region_all) %>%
  mutate(
    Geography="Bay Area",
    H_Share_lessthan20percent = Share1_LT20,
    H_Share_20to24percent = Share20to24,
    H_Share_25to29percent = Share25to29,
    H_Share_30to34percent = Share30to34,
    H_Share_35to39percent = Share35to39,
    H_Share_40to49percent = Share40to49,
    H_Share_morethan50percent = ShareGT50,
    Year=ACS_year,
    Source1=source1,
    Source2=source2) %>%
  select(Geography,Year,Household_Type,H_Share_lessthan20percent,H_Share_20to24percent,
         H_Share_25to29percent,H_Share_30to34percent,H_Share_35to39percent,H_Share_40to49percent,H_Share_morethan50percent,
         Source1,Source2)

#Import and join datasets to create a master dataset for Metros

metro_rent1 <- f.afford(metro_rent_url1) 
metro_rent2 <- f.afford(metro_rent_url2)
metro_own1 <- f.afford(metro_own_url1)
metro_own2 <- f.afford(metro_own_url2)

metro_rent <- left_join(metro_rent1,metro_rent2, by=c("NAME","metropolitan statistical area/micropolitan statistical area"))
metro_own <- left_join(metro_own1,metro_own2, by=c("NAME","metropolitan statistical area/micropolitan statistical area"))
metro_all <- left_join(metro_rent, metro_own, by=c("NAME","metropolitan statistical area/micropolitan statistical area")) 

# Simplify names for easier summing of columns

names(metro_all) <- gsub("B25074_0","r_",names(metro_all))
names(metro_all) <- gsub("B25095_0","o_",names(metro_all))
names(metro_all) <- ifelse(names(metro_all) =="NAME", "NAME", gsub("E","",names(metro_all)))

#Convert all columns to numeric format

for(i in 2:ncol(metro_all)) {
  metro_all[,i] <- as.numeric(metro_all[,i])
}

# Create Metro categories

metro_all_nobay <- metro_all %>% mutate(
  Metro=sapply((strsplit(as.character(NAME),'-')),function(x) x[1]),
  Share1_LT20=r_03+r_12+r_21+r_30+r_39+r_48+r_57+o_03+o_12+o_21+o_30+o_39+o_48+o_57+o_66,
  Share20to24=r_04+r_13+r_22+r_31+r_40+r_49+r_58+o_04+o_13+o_22+o_31+o_40+o_49+o_58+o_67,
  Share25to29=r_05+r_14+r_23+r_32+r_41+r_50+r_59+o_05+o_14+o_23+o_32+o_41+o_50+o_59+o_68,
  Share30to34=r_06+r_15+r_24+r_33+r_42+r_51+r_60+o_06+o_15+o_24+o_33+o_42+o_51+o_60+o_69,
  Share35to39=r_07+r_16+r_25+r_34+r_43+r_52+r_61+o_07+o_16+o_25+o_34+o_43+o_52+o_61+o_70,
  Share40to49=r_08+r_17+r_26+r_35+r_44+r_53+r_62+o_08+o_17+o_26+o_35+o_44+o_53+o_62+o_71,
  ShareGT50=r_09+r_18+r_27+r_36+r_45+r_54+r_63+o_09+o_18+o_27+o_36+o_45+o_54+o_63+o_72,
  Total=(r_01+o_01)-(r_10+r_19+r_28+r_37+r_46+r_55+r_64+o_10+o_19+o_28+o_37+o_46+o_55+o_64+o_73), #Total minus "Not computed"
  H_Share_lessthan20percent=Share1_LT20/Total,
  H_Share_20to24percent=Share20to24/Total,
  H_Share_25to29percent=Share25to29/Total,
  H_Share_30to34percent=Share30to34/Total,
  H_Share_35to39percent=Share35to39/Total,
  H_Share_40to49percent=Share40to49/Total,
  H_Share_morethan50percent=ShareGT50/Total,
  Year=ACS_year,
  Source1=source1,
  Source2=source2
  ) %>%
  select(Metro,Year,H_Share_lessthan20percent,H_Share_20to24percent,H_Share_25to29percent,
         H_Share_30to34percent,H_Share_35to39percent,H_Share_40to49percent,H_Share_morethan50percent,
         Source1,Source2)

region_append <- Region_Overall %>%
  filter(Household_Type=="All") %>%
  select(-Household_Type) %>%
  rename(Metro=Geography)

Metro <- rbind(metro_all_nobay,region_append)

rm(county_all, county_byincome_raw,county_own,county_own_appending,county_own1,county_own2,
   county_rent,county_rent_appending, county_rent_merging,county_own_merging,county_rent1,
   county_rent2,metro_all,metro_all_nobay,metro_own,metro_own1,metro_own2,metro_rent,metro_rent1,
   metro_rent2,region_all,region_append,region_own_rent,temp_own,temp_rent)

# Write out CSV 

write.csv(Region_Overall, paste0(residence_output_csv, Region_Overall_csv), row.names = FALSE, quote = T)
write.csv(Region_byIncome, paste0(residence_output_csv, Region_byIncome_csv), row.names = FALSE, quote = T)
write.csv(County_Overall, paste0(residence_output_csv, County_Overall_csv), row.names = FALSE, quote = T)
write.csv(County_byIncome, paste0(residence_output_csv, County_byIncome_csv), row.names = FALSE, quote = T)
write.csv(Metro, paste0(residence_output_csv, Metro_csv), row.names = FALSE, quote = T)