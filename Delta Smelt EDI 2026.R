#code to prep the delta smelt edi package
#mainly using this to combine dop and flash data as opposed to in excel
#used previous code that I made for SDWSC

library(tidyverse)
library(readxl)
library(lubridate)
library(hms)
library(stringr)

setwd("C:/Users/cburdi/OneDrive - California Department of Water Resources/Documents/Current Data/EDI Data Posting/Delta Smelt/R/Delta Smelt Diet/")


#read in the dop and flash diet by number from access queries and combine in R
#previously been combined via excel which takes so long and wayyyyy inaccuarate

#presence-absence categories have a separate query since they don't pop up in the prey by number queries

#Diet by Number File----------------------------------------------------

###DOP Data-------

#used query EDI Diet by number in the dop database and converted file to a csv----- annoyingly need to keep it in xlsx format to maintain decimal points. Otherwise sometimes only 2 decimal points are retained

#specimens table has 634 delta smelt up until 2025
#includes cvp and swp fish

dop = read_xlsx("Data for R/DOP/DOP EDI Qry Diet by Number.xlsx")%>% #adding in the check names argument is needed for when reading a csv so it doesn't remove the spaces in the prey columns
  mutate(Time = as_hms(ymd_hms(Time, tz="America/Los_Angeles"))) %>% #convert time. need to have the mdy_hms in there because it wants it first as a date/time instead of a character
  mutate(Date = date(Date),
         DietStudy = "DOP", 
         Year = as.numeric(Year), 
         Month = as.numeric(Month)) %>% 
  filter(Species %in% c( "DELSME","DELSME x WAKASA")) %>% #one is a genetic hybrid, but morph ID'd as DS so adding here
  filter(Year<2024) %>% 
  filter(GutContents !="") %>% #not including the error fish
  rename(LogNumber = DOPLogNumber) %>% 
  select(-c(`<>`, Species)) #has this weird prey category. I don't know what it is but it has nothing in the column

#need to add in presence-absence columns separately I guess

dop_pa = read_xlsx("Data for R/DOP/DOP EDI Qry_Presence_Absence Categories to Post.xlsx") %>% 
  mutate(Time = as_hms(ymd_hms(Time, tz="America/Los_Angeles"))) %>% 
  # mutate(Date2 = mdy_hms(Date)) %>% #need to convert to date, but also has the time in there which is why use _hm
  mutate(Date = date(Date),
         DietStudy = "DOP", 
         Year = as.numeric(Year), 
         Month = as.numeric(Month)) %>% 
  rename(LogNumber = DOPLogNumber, 
         Debris = `Debris (sand/silt/mud)`,
         "Stomach tissue" = `Stomach/Gut Tissue`) %>% 
  select(DietStudy, LogNumber, Year:SerialNumber, Time, Debris:`Unid plant material`)
  
#merge the two datasets

dopall = dop %>% 
  left_join(., dop_pa, by = c('DietStudy', 'LogNumber', 'Year', 'Month', 'Date', 'Station', 'SerialNumber', 'Time')) %>% 
  mutate(LogNumber = as.character(str_pad(LogNumber, width = 4, side = "left", pad = "0")) )%>% #add preceding zeros to the log numbers. also need to make it a character since flash has it that way
  mutate(UniqueID = paste(DietStudy, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) %>%  #make a unique ID
  mutate(SurfacePPT = as.numeric(SurfacePPT), 
         BottomPPT = as.numeric(BottomPPT)) %>% 
  rename(CultureOrigin = CulturedOrigin) %>% 
  select(UniqueID, DietStudy, LogNumber, Project:SerialNumber, CultureOrigin, Depth:DigestionRank, Debris: `Unid plant material`, `Acanthocyclops spp`: `Unid mysids` )

#numbers look good, but just checking to make sure there's no duplicates

dopdups = dop %>% 
  group_by(LogNumber) %>% 
  summarise(n = n()) %>%
  filter(n>1)

#######Flash Data--------

#ran edi by number query in the database. this results in duplicates for ges samples which I'll need to remove down below

flash = read_xlsx("Data for R/FLaSH/FLaSH EDI Qry_Diet by Number.xlsx") %>% 
  rename(LogNumber = FLaSHLogNumber)%>% 
  mutate(Time = as_hms(ymd_hms(Time, tz="America/Los_Angeles"))) %>% 
  filter(GutContents !="") %>% 
  mutate(Date= date(Date), 
         TotalPreyWeight = as.numeric(TotalPreyWeight), 
         GutFullness = as.numeric(GutFullness)) %>% 
  mutate(DietStudy = "FLaSH")


#flash queries don't put out empties for some reason so need to add them

flashempty = read_xlsx("Data for R/FLaSH/FLaSH EDI Qry_Empties with Enviro.xlsx") %>% 
  mutate(Time = as_hms(ymd_hms(Time, tz="America/Los_Angeles"))) %>% 
  mutate(SerialNumber = as.character(SerialNumber),
         TotalNumberOfPrey = as.numeric(TotalNumberOfPrey), 
         TotalPreyWeight = as.numeric(TotalPreyWeight), 
         GutFullness = as.numeric(GutFullness)) %>% 
  rename(LogNumber = FLaSHLogNumber) %>% 
  mutate(Date = date(Date), 
         DietStudy = "FLaSH") %>% 
  rename(SurfacePPT = PPTSurf) %>% 
  select(-Species)

#combine the all the flash files so far, two files so far

flash2 = bind_rows(flash, flashempty)

#need to add pa categories

flashpa = read_xlsx("Data for R/FLaSH/FLaSH EDI Qry_Presence_Absence Categories.xlsx") %>% 
  select(-`Stomach empty`) %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate(DietStudy = "FLaSH", 
         Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         Station = as.character(Station)) %>% 
  rename(LogNumber = FLaSHLogNumber, 
         Debris = `Debris (sand/silt/mud)`,
         "Stomach tissue" = `Stomach/Gut Tissue`) %>% 
  select(DietStudy, LogNumber, Year:SerialNumber, Time, Debris:`Worm pieces`)

#combine all the flash files 

flash3= flash2 %>% 
  left_join(., flashpa, by = c('DietStudy', 'LogNumber', 'Year', 'Month', 'Date', 'Station', 'SerialNumber', 'Time')) %>% 
  mutate(LogNumber = str_pad(LogNumber, width = 4, side = "left", pad = "0")) #add preceding zeros to the log numbers


#check for duplicates----- all ges

fdups = flash3%>% 
  group_by(LogNumber, Project) %>% 
  summarise(n = n()) %>%
  filter(n>1)



####GES samples-----

#separate out ges samples. #hopefully this only needs to be done for this first pub
#want to have the flash all file with only ges so I can filter out the correct ones, and then can add in with the other non ges
#prolly a better way to do this but easiest for now

flash_noges = flash3 %>% 
  filter(Project != "GES")

flash_ges= flash3 %>% 
  filter(Project == 'GES') 

#make a csv of ges samples so can connect them to the right station/ time
#not all of these are duplicates

gessumm = flash_ges %>% 
  group_by(LogNumber, Project, Station, Date) %>% 
  summarise(n = n())

# 
# write.csv(gessumm, "Outputs/Error Checks/gessum.csv", row.names = FALSE)


#looked up all the ges samples and connected them to time so I can filter them out
#pull in those files so can filter out the ones that aren't correct

gescheck = read_xlsx("Data for R/GES Sample Details.xlsx") %>% 
  mutate(Time = as_hms(Time)) %>%
  mutate(Date = date(Date), 
         Station = as.character(Station), #change all these to match with the other file
         LogNumber = as.character(LogNumber)) %>% 
  select(LogNumber, Project:Date, Time)


#join the file with the correct times/dates with the diet by number

ges = flash_ges %>% 
  right_join(., gescheck, by= c("LogNumber", "Project", "Date", "Time", "Station"))


#add all flash files together again

flash4 = flash_noges %>% 
  rbind(ges) %>% 
  mutate(BottomTemperature = as.numeric(BottomTemperature), 
         BottomPPT = as.numeric(BottomPPT))


####Sample Number Check-----

#looking at total records here and total in specimen table in db we're missing some samples
# so doing a sample check for what's missing

#do sample list query in flash

samples = read.csv("Data for R/FLaSH/FLaSH EDI Qry_Sample List.csv") %>% 
  mutate(LogNumber = str_pad(FLaSHLogNumber, width = 4, side = "left", pad = "0")) %>% 
  select(-FLaSHLogNumber) 

#whats missing in the flash all file
missfsamps = samples %>% 
  filter(!LogNumber %in% flash4$LogNumber )

#missing 8 samples which are all samples that just have pa prey
#need to bring these in separately
#need to do it with the original pa csv since the already edited one needed to select out some columns

misspa = read_xlsx("Data for R/FLaSH/FLaSH EDI Qry_Presence_Absence Categories.xlsx") %>% 
  select(-`Stomach empty`) %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate(DietStudy = "FLaSH", 
         Year = as.numeric(Year), 
         Month = as.numeric(Month), 
         Station = as.character(Station)) %>% 
  rename(LogNumber = FLaSHLogNumber, 
         Debris = `Debris (sand/silt/mud)`,
         "Stomach tissue" = `Stomach/Gut Tissue`) %>% 
  mutate(LogNumber = str_pad(LogNumber, width = 4, side = "left", pad = "0")) %>% 
  filter(!LogNumber %in% flash4$LogNumber)  %>% 
  mutate(BottomTemperature = as.numeric(BottomTemperature), 
         BottomPPT = as.numeric(BottomPPT)) %>% 
  select(-Species)


missfsamps2 = flash4 %>% 
  filter(!LogNumber %in% samples$LogNumber )

#no samples that are in the flash all file that are in samples


####All FLash-----

#combine the flash4 file with the missing pa samples

flashall = flash4 %>% 
  full_join(., misspa) %>% 
  mutate(UniqueID = paste(DietStudy, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) %>% 
  rename(CultureOrigin = CulturedOrigin)
  
#check for duplicates again. 

flashdups = flashall %>% 
  group_by(LogNumber, SerialNumber, Project, Station, Date) %>%
  summarise(n = n()) %>%
  filter(n>1)

#wooooooooo no duplicates


##All Combined----

#add all together and create a unique ID

numball = bind_rows(flashall, dopall) %>%
  select (UniqueID, DietStudy, LogNumber, Project, GearType, Year, Month, Date, Time, Station, SerialNumber, CultureOrigin, Depth, SurfaceTemperature, SurfaceConductivity, SurfacePPT, BottomTemperature, BottomConductivity, BottomPPT, Secchi,	Turbidity, TotalBodyWeight, Length, GutContents, TotalGutContentWeight, TotalNumberOfPrey, TotalPreyWeight, GutFullness, FullnessRank, DigestionRank, Debris, `Unid animal material`,	`Unid plant material`, `Stomach tissue`, `Worm pieces`, `Acanthocyclops spp`, `Acartia copepodid`,  `Acartia spp`,  `Acartiella copepodid`, `Acartiella sinensis`, `Barnacle nauplii`,  `Bosmina spp`, `Calanoid copepodid`, `Ceriodaphnia spp`,  `Chironomid larvae`, Clams, `Copepod nauplii`, `Corophium type`, `Crab zoea`, Cumaceans, `Cyclopoid copepodid`, `Daphnia spp`, `Diaphanosoma spp`, `Diaptomus copepodid`, `Diaptomus spp`, `Eurytemora copepodid`, `Eurytemora nauplii`, `Eurytemora spp`, `Fish eggs`, `Gammarus type`, Harpacticoids, `Hyperacanthomysis longirostris`,  Isopods, `Limnoithona copepodid`, `Limnoithona spp`, `Longfin Smelt`, `Neomysis kadiakensis`, `Neomysis mercedis`, `Nippoleucon hinumensis`, `Oithona copepodid`, `Oithona davisae`, `Osphranticum`, `Ostracods`, `Other calanoid`, `Other cladocera`, `Other cyclopoid`, `Other insect larvae`, `Other malacostraca`, `Other rotifer`, `Other zooplankton`, `Pacific Herring`, Palaemon, `Prickly Sculpin`, `Pseudodiaptomus copepodid`, `Pseudodiaptomus forbesi`, `Pseudodiaptomus marinus`, `Pseudodiaptomus nauplii`, `Pseudodiaptomus spp`, `Sinocalanus copepodid`, `Sinocalanus nauplii`, `Sinocalanus spp`, Synchaeta, `Terrestrial invertebrates`, `Tortanus copepodid`, `Tortanus dextrilobatus`, `Tortanus spp`, `Tridentiger spp`, `Unid amphipod`, `Unid calanoid`, `Unid cladocera`, `Unid copepod`, `Unid cyclopoid`, `Unid fish`, `Unid mysids`) %>% #listing the exact way that it's in the metadata doc so I don't have to keep changing this based on how the different df pop out and adding it first so formating code can happen
  mutate_at(vars('Acanthocyclops spp' :`Unid mysids` ), ~replace(., is.na(.), 0)) %>%  #need to replace the NAs with 0s in some of the prey columns that are in one df but not the other
  mutate(CultureOrigin = case_when(CultureOrigin %in% c("n", "No") ~ "U",
                                    CultureOrigin %in% c( "y", "Yes", "AdClipped", "VIE", "Y")~ "M",
                                    Date < "2021-12-15" ~"NA", #first release date
                                    .default = CultureOrigin)) %>% #making it so all cultured column is either marked, unmarked or NA for pre supplementation
  mutate(GutContents = case_when(GutContents %in% c("n", "No") ~ "N",
                                 GutContents %in% c( "y", "Yes")~ "Y",
                                  .default = GutContents)) %>% #consistent capitalized Y N
  mutate(GutFullness = case_when(TotalNumberOfPrey == 0 ~ 0, 
                                 .default = GutFullness)) %>% 
  mutate(Debris = if_else(is.na(Debris), "N", "Y"), #change it to yes/no
         `Stomach tissue` = if_else(is.na(`Stomach tissue`), "N", "Y"), 
         `Unid animal material`= if_else(is.na(`Unid animal material`), "N", "Y"), 
         `Unid plant material` = if_else(is.na(`Unid plant material`), "N", "Y"),
         `Worm pieces` = if_else(is.na(`Worm pieces`), "N", "Y")) %>% 
  filter(Year<2024)#Only up to 2023 has been fully QC'd

#for some reason the log numbers don't retain the preceeding zeros

write.csv(numball, "Outputs/Delta Smelt Diet Data 2011to2023.csv", row.names = FALSE)

##Error Checks-----

#want to verify a few things for the diet by number file

#check that if GC = N, the there is no prey

emptycheck = numball %>% 
  filter(GutContents == "N" & TotalNumberOfPrey != 0)

#check that all the prey columns add up to zero, if GC = N

zerosum = numball %>% 
  mutate(totprey = rowSums(across(c(`Acanthocyclops spp`: `Unid mysids`)))) %>% 
  filter(GutContents == "N") %>% 
  filter(totprey !=0)

#check that all GC=Y have prey

zeroprey = numball %>% 
  mutate(totprey = rowSums(across(c(`Acanthocyclops spp`: `Unid mysids`)))) %>% 
  filter(GutContents == "Y") %>% 
  filter(totprey ==0)

#make csv of GC Y and have prey

write.csv(zeroprey, "Outputs/Error Checks/zeroprey.csv", row.names = FALSE)

#all are fish with presence absence cats only so ok

#Gut fullness = 0, if sum of prey = 0

guterror = numball %>% 
  filter(GutFullness == 0 & TotalNumberOfPrey != 0) #all good

#check if there are any NA for gut fullness

gutna = numball %>% 
  filter(is.na (GutFullness)) #12 records that need to verify. make csv to do so

write.csv(gutna, "gutcheck.csv")
 #all of these have no body weight so gf can't be calculated
#or there's non quantifiable prey


#Prey Lengths-----------

#did queries in access for all prey lengths and then converted to csv

#also want to add antennae lengths to this file

##DOP Lengths----

dop_lengths = read.csv("Data for R/DOP/DOP EDI Qry_Prey Lengths to Post.csv", check.names = FALSE) %>% 
  mutate(DietStudy = "DOP",   #adding a database column
         Date2 = mdy_hms(Date),  #need to do the same thing with the dates like i did the diet by number files
         Date = date(Date2)) %>%  #now move it to just the date, no time 
  rename(LogNumber = DOPLogNumber, 
         Comments = PreyLengthComments) %>% 
  mutate(LogNumber = str_pad(LogNumber, width = 4, side = "left", pad = "0")) %>% #add preceding zeros to the log numbers
  mutate(UniqueID = paste(DietStudy, LogNumber, Project, Date,
                          Station, SerialNumber, sep = " ")) %>% 
  select(UniqueID, DietStudy, LogNumber, Project, Station, Date, SerialNumber, PreyCategory, PreyLengthSpecies, PreyLength, LengthEstimate, PreyWeight, EyeDiameter, PreyAntennaLength, PreySex, Comments ) 


##Flash Lengths----

#there are some lengths in here that are NA. Especially for Cumaceans. Need to verify

flash_lengths = read.csv("Data for R/FLaSH/FLaSH Qry_EDI Prey Lengths.csv", check.names = FALSE) %>% 
  mutate(DietStudy = "FLaSH",   #adding a database column
         Date2 = mdy_hms(Date),  #need to do the same thing with the dates like i did the diet by number files
         Date = date(Date2)) %>%  #now move it to just the date, no time 
  rename(LogNumber = FLaSHLogNumber, 
         Time = MinOfTowTime) %>% 
  mutate(LogNumber = str_pad(LogNumber, width = 4, side = "left", pad = "0")) %>% #add preceding zeros to the log numbers
  mutate(UniqueID = paste(DietStudy, LogNumber, Project, Date,
                          Station, SerialNumber, sep = " ")) %>% 
  select(UniqueID, DietStudy, LogNumber, Project, Station, Date, SerialNumber, PreyCategory, PreyLength, LengthEstimate, PreyWeight, EyeDiameter, Comments )

#need to add anntennae lengths to the flash file
#some of these have an NA length. need to check again with new database

flashantenn = read.csv("Data for R/FLaSH/FLaSH Qry_EDI Antennae Lengths.csv", check.names = FALSE) %>% 
  mutate(DietStudy = "FLaSH",   #adding a database column
         Date2 = mdy_hms(Date),  #need to do the same thing with the dates like i did the diet by number files
         Date = date(Date2)) %>%  #now move it to just the date, no time 
  rename(LogNumber = FLaSHLogNumber, 
         Time = MinOfTowTime) %>% 
  mutate(LogNumber = str_pad(LogNumber, width = 4, side = "left", pad = "0")) %>% #add preceding zeros to the log numbers
  mutate(UniqueID = paste(DietStudy, LogNumber, Project, Date,
                          Station, SerialNumber, sep = " ")) %>% 
  select(UniqueID, DietStudy, LogNumber, Project, Station, Date, SerialNumber, PreyCategory, PreyLength, PreyLengthSpecies, PreyAntennaLength, PreySex)

#check if there are antennae lengths not in the length file

lengthcheck = flashantenn %>% 
  filter(!UniqueID %in% flash_lengths$UniqueID) #no antenntae lengths not in the length file. Perfect

#combine flash lengths and antennae lengths

f_alllengths = flash_lengths %>% 
  left_join(., flashantenn, by = c('UniqueID', 'DietStudy', 'LogNumber', 'Project', 'Station', 'Date', 'SerialNumber', 'PreyCategory', 'PreyLength'))


#combine all lengths
lengths = rbind(dop_lengths, f_alllengths) %>% 
  filter(UniqueID %in% numball$UniqueID) %>%  #only keep lengths for specimens that are in the main file
  mutate(PreyLengthSpecies = case_when(PreyLengthSpecies == "juvenile Gammarus" ~ NA, 
                                       PreyLengthSpecies == "juvenile Corophium" ~ NA, 
                                       PreyLengthSpecies == "Unid juvenile" ~ NA, 
                                       PreyLengthSpecies == "" ~ NA, 
                                       PreyLengthSpecies == "Unid Amphipod" ~ "Unid amphipod",
                                       PreyLengthSpecies %in% c("Unid Corophium", "Unid Corophium type") ~ "Unid Corophium type",
                                       PreyLengthSpecies %in% c("Unid gammarus type", "Unid Gammarus") ~ "Unid Gammarus type",
                                       PreyLengthSpecies == "Gammarus spp." ~ "Gammarus spp", 
                                       
                                       .default = PreyLengthSpecies)) %>% #change it so we remove the juvenile category and the others match
  # mutate(RoutinelyMeasured = if_else(PreyCategory %in% c("Hyperacanthomysis longirostris", "Unid mysids", "Corophium type", "Gammarus type", "Unid amphipod", "Chironomid larvae", "Isopods", "Terrestrial invertebrates", "Palaemon", "Pacific Herring", "Unid fish", "Prickly Sculpin", "Neomysis mercedis", "Longfin Smelt", "Tridentiger spp", "Neomysis kadiakensis"), "Y", "N")) %>% #adding a column for whether the prey is regularly measured or its just a one off
  filter(!PreyCategory %in% c("Unid plant material", "Unid animal material", "Annelid worms", "Other zooplankton", "Ostracods", "Crab zoea"))   #removed some of the presence/absence categories and ones with just one or two lengths
  #would normally remove NA lengths but keeping them so people know that it wasn't missed


write.csv(lengths, file = "Outputs/Delta Smelt Prey Lengths.csv", row.names = FALSE)

##Error Checks----

#missing lengths

misslengths = lengths %>% 
  filter(is.na(PreyLength)) %>% 
  filter(!is.na(EyeDiameter))

#all good

#check that all critters that require lengths, have them

lcheck = lengths

#Station List------

#want to make sure our station list has coordinates for all listed stations

stations = read_xlsx ("Data for R/Delta Smelt Diet Station Lookup_NKU07Apr2026_v2.xlsx")

stationcheck = numball %>% 
  group_by(Project, Station) %>% 
  summarise(n = n()) %>% 
  merge(., stations) %>% 
  select(-n)

##Error Checks-------

#check for missing coords

stationerror = stationcheck %>% 
  filter(is.na(Longitude))

#no missing coords. nice

#make sure no orphan stations

stationerror2 = stationcheck %>% 
  






