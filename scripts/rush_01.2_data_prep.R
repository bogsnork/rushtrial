#Import and prepare rush trial data

#Context
#>HP = Herdship Pasture
#>HM = Herdship Meadow
#>LM = Lingy Hill Meadow
#>VP = Valance Pasture

#packages----
library(readxl)
library(tidyverse)
library(vegan)
library(lubridate)

#Import data-----

list.files("data/")

##2015
excel_sheets("data/rush_trial_data_2015.xls")

meadows_2015 <- read_excel("data/rush_trial_data_2015.xls", 
                           sheet = "Mead2015spp", col_names = FALSE)

pastures_2015 <- read_excel("data/rush_trial_data_2015.xls", 
                            sheet = "Past2015spp", col_names = FALSE)


#clean data

#need to clean location / loaction; rytidiadelphus sq is spellt three different ways.  



# Wrangle data ----

meadows_2015$X__1 <- 
  recode_factor(meadows_2015$X__1,
                "Calliergon cuspidatum" = "Calliergonella cuspidata",
                "Ranuunculus  repens" = "Ranunculus repens", 
                "Rhytidiadelphus squarosus" = "Rhytidiadelphus squarrosus",
                "Rhytidiadelphus squaros" = "Rhytidiadelphus squarrosus")


pastures_2015$X__1 <- 
  recode_factor(pastures_2015$X__1,
                "Calliergon cuspidatum" = "Calliergonella cuspidata",
                "Ranuunculus  repens" = "Ranunculus repens", 
                "Rhytidiadelphus squarosus" = "Rhytidiadelphus squarrosus",
                "Rhytidiadelphus squaros" = "Rhytidiadelphus squarrosus")


#species group lookup

spp_groups <- rbind(meadows_2015[, c(1,2)], pastures_2015[, c(1,2)])
spp_groups <- unique(spp_groups)
colnames(spp_groups) <- c("species_name", "species_group")
spp_groups <- spp_groups[which(!spp_groups$species_name %in% 
                                   c("Combined", "plot_ID", "Actual Quadrat Number", 
                                     "Type", "Site", "Year", "Sample Ref", 
                                     "Replicate number", "Replicate", "Treatment Plot", 
                                     "Surveyor", "Date", "Location", "Loaction")), ]

spp_groups$species_abbr <- make.cepnames(spp_groups$species_name)

#transpose
meadows_2015_t <- t(meadows_2015) 
colnames(meadows_2015_t) <- c("combined_id", "plot_ID", "quad", "type", 
                              "site", "year", "sample_ref", "replicate_nr",
                              "replicate", "treatment", "surveyor", "date",
                              "location", meadows_2015_t[1,-c(1:13)])

colnames(meadows_2015_t)[11:ncol(meadows_2015_t)] <- 
  make.cepnames(colnames(meadows_2015_t)[11:ncol(meadows_2015_t)]) 


pastures_2015_t <- t(pastures_2015) 
colnames(pastures_2015_t) <- c("combined_id", "plot_ID", "quad", "type", 
                              "site", "year", "sample_ref", "replicate_nr",
                              "replicate", "treatment", "surveyor", "date",
                              "location", pastures_2015_t[1,-c(1:13)])

colnames(pastures_2015_t)[11:ncol(pastures_2015_t)] <- 
  make.cepnames(colnames(pastures_2015_t)[11:ncol(pastures_2015_t)]) 

# remove unwanted data
meadows_2015_t <- meadows_2015_t[-c(1:2),]
pastures_2015_t <- pastures_2015_t[-c(1:2),]


#make tidy and merge

meadows_2015_t <- meadows_2015_t %>% 
  data.frame() %>% 
  gather(key = "species_abbr", value = "cover", 14:ncol(meadows_2015_t))


pastures_2015_t <- pastures_2015_t %>% 
  data.frame() %>% 
  gather(key = "species_abbr", value = "cover", 14:ncol(pastures_2015_t))

species_2015 <- rbind(meadows_2015_t, pastures_2015_t)












# clean columns
#convert to upper case
rushdata$site <- toupper(rushdata$site)
rushdata$type <- toupper(rushdata$type)
rushdata$replicate <- toupper(rushdata$replicate)


#drop unused levels
rushdata <- droplevels(rushdata)
unique(rushdata$site)

#sort out classes
names(rushdata)
rushdata <- mutate_at(rushdata, funs(as.factor), 
                      .vars = 1:26)
rushdata <- mutate_at(rushdata, funs(as.numeric), .vars = 27:ncol(rushdata))
#date still an issue



# Merge columns ----

#location
rushdata$location2 <- paste(rushdata$site, rushdata$type, sep = "")

table(rushdata$location2, useNA = "ifany")
table(rushdata$location, useNA = "ifany")

rushdata$location <- recode(rushdata$location,   #recode 2015 and 2016 data
                            `H'SHIP  MED` = "HM", 
                            `H'SHIP   PAST` = "HP", 
                            `LINGY MED` = "LM", 
                            `VAL PAST` = "VP")
rushdata$location[is.na(rushdata$location)] <- rushdata$location2[is.na(rushdata$location)]
#paste in 2017 data

table(rushdata$location) #check
rushdata$location2 <- NULL #delete duplicate col
rushdata$site <- NULL #delete site column as it duplicates location col

#replicate

table(rushdata$replicate2, useNA = "ifany")
table(rushdata$replicate, useNA = "ifany")

rushdata$replicate2 <- recode(rushdata$replicate2,   #recode 2015 and 2016 data
                              `A (1)` = "A",
                              `A(1)` = "A",
                              `B (2)` = "B",
                              `B(2)` = "B",
                              `C(3)` = "C")
rushdata$replicate[is.na(rushdata$replicate)] <- 
  rushdata$replicate2[is.na(rushdata$replicate)]  #paste in 2017 data

table(rushdata$replicate) #check
rushdata$replicate2 <- NULL #delete duplicate col


#type
table(rushdata$type, rushdata$location, useNA = "ifany") #check
rushdata$type <- substring(rushdata$location, 2) #take last letter of location
table(rushdata$type, rushdata$location, useNA = "ifany") #check

#treatment
table(rushdata$treat_plot, useNA = "ifany")
table(rushdata$treat_plot2, useNA = "ifany")

rushdata$treat_plot[is.na(rushdata$treat_plot)] <- 
  rushdata$treat_plot2[is.na(rushdata$treat_plot)]  #paste in 2017 data

table(rushdata$treat_plot) #check
rushdata$treat_plot2 <- NULL #delete duplicate col

#make new treatment variable
rushdata$treatment <- as.factor(paste(rushdata$type, rushdata$treat_plot, sep = ""))

#quadrat

table(rushdata$quad2, useNA = "ifany")
table(rushdata$quad, useNA = "ifany")

rushdata$quad[is.na(rushdata$quad)] <- 
  rushdata$quad2[is.na(rushdata$quad)]  #paste in 2017 data

table(rushdata$quad, useNA = "ifany") #check
rushdata$quad2 <- NULL #delete duplicate col

#rushes
table(rushdata$rushes, useNA = "ifany")
table(rushdata$rushes2, useNA = "ifany")
rushdata$rushes[is.na(rushdata$rushes)] <- 
  rushdata$rushes2[is.na(rushdata$rushes)]  #paste in 2017 data
rushdata$rushes2 <- NULL
table(rushdata$rushes, useNA = "ifany")

#grasses
rushdata$grasses[is.na(rushdata$grasses)] <- 
  rushdata$grasses2[is.na(rushdata$grasses)]  #paste in 2017 data
rushdata$grasses2 <- NULL

#herbs
rushdata$herbs[is.na(rushdata$herbs)] <- 
  rushdata$herbs2[is.na(rushdata$herbs)]  #paste in 2017 data
rushdata$herbs2 <- NULL

#sedges
rushdata$sedges[is.na(rushdata$sedges)] <- 
  rushdata$sedges2[is.na(rushdata$sedges)]  #paste in 2017 data
rushdata$sedges2 <- NULL
table(rushdata$sedges, useNA = "ifany")

#bryophytes
table(rushdata$bryophytes, useNA = "ifany")
rushdata$bryophytes[is.na(rushdata$bryophytes)] <- 
  rushdata$bryophytes2[is.na(rushdata$bryophytes)]  #paste in 2017 data
rushdata$bryophytes2 <- NULL
table(rushdata$bryophytes, useNA = "ifany")


#bare_grd
table(rushdata$bare_grd, useNA = "ifany")
rushdata$bare_grd[is.na(rushdata$bare_grd)] <- 
  rushdata$bare_grd2[is.na(rushdata$bare_grd)]  #paste in 2017 data
rushdata$bare_grd2 <- NULL
table(rushdata$bare_grd, useNA = "ifany")

#litter
table(rushdata$litter, useNA = "ifany")
rushdata$litter[is.na(rushdata$litter)] <- 
  rushdata$litter2[is.na(rushdata$litter)]  #paste in 2017 data
rushdata$litter2 <- NULL
table(rushdata$litter, useNA = "ifany")



# Final prettyfication ----

# reorganise
startcols <- c("location", "type", "replicate", "treatment", "treat_plot",
               "quad", "year" ,"date", "surveyor")
rushdata <- select(rushdata, one_of(startcols),everything())

# Abbreviate species names 
#make species list lookup
head(names(rushdata), 20) #check where species start
#spp start at col 17
spp_list <- data.frame(names(rushdata)[17:ncol(rushdata)])
names(spp_list) <- "spp_name"
spp_list$spp_abbr <- vegan::make.cepnames(spp_list$spp_name)
duplicated(spp_list$spp_abbr)

#rename species columns
names(rushdata)[17:ncol(rushdata)] <- spp_list$spp_abbr


#sort out classes
names(rushdata)
rushdata <- mutate_at(rushdata, funs(as.factor), 
                      .vars = 1:9)
rushdata <- mutate_at(rushdata, funs(as.numeric), .vars = 10:ncol(rushdata))
#date still an issue

# export -----
write.csv(rushdata, "data/rushdata.csv")

