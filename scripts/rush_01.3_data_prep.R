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
excel_sheets("data/rush_trial_data_2015.xls")# read names of sheets

meadows_2015 <- read_excel("data/rush_trial_data_2015.xls", 
                           sheet = "Mead2015spp", col_names = FALSE)

pastures_2015 <- read_excel("data/rush_trial_data_2015.xls", 
                            sheet = "Past2015spp", col_names = FALSE)



##2016
excel_sheets("data/2016 Rush Trial Data MASTER.XLSX")

meadows_2016 <- read_excel("data/2016 Rush Trial Data MASTER.XLSX", 
                           sheet = "Meadows", col_names = FALSE, skip = 3)

pastures_2016 <- read_excel("data/2016 Rush Trial Data MASTER.XLSX", 
                            sheet = "Pastures", col_names = FALSE, skip = 3)


##2017

excel_sheets("data/2017 Rush Trial Data MASTER.XLSX") 

HP_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Herdship Pasture", col_names = FALSE)
HM_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Herdship Meadow", col_names = FALSE)
LM_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Lingy Hill Meadow", col_names = FALSE)
VP_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Valance Pasture", col_names = FALSE)



# Wrangle data ----


# recode misspellings / duplications
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

### remove blank lines and comments
meadows_2016 <- meadows_2016[-c(7, 83, 84, 153), ]
pastures_2016 <- pastures_2016[-c(7, 83, 84, 154, 155), ]

HP_2017 <- HP_2017[-c(6, 122, 123, 130:141),]
HM_2017 <- HM_2017[-c(6, 15, 122, 123, 127:139),]
LM_2017 <- LM_2017[-c(6, 15, 122:140),]
VP_2017 <- VP_2017[-c(6, 122, 123, 129:140),]


#species group lookup - so far only for 2015 species. Need to add remainder

spp_groups <- rbind(meadows_2015[, c(1,2)], pastures_2015[, c(1,2)])
spp_groups <- unique(spp_groups)
colnames(spp_groups) <- c("species_name", "species_group")
spp_groups <- spp_groups[which(!spp_groups$species_name %in% 
                                 c("Combined", "plot_ID", "Actual Quadrat Number", 
                                   "Type", "Site", "Year", "Sample Ref", 
                                   "Replicate number", "Replicate", "Treatment Plot", 
                                   "Surveyor", "Date", "Location", "Loaction")), ]

spp_groups$species_abbr <- make.cepnames(spp_groups$species_name)

# munge sheets
rushdata_2015 <- full_join(meadows_2015, pastures_2015, by = "X__1")

rushdata_2016 <- full_join(meadows_2016, pastures_2016, by = "X__1")

rushdata_2017 <- full_join(HP_2017, HM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, LM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, VP_2017, by = "X__1")


### add year
rushdata_2015 <- rbind(rushdata_2015[c(1:13),], 
                       c("year", rep("2015", ncol(rushdata_2015)-1)), 
                       rushdata_2015[c(14:nrow(rushdata_2015)),])

rushdata_2016 <- rbind(rushdata_2016[c(1:6),], 
                       c("year", rep("2016", ncol(rushdata_2016)-1)), 
                       rushdata_2016[c(7:nrow(rushdata_2016)),])

rushdata_2017 <- rbind(rushdata_2017[c(1:6),], 
                       c("year", rep("2017", ncol(rushdata_2017)-1)), 
                       rushdata_2017[c(7:nrow(rushdata_2017)),])


# munge years
rushdata <- full_join(rushdata_2015, rushdata_2016, by = "X__1")
rushdata <- full_join(rushdata, rushdata_2017, by = "X__1")

#check for duplicated names
rushdata$X__1[duplicated(rushdata$X__1)] #Lathyrus pratensis
rushdata$X__1[duplicated(rushdata$X__1)] <- "Lathyrus pratensis 2" #Lathyrus pratensis


#save colnames
colnames_rush <- rushdata$X__1

#transform
rushdata <- data.frame(t(rushdata))

#write col names
names(rushdata) <- colnames_rush
colnames(rushdata)
