#Import and prepare rush trial data

#Context
#>HP = Herdship Pasture
#>HM = Herdship Meadow
#>LM = Lingy Hill Meadow
#>VP = Valance Pasture

list.files("data/")

#packages
library(readxl)
library(tidyverse)
library(vegan)
library(lubridate)

#Import data


##2015
excel_sheets("data/2015 Rush Trial Data completed.XLSX")

meadows_2015 <- read_excel("data/2015 Rush Trial Data completed.XLSX", 
                           sheet = "Meadows", col_names = FALSE, skip = 3)

pastures_2015 <- read_excel("data/2015 Rush Trial Data completed.XLSX", 
                            sheet = "Pastures", col_names = FALSE, skip = 3)
#NO TAXON SUMMARIES AVAILABLE

##2016
excel_sheets("data/2016 Rush Trial Data MASTER.XLSX")

meadows_2016 <- read_excel("data/2016 Rush Trial Data MASTER.XLSX", 
                           sheet = "Meadows", col_names = FALSE, skip = 3)

pastures_2016 <- read_excel("data/2016 Rush Trial Data MASTER.XLSX", 
                            sheet = "Pastures", col_names = FALSE, skip = 3)



##2017

excel_sheets("data/2017 Rush Trial Data MASTER.XLSX") # read names of sheets

HP_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Herdship Pasture", col_names = FALSE)
HM_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Herdship Meadow", col_names = FALSE)
LM_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Lingy Hill Meadow", col_names = FALSE)
VP_2017 <- read_excel("data/2017 Rush Trial Data MASTER.XLSX", 
                      sheet = "Valance Pasture", col_names = FALSE)


# Wrangle data

### remove blank lines and comments
meadows_2016 <- meadows_2016[-c(7, 83, 84, 153), ]
pastures_2016 <- pastures_2016[-c(7, 83, 84, 154, 155), ]

HP_2017 <- HP_2017[-c(6, 122, 123, 130:141),]
HM_2017 <- HM_2017[-c(6, 15, 122, 123, 127:139),]
LM_2017 <- LM_2017[-c(6, 15, 122:140),]
VP_2017 <- VP_2017[-c(6, 122, 123, 129:140),]

# munge sheets
rushdata_2017 <- full_join(HP_2017, HM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, LM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, VP_2017, by = "X__1")

rushdata_2016 <- full_join(meadows_2016, pastures_2016, by = "X__1")

### add year
rushdata_2016 <- rbind(rushdata_2016[c(1:6),], 
                       c("year", rep("2016", ncol(rushdata_2016)-1)), 
                       rushdata_2016[c(7:nrow(rushdata_2016)),])

rushdata_2017 <- rbind(rushdata_2017[c(1:6),], 
                       c("year", rep("2017", ncol(rushdata_2017)-1)), 
                       rushdata_2017[c(7:nrow(rushdata_2017)),])

## sort out site names etc. 
unique(paste(rushdata_2016[1,]))
table(paste(rushdata_2016[1,]))
  #do this later, easier in columns

# munge years
rushdata <- full_join(rushdata_2016, rushdata_2017, by = "X__1")

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

  #reorganise
startcols <- c("Loaction", "Replicate number", "Treatment Plot", 
               "Actual Quadrat Number", "Surveyor", 
               "Site (H/L/V)", "Type (M/P)", "Replicate (A/B/C)", 
               "Treatment Plot (1-8)", "Quadrat (1-100)",  
               "Date", "year", "Rush cover", "Grass cover", "Herb cover", 
               "Sedge cover", "Bryophyte cover", "Bare ground", "Litter")
rushdata <- select(rushdata, one_of(startcols),everything())

unique(rushdata$Site)

