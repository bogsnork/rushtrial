#Import and prepare rush trial data

#Context
#>HP = Herdship Pasture
#>HM = Herdship Meadow
#>LM = Lingy Hill Meadow
#>VP = Valance Pasture

list.files("data/")

#packages----
library(readxl)
library(tidyverse)
library(vegan)
library(lubridate)

#Import data-----


##2015
excel_sheets("data/2015 Rush Trial Data completed.XLSX")

meadows_2015 <- read_excel("data/2015 Rush Trial Data completed.XLSX", 
                           sheet = "Meadows", col_names = FALSE, skip = 3)

pastures_2015 <- read_excel("data/2015 Rush Trial Data completed.XLSX", 
                            sheet = "Pastures", col_names = FALSE, skip = 3)
#NO TAXON SUMMARIES AVAILABLE

##2015
excel_sheets("data/2015 Rush Trial Data completed.XLSX")

meadows_2015 <- read_excel("data/2015 Rush Trial Data completed.XLSX", 
                           sheet = "Meadows", col_names = FALSE, skip = 3)

pastures_2015 <- read_excel("data/2015 Rush Trial Data completed.XLSX", 
                            sheet = "Pastures", col_names = FALSE, skip = 3)

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


# Wrangle data ----

### remove blank lines and comments
meadows_2015 <- meadows_2015[-c(7, 83, 84, 148, 149), ]
pastures_2015 <- pastures_2015[-c(7, 83, 84, 151), ]

meadows_2016 <- meadows_2016[-c(7, 83, 84, 153), ]
pastures_2016 <- pastures_2016[-c(7, 83, 84, 154, 155), ]

HP_2017 <- HP_2017[-c(6, 122, 123, 130:141),]
HM_2017 <- HM_2017[-c(6, 15, 122, 123, 127:139),]
LM_2017 <- LM_2017[-c(6, 15, 122:140),]
VP_2017 <- VP_2017[-c(6, 122, 123, 129:140),]

# munge sheets
rushdata_2015 <- full_join(meadows_2015, pastures_2015, by = "X__1")

rushdata_2016 <- full_join(meadows_2016, pastures_2016, by = "X__1")

rushdata_2017 <- full_join(HP_2017, HM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, LM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, VP_2017, by = "X__1")

### add year
rushdata_2015 <- rbind(rushdata_2015[c(1:6),], 
                       c("year", rep("2015", ncol(rushdata_2015)-1)), 
                       rushdata_2015[c(7:nrow(rushdata_2015)),])

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

  #delete row 1
rushdata <- rushdata[-1,]

  #reorganise
startcols <- c("Loaction", "Replicate number", "Treatment Plot", 
               "Actual Quadrat Number", "Surveyor", 
               "Site (H/L/V)", "Type (M/P)", "Replicate (A/B/C)", 
               "Treatment Plot (1-8)", "Quadrat (1-100)",  
               "Date", "year", "Rush cover", "Grass cover", "Herb cover", 
               "Sedge cover", "Bryophyte cover", "Bare ground", "Litter", 
               "Total rush cover", "Total grass cover", "Total herb cover", 
               "Total sedge cover", "Total mosses", "Bare Ground", "Plant Litter")
rushdata <- select(rushdata, one_of(startcols),everything())

  #check for duplicates
names(rushdata)[which(duplicated(names(rushdata)))] 

  #delete NA column
rushdata <- rushdata[ , -198]

# clean variables ----

# clean column names
rushdata <- rename(rushdata,
                   location = Loaction,
                   site = `Site (H/L/V)`, 
                   type = `Type (M/P)`, 
                   replicate = `Replicate (A/B/C)`, 
                   replicate2 = `Replicate number`,
                   treat_plot = `Treatment Plot (1-8)`, 
                   treat_plot2 = `Treatment Plot`,
                   quad = `Quadrat (1-100)`, 
                   quad2 = `Actual Quadrat Number`,
                   date = Date, 
                   surveyor = Surveyor, 
                   rushes = `Rush cover`, 
                   grasses = `Grass cover`, 
                   herbs = `Herb cover`, 
                   sedges = `Sedge cover`, 
                   bare_grd = `Bare ground`, 
                   litter = Litter,
                   bryophytes = `Bryophyte cover`, 
                   rushes2 = `Total rush cover`,
                   grasses2 = `Total grass cover`,
                   herbs2 = `Total herb cover`, 
                   sedges2 = `Total sedge cover`,
                   bryophytes2 = `Total mosses`,
                   bare_grd2 = `Bare Ground`,
                   litter2 = `Plant Litter`) 
  

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



#sort out classes
names(rushdata)
rushdata <- mutate_at(rushdata, funs(as.factor), 
                      .vars = 1:9)
rushdata <- mutate_at(rushdata, funs(as.numeric), .vars = 10:ncol(rushdata))
#date still an issue

# export -----
write.csv(rushdata, "data/rushdata.csv")

