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
library(VIM) #to investigate missing values

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
                "Loaction" = "Location",
                "Calliergon cuspidatum" = "Calliergonella cuspidata",
                "Ranuunculus  repens" = "Ranunculus repens", 
                "Rhytidiadelphus squarosus" = "Rhytidiadelphus squarrosus",
                "Rhytidiadelphus squaros" = "Rhytidiadelphus squarrosus")

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

#remove taxon column
meadows_2015$X__2 <- NULL
pastures_2015$X__2 <- NULL


### remove blank lines and comments 
meadows_2016 <- meadows_2016[-c(7, 83, 84, 153:159), ]
pastures_2016 <- pastures_2016[-c(7, 83, 84, 153), ]

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

#Harmonise site-variable names

rushdata_2016$X__1 <- 
  recode_factor(rushdata_2016$X__1, "Loaction" = "Location")

rushdata_2017$X__1 <- 
  recode_factor(rushdata_2017$X__1,
                "Quadrat (1-100)" = "Actual Quadrat Number", 
                "Treatment Plot (1-8)" = "Treatment Plot", 
                "Replicate (A/B/C)" = "Replicate number")

rushdata_2017[1,] <- toupper(paste0(rushdata_2017[1,], rushdata_2017[2,]))

rushdata_2017$X__1 <- as.character(rushdata_2017$X__1)
rushdata_2017$X__1[1] <- "Location"

#from 2015 remove the following: "Combined", "plot_ID", "Type", "Site", "Year", "Sample Ref", "Replicate", "Surveyor", "Date"

rushdata_2015 <- 
  rushdata_2015[which(!rushdata_2015$X__1 %in%
                        c("Combined", "plot_ID", "Type", "Site", "Year", 
                          "Sample Ref", "Replicate", "Surveyor", "Date")), ]

#from 2016 and 2017 also remove:   "Bare Ground", "Plant Litter", "Total rush cover", "Total grass cover", "Total herb cover", "Total sedge cover", "Rush cover", "Grass cover", "Herb cover", "Sedge cover", "Bare ground", "Litter"

rushdata_2016 <- 
  rushdata_2016[which(!rushdata_2016$X__1 %in%
                       c("Combined", "plot_ID", "Type", "Site", "Year", 
                         "Sample Ref", "Replicate", "Surveyor", "Date",
                         "Bare Ground", "Plant Litter", "Total rush cover", 
                         "Total grass cover", "Total herb cover", 
                         "Total sedge cover", "Total mosses",
                         "Rush cover", "Grass cover", "Herb cover", 
                         "Sedge cover", "Bare ground", "Litter")),]


rushdata_2017 <- rushdata_2017[which(!rushdata_2017$X__1 %in%
                         c("Date", "Surveyor", "Rush cover", "Grass cover", 
                           "Herb cover", "Sedge cover", "Bare ground", "Litter", 
                           "Bryophyte cover")),]



### add year
rushdata_2015 <- rbind(c("year", rep("2015", ncol(rushdata_2015)-1)), 
                       rushdata_2015)

rushdata_2016 <- rbind(c("year", rep("2016", ncol(rushdata_2016)-1)), 
                       rushdata_2016)
rushdata_2016$X__1 <- as.character(rushdata_2016$X__1)
rushdata_2016$X__1[1] <- "year"

rushdata_2017 <- rbind(c("year", rep("2017", ncol(rushdata_2017)-1)), 
                       rushdata_2017)




# munge years
rushdata <- full_join(rushdata_2015, rushdata_2016, by = "X__1")
rushdata <- full_join(rushdata, rushdata_2017, by = "X__1")

#check for duplicated names
rushdata[which(rushdata$X__1 %in% 
                 rushdata$X__1[duplicated(rushdata$X__1)]), 1] 

#[1] "Calliergonella cuspidata"   "Calliergonella cuspidata"   "Calliergonella cuspidata" [4] "Rhytidiadelphus squarrosus" "Lathyrus pratensis"         NA    

#not sure if I need to do anything about these at this stage, or why they are there.  


#save colnames
colnames_rush <- rushdata$X__1

#transform
rushdata_t <- data.frame(t(rushdata))

#write col names
names(rushdata_t) <- colnames_rush
colnames(rushdata_t)

#delete row 1
rushdata_t <- rushdata_t[-1,]

#add temporary rownames
rownames(rushdata_t) <- seq(1:nrow(rushdata_t))


#find duplicate col names
names(rushdata_t)[which(names(rushdata_t) %in% 
                 names(rushdata_t)[duplicated(names(rushdata_t))])] 

#make them unique, and deal with later
names(rushdata_t) <- make.names(names(rushdata_t), unique = TRUE)

#delete some duds
rushdata_t$Site..H.L.V.<- NULL
rushdata_t$Type..M.P. <- NULL
rushdata_t$NA. <- NULL
rushdata_t$NA..1 <- NULL

#reorganise
startcols <- c("year", "Location", "Replicate.number", 
               "Treatment.Plot", "Actual.Quadrat.Number")

rushdata_t <- select(rushdata_t, one_of(startcols),everything())

#have a look where there are missing values
VIM::matrixplot(rushdata_t[1:10], 
                labels = substr(names(rushdata_t[1:10]), 1, 5))

#have a look at the matrix plot, Location column doesn't seem to work, probably need to get some of the other cols from 2016 over.  Or maybe it was that loaction column

# 
# temp <- rushdata_t 
# 
# temp$Location[which(is.na(temp$Location))] <- 
#   temp$Loaction[which(is.na(temp$Location))]
#   


## Clean up the site variables: 

rushdata <- rushdata_t
names(rushdata)[1:10]
# clean column names
rushdata <- rename(rushdata,
                   year = year,
                   location = Location,
                   replicate = Replicate.number,
                   treat_plot = Treatment.Plot,
                   quad = Actual.Quadrat.Number) 
  

table(rushdata$year) # all good

table(rushdata$location)
rushdata$location <- recode(rushdata$location,   #recode 2015 and 2016 data
                            `H'SHIP  MED` = "HM", 
                            `H'SHIP   PAST` = "HP", 
                            `LINGY MED` = "LM", 
                            `VAL PAST` = "VP")
rushdata$location <- droplevels(rushdata$location)
table(rushdata$location)

table(rushdata$replicate)
rushdata$replicate <- toupper(rushdata$replicate)
rushdata$replicate <- recode(rushdata$replicate,   #recode 2015 and 2016 data
                            `A (1)` = "A", 
                            `A(1)` = "A", 
                            `B (2)` = "B", 
                            `B(2)` = "B", 
                            `C(3)` = "C")
table(rushdata$replicate)
rushdata$replicate <- as.factor(rushdata$replicate)

table(rushdata$treat_plot)
rushdata$treat_plot <- droplevels(rushdata$treat_plot)
table(rushdata$treat_plot)

unique(rushdata$quad)

#have a look where there are missing values
VIM::matrixplot(rushdata[1:10], 
                labels = substr(names(rushdata[1:10]), 1, 5))


 #sort out classes
names(rushdata)
rushdata <- mutate_at(rushdata, funs(as.factor),
                      .vars = 1:5)
rushdata <- mutate_at(rushdata, funs(as.numeric), .vars = 6:ncol(rushdata))


# make unique id
uid <- paste(rushdata$location, rushdata$treat_plot, rushdata$replicate, 
             rushdata$quad, rushdata$year, sep = ".")

dupes <- uid[which(duplicated(uid))]

uid[which(uid %in% dupes)]
#some duplicates
#"HP.6.B.48.2015" "HP.6.B.48.2015" "VP.4.C.NA.2015" "VP.4.C.NA.2015" "VP.4.C.NA.2015"
# separate records show HP.6.B in 2015 should have been quads 48, 47, 86.  47 was miscoded as 48.  For the time being we'll call them 48a and 48b.  For VP.4.C in 2015 we'll call them 00a, 00b, 00c

#because quad is a factor variable, we need to add the levels first: 
levels(rushdata$quad) <- c(levels(rushdata$quad), "48a", "48b", 86)
rushdata[which(rushdata$location == "HP" & 
               rushdata$treat_plot == "6" & 
               rushdata$replicate == "B" & 
               rushdata$year == "2015") ,"quad"] <- c("48a", "48b", 86)

levels(rushdata$quad) <- c(levels(rushdata$quad), "00a", "00b", "00c")
rushdata[which(rushdata$location == "VP" & 
                 rushdata$treat_plot == "4" & 
                 rushdata$replicate == "C" & 
                 rushdata$year == "2015") ,"quad"] <- c("00a", "00b", "00c")


# make unique id again
uid <- paste(rushdata$location, rushdata$treat_plot, rushdata$replicate, 
             rushdata$quad, rushdata$year, sep = ".")
dupes <- uid[which(duplicated(uid))]
uid[which(uid %in% dupes)]
#all clear


#add uid to rushdata rownames

rownames(rushdata) <- uid



# export -----
write.csv(rushdata, "data/rushdata.csv", row.names = TRUE)

