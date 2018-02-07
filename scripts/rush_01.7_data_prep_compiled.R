#Import and prepare rush trial data

# rush_01.3_data_prep.R ----

#Context ----
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


## recode misspellings / duplications ----
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

# Make species lookup ----

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

# Remove unnecessary rows and columns ----

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

# Combine sheets ----
rushdata_2015 <- full_join(meadows_2015, pastures_2015, by = "X__1")

rushdata_2016 <- full_join(meadows_2016, pastures_2016, by = "X__1")

rushdata_2017 <- full_join(HP_2017, HM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, LM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, VP_2017, by = "X__1")

#Harmonise site-variable names ----

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



## add year ----
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


# finalise variable names ----

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


# missing values ----

#have a look where there are missing values
VIM::matrixplot(rushdata_t[1:10], 
                labels = substr(names(rushdata_t[1:10]), 1, 5))

#have a look at the matrix plot, Location column doesn't seem to work, probably need to get some of the other cols from 2016 over.  Or maybe it was that loaction column


## Clean up the site variables ---- 

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


# make unique id ----
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
#write.csv(rushdata, "data/rushdata_w.csv", row.names = TRUE)
#write.csv(spp_groups, "data/spp_groups.csv", row.names = FALSE)


#rush_01.4_prep_species.R----

#rushdata <- read.csv("data/rushdata_w.csv", header = TRUE, row.names = 1)

rushdata <- rushdata %>% 
  mutate_at(funs(as.factor), .vars = 1:5)

# Abbreviate species names 
#make species list lookup
head(names(rushdata), 20) #check where species start
#spp start at col 17
spp_list <- data.frame(names(rushdata)[6:ncol(rushdata)])
names(spp_list) <- "spp_name"
spp_list$spp_abbr <- vegan::make.cepnames(spp_list$spp_name)
spp_list$spp_name[which(duplicated(spp_list$spp_abbr))]

spp_list <- separate(spp_list, spp_name, into = c("genus", "species"), 
                     remove = FALSE)


#eyeball these to find duplicate species and other mistakes

### sort out species -----

#Rhytidiadelphus squarrosus
VIM::matrixplot(select(rushdata, contains("Rhytidiadelphus")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Rhytidiadelphus")))))

#Rhytidiadelphus.squarrosus and Rhytidiadelphus.squarrosus.1 almost completely overlap - need to check if values are the same
#Rhytidiadelphus.squaros is different 

rhytid <- select(rushdata, contains("Rhytidiadelphus"))
summary(rhytid)
#by eyeballing, seems that there are no conflicts so can merge them.  

rushdata$Rhytidiadelphus.squarrosus[is.na(rushdata$Rhytidiadelphus.squarrosus)] <- 
  rushdata$Rhytidiadelphus.squarrosus.1[is.na(rushdata$Rhytidiadelphus.squarrosus)]  

rushdata$Rhytidiadelphus.squarrosus[is.na(rushdata$Rhytidiadelphus.squarrosus)] <- 
  rushdata$Rhytidiadelphus.squaros[is.na(rushdata$Rhytidiadelphus.squarrosus)]  

rushdata$Rhytidiadelphus.squarrosus.1 <- NULL
rushdata$Rhytidiadelphus.squaros <- NULL


#Alopecurus pratensis
VIM::matrixplot(select(rushdata, contains("Alopecurus")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Alopecurus")))))

#no overlap, although there is also not much overlap with A. geniculatus - so maybe an issue here.  

rushdata$Alopecurus.pratensis[is.na(rushdata$Alopecurus.pratensis)] <- 
  rushdata$Alopecurus..pratensis[is.na(rushdata$Alopecurus.pratensis)]  

rushdata$Alopecurus..pratensis <- NULL

#Anthoxanthum.odoratum
VIM::matrixplot(select(rushdata, contains("Anthoxanthum")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Anthoxanthum")))))
#diff 2016 v 2015/2017

rushdata$Anthoxanthum.odoratum[is.na(rushdata$Anthoxanthum.odoratum)] <- 
  rushdata$Anthoxanthum.odorat[is.na(rushdata$Anthoxanthum.odoratum)]  
rushdata$Anthoxanthum.odorat <- NULL

#Calliergonella.cuspidata
VIM::matrixplot(select(rushdata, contains("Calliergon")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Calliergon")))))

rushdata$Calliergonella.cuspidata[is.na(rushdata$Calliergonella.cuspidata)] <- 
  rushdata$Calliergon.cuspidatum[is.na(rushdata$Calliergonella.cuspidata)]  
rushdata$Calliergon.cuspidatum <- NULL

rushdata$Calliergonella.cuspidata[is.na(rushdata$Calliergonella.cuspidata)] <- 
  rushdata$Calliergonella.cuspidata.1[is.na(rushdata$Calliergonella.cuspidata)]  
rushdata$Calliergonella.cuspidata.1 <- NULL

rushdata$Calliergonella.cuspidata[is.na(rushdata$Calliergonella.cuspidata)] <- 
  rushdata$Calliergonella.cuspidata.2[is.na(rushdata$Calliergonella.cuspidata)]  
rushdata$Calliergonella.cuspidata.2<- NULL

rushdata$Calliergonella.cuspidata[is.na(rushdata$Calliergonella.cuspidata)] <- 
  rushdata$Calliergonella.cuspidata.3[is.na(rushdata$Calliergonella.cuspidata)]  
rushdata$Calliergonella.cuspidata.3 <- NULL


#Carex
VIM::matrixplot(select(rushdata, contains("Carex")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Carex")))))

#Carex.echinata
rushdata$Carex.echinata[is.na(rushdata$Carex.echinata)] <- 
  rushdata$Carex.sp..echinata..[is.na(rushdata$Carex.echinata)]  
rushdata$Carex.sp..echinata.. <- NULL

#Carex.hostiana
rushdata$Carex.hostiana[is.na(rushdata$Carex.hostiana)] <- 
  rushdata$Carex.hostiana.1[is.na(rushdata$Carex.hostiana)]  
rushdata$Carex.hostiana.1 <- NULL

#Carex.pilulifera
rushdata$Carex.pilulifera[is.na(rushdata$Carex.pilulifera)] <- 
  rushdata$Carex.pilulifera.1[is.na(rushdata$Carex.pilulifera)]  
rushdata$Carex.pilulifera.1 <- NULL

#Carex.viridula
rushdata$Carex.viridula[is.na(rushdata$Carex.viridula)] <- 
  rushdata$Carex.viridula.1[is.na(rushdata$Carex.viridula)]  
rushdata$Carex.viridula.1 <- NULL

#Cerastium.glomeratum
VIM::matrixplot(select(rushdata, contains("Cerastium")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Cerastium")))))

rushdata$Cerastium.glomeratum[is.na(rushdata$Cerastium.glomeratum)] <- 
  rushdata$Cerastium..glomeratum[is.na(rushdata$Cerastium.glomeratum)]  
rushdata$Cerastium..glomeratum <- NULL

#Hylocomium.splendens
VIM::matrixplot(select(rushdata, contains("Hyloc")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Hyloc")))))

rushdata$Hylocomium.splendens[is.na(rushdata$Hylocomium.splendens)] <- 
  rushdata$Hylocumium.splendens[is.na(rushdata$Hylocomium.splendens)]  
rushdata$Hylocumium.splendens <- NULL


#Lathyrus.pratensis
VIM::matrixplot(select(rushdata, contains("Lathyrus")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Lathyrus")))))
rushdata$Lathyrus.pratensis.1 <- NULL

#Luzula sp
VIM::matrixplot(select(rushdata, contains("Luzula")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Luzula")))))

rushdata$Luzula.sp[is.na(rushdata$Luzula.sp)] <- 
  rushdata$Luzula.sp.[is.na(rushdata$Luzula.sp)]  
rushdata$Luzula.sp. <- NULL


#Lychnis.flos.cuculi
VIM::matrixplot(select(rushdata, contains("Lychnis")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Lychnis")))))

rushdata$Lychnis.flos.cuculi[is.na(rushdata$Lychnis.flos.cuculi)] <- 
  rushdata$Lychnis.flos.cuculi.[is.na(rushdata$Lychnis.flos.cuculi)]  
rushdata$Lychnis.flos.cuculi. <- NULL

#Lysimachia.nemorum
VIM::matrixplot(select(rushdata, contains("ysimach")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("ysimach")))))

rushdata$Lysimachia.nemorum[is.na(rushdata$Lysimachia.nemorum)] <- 
  rushdata$lysimachia.nemorum[is.na(rushdata$Lysimachia.nemorum)]  
rushdata$lysimachia.nemorum <- NULL

rushdata$Lysimachia.nemorum[is.na(rushdata$Lysimachia.nemorum)] <- 
  rushdata$Lysimachia.nemorum.1[is.na(rushdata$Lysimachia.nemorum)]  
rushdata$Lysimachia.nemorum.1 <- NULL

#Polytrichum.formosum
VIM::matrixplot(select(rushdata, contains("formos")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("formos")))))

rushdata$Polytrichum.formosum[is.na(rushdata$Polytrichum.formosum)] <- 
  rushdata$Polytrichum.formosa[is.na(rushdata$Polytrichum.formosum)]  
rushdata$Polytrichum.formosa <- NULL

#Potentilla.erecta
VIM::matrixplot(select(rushdata, contains("Potentilla")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Potentilla")))))

rushdata$Potentilla.erecta[is.na(rushdata$Potentilla.erecta)] <- 
  rushdata$Potentilla.erecta.erecta[is.na(rushdata$Potentilla.erecta)]  
rushdata$Potentilla.erecta.erecta <- NULL


#Ranunculus.repens
VIM::matrixplot(select(rushdata, contains("unculus")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("unculus")))))

rushdata$Ranunculus.repens[is.na(rushdata$Ranunculus.repens)] <- 
  rushdata$Ranunculus..repens[is.na(rushdata$Ranunculus.repens)]  
rushdata$Ranunculus..repens <- NULL

rushdata$Ranunculus.repens[is.na(rushdata$Ranunculus.repens)] <- 
  rushdata$Ranuunculus..repens[is.na(rushdata$Ranunculus.repens)]  
rushdata$Ranuunculus..repens <- NULL

#Sphagnum.squarrosum
VIM::matrixplot(select(rushdata, contains("squarrosum")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("squarrosum")))))

rushdata$Sphagnum.squarrosum[is.na(rushdata$Sphagnum.squarrosum)] <- 
  rushdata$Sphagnum_squarrosum[is.na(rushdata$Sphagnum.squarrosum)]  
rushdata$Sphagnum_squarrosum <- NULL

#Valeriana.dioica
VIM::matrixplot(select(rushdata, contains("Valerian")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("Valerian")))))

rushdata$Valeriana.dioica[is.na(rushdata$Valeriana.dioica)] <- 
  rushdata$Valerian.dioica[is.na(rushdata$Valeriana.dioica)]  
rushdata$Valerian.dioica <- NULL

#Veronica.serpyllifolia
VIM::matrixplot(select(rushdata, contains("serpyl")), 
                labels = vegan::make.cepnames(names(select(rushdata, 
                                                           contains("serpyl")))))

rushdata$Veronica.serpyllifolia[is.na(rushdata$Veronica.serpyllifolia)] <- 
  rushdata$Veronica.serpylifolia[is.na(rushdata$Veronica.serpyllifolia)]  
rushdata$Veronica.serpylifolia <- NULL

rushdata$Veronica.serpyllifolia[is.na(rushdata$Veronica.serpyllifolia)] <- 
  rushdata$Veronica.serpyllifolia.1[is.na(rushdata$Veronica.serpyllifolia)]  
rushdata$Veronica.serpyllifolia.1 <- NULL

# Abbreviate species names 

#make species list lookup
head(names(rushdata), 20) #check where species start
#spp start at col 6
spp_list <- data.frame(names(rushdata)[6:ncol(rushdata)])
names(spp_list) <- "spp_name"
spp_list$spp_abbr <- vegan::make.cepnames(spp_list$spp_name)
spp_list$spp_name[which(duplicated(spp_list$spp_abbr))]

spp_list <- separate(spp_list, spp_name, into = c("genus", "species"), 
                     remove = FALSE)


#rename species columns
names(rushdata)[6:ncol(rushdata)] <- spp_list$spp_abbr


#sort out classes
names(rushdata)
rushdata <- mutate_at(rushdata, funs(as.factor),
                      .vars = 1:5)
rushdata <- mutate_at(rushdata, funs(as.numeric), .vars = 6:ncol(rushdata))

#add zeroes
rushdata[6:ncol(rushdata)][is.na(rushdata[6:ncol(rushdata)])] <- 0

#have a look
VIM::matrixplot(rushdata, labels = names(rushdata))


# make unique id again
uid <- paste(rushdata$location, rushdata$treat_plot, rushdata$replicate, 
             rushdata$quad, rushdata$year, sep = ".")
dupes <- uid[which(duplicated(uid))]
uid[which(uid %in% dupes)]
#all clear


#add uid to rushdata rownames

rownames(rushdata) <- uid

# export -----
 write.csv(rushdata, "data/rushdata_w.csv", row.names = TRUE)
 write.csv(spp_list, "data/spp_list.csv", row.names = FALSE)
# 

# rush_01.5_prep_taxa.R ----

#rushdata <- read.csv("data/rushdata_w.csv", header = TRUE, row.names = 1)

rushdata <- rushdata %>% 
  rownames_to_column(var = "uid") %>% 
  mutate_at(funs(as.factor), .vars = 1:5)

#spp_groups <- read.csv("data/spp_groups.csv", header = TRUE)
#spp_list <- read.csv("data/spp_list.csv", header = TRUE)

#make taxon lookup ----

#species group lookup - so far only for 2015 species. Need to add remainder

spp_list$spp_abbr[which(!spp_list$spp_abbr %in% spp_groups$species_abbr)]
spp_groups$species_abbr[which(!spp_groups$species_abbr %in% spp_list$spp_abbr)]

spp_groups$species_group <- as.factor(spp_groups$species_group)
levels(spp_groups$species_group)
spp_list$spp_name[which(!spp_list$spp_abbr %in% spp_groups$species_abbr)]
# 
# [1] Alchemilla..xanthochlora       Anthriscus.sylvestris         
# [3] Arrhenatherum.elatius          Bromus.hordaceus.hor          
# [5] Conopodium.majus               Cruciata.laevipes             
# [7] Filipendula.ulmaria            Geranium.sylvaticum           
# [9] Helictotrichon.pubescen        Heracleum.sphondylium         
# [11] Lotus.corniculatus             Ranunculus.bulbosus           
# [13] Sanguisorba.officinalis        Senecio.jacobea               
# [15] Stellaria.media                Urtica.dioica                 
# [17] Veronica.chamaedrys            Dactylorhiza.purpurella       
# [19] Juncus.compactus               Poa.annua                     
# [21] Senecio.vulgaris               Festuca.x.Lolium              
# [23] Lychnis.hosieomi               Ranunculus.lingua             
# [25] Stellaria.liginosum            Epilobium.tetragonum          
# [27] Avenula.pratense               Geum.rivale                   
# [29] Cerastium.glomeratum           Crepis.paludosa               
# [31] Trichophorum.germanicum        Pedicularis.sp..not.flowering.
# [33] Poa.sp                         Myosotis.secunda              
# [35] Glyceria.declinata             Bryophyte                     
# [37] Viola.riviniana                Luzula.sp           


grps <- c("herb", "herb", "grass", "grass", "herb", "herb", 
          "herb", "herb", "grass", "herb", "herb", "herb", 
          "herb", "herb", "herb", "herb", "herb", "herb", "herb", 
          "grass", "herb", "grass", "herb", "herb", "herb", "herb",
          "grass", "herb", "herb", "herb", "sedge", #is deergrass a sedge?
          "herb", "grass", "herb", "grass", "moss", "herb", "woodrush") 

new_spp_groups <- 
  data.frame(
    species_name =
      paste(spp_list$spp_name[which(!spp_list$spp_abbr %in% spp_groups$species_abbr)]),
    species_group = 
      grps,
    species_abbr =
      paste(spp_list$spp_abbr[which(!spp_list$spp_abbr %in% spp_groups$species_abbr)]))

spp_groups <- rbind(new_spp_groups, spp_groups)



#Calculate taxon cover ----

rushdata_g <- gather(rushdata, key = "species", value = "cover", Juncacut:Luzusp) 

rushdata_g <- left_join(rushdata_g, spp_groups, by = c("species" = "species_abbr"))

#by group, gathered
rushgroups_g <- rushdata_g %>%
  select(uid, year, replicate, location, treat_plot, quad, cover, species_group) %>%  
  group_by(uid, species_group) %>% 
  summarise(cover = sum(cover)) 

#by group, spread
rushgroups <- rushdata_g %>%
  select(uid, year, replicate, location, treat_plot, quad, cover, species_group) %>%  
  group_by(uid, year, replicate, location, treat_plot, quad, species_group) %>% 
  summarise(cover = sum(cover)) %>% 
  spread(species_group, cover) %>% 
  mutate(cover_tot = sum(grass, herb, moss, sedge, woodrush, horsetail, rush))

#by uid, total cover
rush_tot_cover <- rushdata_g %>%
  select(uid, cover) %>%  
  group_by(uid) %>% 
  summarise(cover = sum(cover))

#inspect
hist(rush_tot_cover$cover)
hist(rushgroups_g$cover)

rushgroups_g[which(rushgroups_g$cover > 100),]

#import estimated taxon cover ----


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

### remove blank lines and comments 
meadows_2016 <- meadows_2016[c(1:4, 154:159), ]
pastures_2016 <- pastures_2016[c(1:4, 156:162), ]

HP_2017 <- HP_2017[c(1:5, 9:15),]
HM_2017 <- HM_2017[c(1:5, 9:14),] #no bryophytes
LM_2017 <- LM_2017[c(1:5, 9:14),] #no bryophytes
VP_2017 <- VP_2017[c(1:5, 9:15),]

#Harmonise variable names

meadows_2016$X__1 <- 
  recode_factor(meadows_2016$X__1, 
                "Loaction" = "location",
                "Replicate number" = "replicate", 
                "Treatment Plot" = "treat_plot", 
                "Actual Quadrat Number" = "quad",
                "Bare Ground" = "br_grd",
                "Plant Litter" = "litter",
                "Total rush cover" = "rush",
                "Total grass cover" = "grass",
                "Total herb cover" = "herb", 
                "Total sedge cover" = "sedge")

pastures_2016$X__1 <- 
  recode_factor(pastures_2016$X__1, 
                "Loaction" = "location",
                "Replicate number" = "replicate", 
                "Treatment Plot" = "treat_plot", 
                "Actual Quadrat Number" = "quad",
                "Bare Ground" = "br_grd",
                "Plant Litter" = "litter",
                "Total rush cover" = "rush",
                "Total grass cover" = "grass",
                "Total herb cover" = "herb", 
                "Total sedge cover" = "sedge",
                "Total mosses" = "moss")

HP_2017$X__1 <- 
  recode_factor(HP_2017$X__1,
                "Site (H/L/V)" = "site",
                "Type (M/P)" = "type", 
                "Quadrat (1-100)" = "quad", 
                "Treatment Plot (1-8)" = "treat_plot", 
                "Replicate (A/B/C)" = "replicate",
                "Rush cover" = "rush",
                "Grass cover" = "grass",
                "Herb cover" = "herb",
                "Sedge cover" = "sedge",
                "Bare ground" = "br_grd",
                "Litter" = "litter",
                "Bryophyte cover" = "moss")

HM_2017$X__1 <- 
  recode_factor(HM_2017$X__1,
                "Site (H/L/V)" = "site",
                "Type (M/P)" = "type", 
                "Quadrat (1-100)" = "quad", 
                "Treatment Plot (1-8)" = "treat_plot", 
                "Replicate (A/B/C)" = "replicate",
                "Rush cover" = "rush",
                "Grass cover" = "grass",
                "Herb cover" = "herb",
                "Sedge cover" = "sedge",
                "Bare ground" = "br_grd",
                "Litter" = "litter")

LM_2017$X__1 <- 
  recode_factor(LM_2017$X__1,
                "Site (H/L/V)" = "site",
                "Type (M/P)" = "type", 
                "Quadrat (1-100)" = "quad", 
                "Treatment Plot (1-8)" = "treat_plot", 
                "Replicate (A/B/C)" = "replicate",
                "Rush cover" = "rush",
                "Grass cover" = "grass",
                "Herb cover" = "herb",
                "Sedge cover" = "sedge",
                "Bare ground" = "br_grd",
                "Litter" = "litter")

VP_2017$X__1 <- 
  recode_factor(VP_2017$X__1,
                "Site (H/L/V)" = "site",
                "Type (M/P)" = "type", 
                "Quadrat (1-100)" = "quad", 
                "Treatment Plot (1-8)" = "treat_plot", 
                "Replicate (A/B/C)" = "replicate",
                "Rush cover" = "rush",
                "Grass cover" = "grass",
                "Herb cover" = "herb",
                "Sedge cover" = "sedge",
                "Bare ground" = "br_grd",
                "Litter" = "litter",
                "Bryophyte" = "moss")


# munge sheets
grpdata_2016 <- full_join(meadows_2016, pastures_2016, by = "X__1")

grpdata_2017 <- full_join(HP_2017, HM_2017, by = "X__1")
grpdata_2017 <- full_join(grpdata_2017, LM_2017, by = "X__1")
grpdata_2017 <- full_join(grpdata_2017, VP_2017, by = "X__1")

# harmonise site variables
grpdata_2017[1,] <- toupper(paste0(grpdata_2017[1,], grpdata_2017[2,]))

grpdata_2017$X__1 <- as.character(grpdata_2017$X__1)
grpdata_2017$X__1[1] <- "location"

# add year
grpdata_2016 <- rbind(c("year", rep("2016", ncol(grpdata_2016)-1)), 
                      grpdata_2016)
grpdata_2016$X__1 <- as.character(grpdata_2016$X__1)
grpdata_2016$X__1[1] <- "year"

grpdata_2017 <- rbind(c("year", rep("2017", ncol(grpdata_2017)-1)), 
                      grpdata_2017)


# munge years
grpdata <- full_join(grpdata_2016, grpdata_2017, by = "X__1")

#check for duplicated names
grpdata[which(grpdata$X__1 %in% 
                grpdata$X__1[duplicated(grpdata$X__1)]), 1] 



#save colnames
colnames_rush <- grpdata$X__1

#transform
grpdata <- data.frame(t(grpdata))

#write col names
names(grpdata) <- colnames_rush
colnames(grpdata)

#delete row 1
grpdata <- grpdata[-1,]

#add temporary rownames
rownames(grpdata) <- seq(1:nrow(grpdata))


#find duplicate col names
names(grpdata)[which(names(grpdata) %in% 
                       names(grpdata)[duplicated(names(grpdata))])] 
#all clear


#delete some duds
grpdata$type <- NULL

table(grpdata$year) # all good
grpdata$year <- droplevels(grpdata$year)

table(grpdata$location)
grpdata$location <- recode(grpdata$location,   #recode 2015 and 2016 data
                           `H'SHIP  MED` = "HM", 
                           `H'SHIP   PAST` = "HP", 
                           `LINGY MED` = "LM", 
                           `VAL PAST` = "VP")
grpdata$location <- droplevels(grpdata$location)
table(grpdata$location)

table(grpdata$replicate)
grpdata$replicate <- toupper(grpdata$replicate)
grpdata$replicate <- recode(grpdata$replicate,   #recode 2015 and 2016 data
                            `A (1)` = "A", 
                            `A(1)` = "A", 
                            `B (2)` = "B", 
                            `B(2)` = "B", 
                            `C(3)` = "C")
table(grpdata$replicate)
grpdata$replicate <- as.factor(grpdata$replicate)

table(grpdata$treat_plot)
grpdata$treat_plot <- droplevels(grpdata$treat_plot)
table(grpdata$treat_plot)

unique(grpdata$quad)

#have a look where there are missing values
VIM::matrixplot(grpdata[1:10], 
                labels = substr(names(grpdata[1:10]), 1, 5))


#sort out classes
names(grpdata)
grpdata <- mutate_at(grpdata, funs(as.factor),
                     .vars = 1:5)
grpdata <- mutate_at(grpdata, funs(as.numeric), .vars = 6:ncol(grpdata))


# make unique id
uid <- paste(grpdata$location, grpdata$treat_plot, grpdata$replicate, 
             grpdata$quad, grpdata$year, sep = ".")

dupes <- uid[which(duplicated(uid))]

uid[which(uid %in% dupes)]
#no replicates

#add uid to grpdata rownames

rownames(grpdata) <- uid

#check the names match
rushdata$uid[which(
  as.character(rushdata$uid[which(!grepl(pattern = "2015", 
                                         rushdata$uid))]) != 
    rownames(grpdata)
)]
#all clear

#tidy up
rm(grpdata_2016, grpdata_2017, HM_2017, HP_2017, VP_2017, LM_2017, 
   meadows_2016, pastures_2016, rushdata, colnames_rush, dupes, grps)

# export -----
write.csv(spp_groups, "data/spp_groups.csv", row.names = FALSE)
write.csv(grpdata, "data/taxon_group_est_w.csv", row.names = FALSE)
write.csv(rushgroups, "data/taxon_group_calc_w.csv", row.names = FALSE)
write.csv(rushgroups_g, "data/taxon_group_calc_g.csv", row.names = FALSE)
write.csv(rush_tot_cover, "data/taxon_cov_total.csv", row.names = FALSE)
