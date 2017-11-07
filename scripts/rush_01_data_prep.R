#Import and prepare rush trial data

#Context
#>HP = Herdship Pasture
#>HM = Herdship Meadow
#>LM = Lingy Hill Meadow
#>VP = Valance Pasture

# 1         Site (H/L/V)
# 2           Type (M/P)
# 3    Replicate (A/B/C)
# 4 Treatment Plot (1-8)
# 5      Quadrat (1-100)
# 6                 <NA>
# 7                 Date
# 8             Surveyor
# 9           Rush cover
# 10          Grass cover
# 11           Herb cover
# 12          Sedge cover
# 13          Bare ground
# 14               Litter
# 15      Bryophyte cover
# 16 Achillea millefolium
# 17    Achillea ptarmica
# 18      Agrostis canina
# 19  Agrostis capillaris
# 20 Agrostis stolonifera
# 21ff species names and some dud rows




#packages
library(readxl)
library(tidyverse)
library(vegan)
library(lubridate)

#Import data


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


#get more context
head(HP_2017[1], 20)
tail(HP_2017[1], 20)

#remove blank lines and comments
HP_2017 <- HP_2017[-c(6, 122, 123, 130:141),]
HM_2017 <- HM_2017[-c(6, 15, 122, 123, 127:139),]
LM_2017 <- LM_2017[-c(6, 15, 122:140),]
VP_2017 <- VP_2017[-c(6, 122, 123, 129:140),]

#match rownames
rushdata_2017 <- full_join(HP_2017, HM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, LM_2017, by = "X__1")
rushdata_2017 <- full_join(rushdata_2017, VP_2017, by = "X__1")

#make species list lookup
head(rushdata_2017[1], 20) #check where species start
  #spp start at row 15
spp_list <- data.frame(rushdata_2017[15:nrow(rushdata_2017), 1])
names(spp_list) <- "spp_name"
spp_list$spp_abbr <- vegan::make.cepnames(spp_list$spp_name)

#transpose
rushdata_2017 <- t(rushdata_2017)
rushdata_2017 <- data.frame(rushdata_2017)

#write col names
names(rushdata_2017) <- rushdata_2017$X__1

#check
head(names(rushdata_2017), 20)

#abbreviate first 14 col names
names(rushdata_2017)[1:14] <- c("site", "type", "replicate", "treat_plot", "quad", "date", "surveyor", "rush_cov", "grass_cov", "herb_cov", "sedge_cov", "bare_grd", "litter", "bryo_cov")

#abbreviate species colnames
  #compare numbers to make sure all ok
length(names(rushdata_2017)[15:ncol(rushdata_2017)])
nrow(spp_list)
  #all ok

  #write abbreviations into colname
names(rushdata_2017)[15:ncol(rushdata_2017)] <- spp_list$spp_abbr


#remove first row with old colnames
rushdata_2017 <- rushdata_2017[-1,]

#remove row names
row.names(rushdata_2017) <- NULL


#convert date column 
#rushdata_2017$date <- lubridate::parse_date_time(x = rushdata_2017$date)
  #this ain't working

#add year column
rushdata_2017$year <- rep("2017", nrow(rushdata_2017))

#add site_type column
rushdata_2017$site_type <- paste(rushdata_2017$site, rushdata_2017$type, sep = "")

#rearrange columns
rushdata_2017 <- rushdata_2017[c(1:5, 135, 136, 6:134)]

#do some general housekeeping 

#convert incorrect lower case values
rushdata_2017$site <- toupper(rushdata_2017$site)
rushdata_2017$type <- toupper(rushdata_2017$type)
rushdata_2017$replicate <- toupper(rushdata_2017$replicate)

#drop unused levels
rushdata_2017 <- droplevels(rushdata_2017)
levels(rushdata_2017$site)

#sort out classes
rushdata_2017 <- mutate_at(rushdata_2017, funs(as.factor), 
                           .vars = c("site", "type", "replicate", "treat_plot", 
                                     "quad", "surveyor", "site_type"))
rushdata_2017 <- mutate_at(rushdata_2017, funs(as.numeric), .vars = 10:136)
#date still an issue


#write to file
write.csv(rushdata_2017, "data/rushdata_2017.csv", row.names = FALSE)




