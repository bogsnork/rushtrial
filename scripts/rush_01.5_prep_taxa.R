library(tidyverse)
library(readxl)
library(vegan)

rushdata <- read.csv("data/rushdata_w.csv", header = TRUE, row.names = 1)

rushdata <- rushdata %>% 
  rownames_to_column(var = "uid") %>% 
  mutate_at(funs(as.factor), .vars = 1:5)

spp_groups <- read.csv("data/spp_groups.csv", header = TRUE)
spp_list <- read.csv("data/spp_list.csv", header = TRUE)

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

write.csv(spp_groups, "data/spp_groups.csv", row.names = FALSE)

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
  group_by(uid, species_group) %>% 
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
write.csv(grpdata, "data/taxon_group_est_w.csv", row.names = TRUE)
write.csv(rushgroups, "data/taxon_group_calc_w.csv", row.names = FALSE)
write.csv(rushgroups_g, "data/taxon_group_calc_g.csv", row.names = FALSE)
write.csv(rush_tot_cover, "data/taxon_cov_total.csv", row.names = FALSE)