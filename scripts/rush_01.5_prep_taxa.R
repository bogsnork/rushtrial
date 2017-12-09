library(tidyverse)

rushdata <- read.csv("data/rushdata.csv", header = TRUE)

rushdata <- rushdata %>% 
  select(-X) %>% #remove first column
  mutate_at(funs(as.factor), .vars = 1:5)


##2015
excel_sheets("data/rush_trial_data_2015.xls")# read names of sheets

meadows_2015 <- read_excel("data/rush_trial_data_2015.xls", 
                           sheet = "Mead2015spp", col_names = FALSE)

pastures_2015 <- read_excel("data/rush_trial_data_2015.xls", 
                            sheet = "Past2015spp", col_names = FALSE)


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
