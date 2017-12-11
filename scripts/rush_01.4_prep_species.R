library(tidyverse)
library(vegan)

rushdata <- read.csv("data/rushdata_w.csv", header = TRUE, row.names = 1)

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


