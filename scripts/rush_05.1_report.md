# Teesdale Rush Trial Analysis
Christoph Kratz  
7 February 2018  



# Introduction

# Data preparation

A considerable amount of data preparation was required, owing in part to inconsistencies in species naming and some moderate differences in the way data was recorded in each survey year. 

### Import and merging
The data from each worksheet was read into R, perpared and merged into a single table comprising a row for each quadrat in each survey year, and columns containing plot attributes and a column per species.  In the process of doing this some some coding errors were identified and corrected (see Appendix A: Data preparation). 

### Species names
Due to different spellings and use of capitalisation in a number of species, duplicate entries were created.  These were merged. I assumed that the surveys should be interpreted as a census, meaning that the absence of an observation in a quadrat during a particular year should be interpreted as the species being absent.  Zeroes were therefore added to all records where no species was observed.  Species names were abbreviated to unique eight letter codes using the function `make.cepnames` in the `vegan` package.

### Grouping species into taxa
In the datasheet for 2015 species were already assigned to broad groups of taxa: **rushes, grasses, herbs, sedges, woodrushes** and **mosses** as well as **bare ground** and (plant) **litter**.  Additional species (from the 2016 and 2017 surveys) were manually coded to taxa (see Appendix A: Data preparation).  

### Calculating cover at taxon level
In 2016 and 2017 surveyors estimated total cover in each quadrat for all **rushes, grasses, herbs, sedges** and in some cases **mosses**. However this was not done in the baseline 2015 survey.  In order to be able to estimate change from the baseline year, total cover for each taxon has been calculated by summing the cover of all species in a taxon for each quadrat for **all** survey years.  The 2016 and 2017 total cover estimates for each quadrat were merged into a separate table.  

### Wide v Long data  (*Tidy* data)
To aid data analysis, all taxon level cover data was converted from *wide* (quadrats in unique rows, species in separate columns) to *long* format.  This treats each unique quadrat-year-taxon combination as a separate observation.  Two new variables (`taxon` and `cover`) are created and each quadrat-year is therefore no-longer represented by a single row, but by as many rows as there were taxa observed (zeroes were not added at the taxon level).  For useful background on the utility of tidy data in R see: http://vita.had.co.nz/papers/tidy-data.html.  

### Prepared data
The prepared data has been saved to the following output files, which are the inputs for subsequent analysis: 

1.  `rushdata_w.csv` comprises species cover estimates for each quadrat:  
  + one row for each quadrat in each survey year, 
  + five columns for plot variables (year, location, replicate, treatment, quadrat), and 
  + 156 columns for each of the species observed.      

---------------------------------------------------------------------------------
       &nbsp;         year   location   replicate   treat_plot   quad   Juncacut 
-------------------- ------ ---------- ----------- ------------ ------ ----------
 **HM.1.A.61.2015**   2015      HM          A           1         61       34    

 **HM.1.A.64.2015**   2015      HM          A           1         64       12    

 **HM.1.A.51.2015**   2015      HM          A           1         51       11    

 **HM.2.A.8.2015**    2015      HM          A           2         8        20    

 **HM.2.A.15.2015**   2015      HM          A           2         15       23    

 **HM.2.A.35.2015**   2015      HM          A           2         35       28    
---------------------------------------------------------------------------------
2. `spp_groups.csv` contains a unique row for each species identified in the baseline 2015 survey, its abbreviation, and the taxon group it was assigned to in the 2015 survey.  

3. `spp_list.csv` contains a table of all species found, alongside its abbreviation.  

4. `taxon_group_est_w.csv` contains the cover estimates made in the field for each quadrat in 2016 and 2017.  It is in wide format.  

5. `taxon_group_calc_w.csv` (wide) and `taxon_group_calc_g.csv` (tall / tidy) contains the calculated cover for each quarat in all survey years. 

6. `taxon_cov_total.csv` contains the calculated total cover of all taxa for each quadrat in each survey year.


# some other heading




# Appendix 1: Data preparation

The following corrections were made to species names: 

From |To
---|---
Calliergon cuspidatum |Calliergonella cuspidata
Ranuunculus  repens |Ranunculus repens
Rhytidiadelphus squarosus |Rhytidiadelphus squarrosus
Rhytidiadelphus squaros |Rhytidiadelphus squarrosus


and to variable names: 

From |To
---|---
Loaction |Location

There were errors in identifying quadrats in two places.  

1) HP.6.B in 2015 had two quadrats 48.  Should have been quads 48, 47, 86.  47 was miscoded as 48.  For this analysis they have been coded 48a and 48b.  

2) VP.4.C in 2015 do not have a quadrat number.  For this analysis they have been coded 00a, 00b, 00c


The following taxon groups were manually coded to species: 
```
                          species    taxon
1        Alchemilla..xanthochlora     herb
2           Anthriscus.sylvestris     herb
3           Arrhenatherum.elatius    grass
4            Bromus.hordaceus.hor    grass
5                Conopodium.majus     herb
6               Cruciata.laevipes     herb
7             Filipendula.ulmaria     herb
8             Geranium.sylvaticum     herb
9         Helictotrichon.pubescen    grass
10          Heracleum.sphondylium     herb
11             Lotus.corniculatus     herb
12            Ranunculus.bulbosus     herb
13        Sanguisorba.officinalis     herb
14                Senecio.jacobea     herb
15                Stellaria.media     herb
16                  Urtica.dioica     herb
17            Veronica.chamaedrys     herb
18        Dactylorhiza.purpurella     herb
19               Juncus.compactus     herb
20                      Poa.annua    grass
21               Senecio.vulgaris     herb
22               Festuca.x.Lolium    grass
23               Lychnis.hosieomi     herb
24              Ranunculus.lingua     herb
25            Stellaria.liginosum     herb
26           Epilobium.tetragonum     herb
27               Avenula.pratense    grass
28                    Geum.rivale     herb
29           Cerastium.glomeratum     herb
30                Crepis.paludosa     herb
31        Trichophorum.germanicum    sedge
32 Pedicularis.sp..not.flowering.     herb
33                         Poa.sp    grass
34               Myosotis.secunda     herb
35             Glyceria.declinata    grass
36                      Bryophyte     moss
37                Viola.riviniana     herb
38                      Luzula.sp woodrush
```

