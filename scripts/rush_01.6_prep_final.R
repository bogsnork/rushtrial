#final data prep

library(tidyverse)

spp_obs_w <- read.csv("data/rushdata_w.csv", header = TRUE)
cov_tot <- read.csv("data/taxon_cov_total.csv", header = TRUE)
grp_obs_w <- read.csv("data/taxon_group_est_w.csv", header = TRUE)
grp_calc_w <- read.csv("data/taxon_group_calc_w.csv", header = TRUE)
grp_calc_g <- read.csv("data/taxon_group_calc_g.csv", header = TRUE)

# add type column
spp_obs_w$type <- ifelse(spp_obs_w$location %in% c("HP", "VP"), "pasture", "meadow")
spp_obs_w <- select(spp_obs_w, uid:quad, type, everything()) 

grp_obs_w$type <- ifelse(grp_obs_w$location %in% c("HP", "VP"), "pasture", "meadow")
grp_obs_w <- select(grp_obs_w, uid:quad, type, everything()) 

grp_calc_w$type <- ifelse(grp_calc_w$location %in% c("HP", "VP"), "pasture", "meadow")
grp_calc_w <- select(grp_calc_w, uid:quad, type, everything()) 

# export
write.csv(spp_obs_w, "data/prepped/spp_obs_w.csv")
write.csv(cov_tot, "data/prepped/spp_cov_total.csv")
write.csv(grp_obs_w, "data/prepped/taxon_grp_est_w.csv")
write.csv(grp_calc_w, "data/prepped/taxon_grp_calc_w.csv")
write.csv(grp_calc_g, "data/prepped/taxon_grp_calc_g.csv")