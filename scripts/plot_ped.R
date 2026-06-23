library(tidyverse)
library(pedtools)

source("scripts/ped_functions.R")

# read in data
batchID <- "royals" # CHECK

ped_data <- read.csv(paste0(batchID,"/ped_",batchID,".csv")) %>% # CHECK
  filter(SupPEDID != "") %>% mutate(Sex = case_match(Sex, "M"~1, "F"~2))
ped_data <- add_dummy_parents(ped_data)

proband_IDs <- read.csv(paste0(batchID,"/proband_IDs_",batchID,".csv"), colClasses="character") %>% # CHECK
  mutate(label = paste(ogID, PEDID, sep="\n"))

# make pedigree object and plot for each superpedigree
pdf(file = paste0(batchID,"/ped_plots_",batchID,".pdf"), w = 16.17, h = 8)

for (superpedid in sort(unique(ped_data$SupPEDID))) {
  ped_data_fam <- filter(ped_data, SupPEDID == superpedid)
  proband_IDs_fam <- filter(proband_IDs, MgvID %in% ped_data_fam$ID)
  proband_labs <- setNames(proband_IDs_fam$label, proband_IDs_fam$MgvID)
  
  all_labs <- setNames(ped_data_fam$ID, ped_data_fam$ID)
  all_labs <- c(proband_labs, Filter(function(x) ! x %in% names(proband_labs), all_labs))
  inv_labs <- setNames(names(all_labs), all_labs)
  
  ped_obj <- ped(id = ped_data_fam$ID, fid = ped_data_fam$Father, mid = ped_data_fam$Mother,
                 sex = ped_data_fam$Sex, isConnected = TRUE)
  ped_obj <- relabel(ped_obj, new=proband_labs, old=names(proband_labs))
  
  ped_obj_trim <- trim_ped(ped_obj, ped_data_fam, proband_labs, inv_labs, all_labs)
  plot(ped_obj_trim, aff=proband_labs, labs=unname(proband_labs),
       title = paste("Superpedigree", superpedid), margins=c(0,1,2,1))
}

dev.off()
