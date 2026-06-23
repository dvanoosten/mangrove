library(tidyverse)
if(!require(pedtools, quietly=TRUE)) install.packages("pedtools",repos="http://cran.r-project.org")
library(pedtools)
if(!require(ribd, quietly=TRUE)) install.packages("ribd",repos="http://cran.r-project.org")
library(ribd)

source("scripts/ped_functions.R")

# read in data, make pedigree object
batchID <- "royals" # CHECK

ped_data <- read.csv(paste0(batchID,"/ped_",batchID,".csv")) %>% # CHECK
  mutate(Sex = case_match(Sex, "M"~1, "F"~2))
ped_data <- add_dummy_parents(ped_data)
ped_obj <- ped(id = ped_data$ID, fid = ped_data$Father, mid = ped_data$Mother,
               sex = ped_data$Sex, isConnected = FALSE)

proband_IDs <- read.csv(paste0(batchID,"/proband_IDs_",batchID,".csv"), colClasses="character") %>% # CHECK
  replace(is.na(.), "X")
ped_obj <- relabel(ped_obj, new=proband_IDs$ogID, old=proband_IDs$MgvID)

# calculate kinship matrix, get pairwise relationships between probands
kinship_mat <- kinship(ped_obj, ids=proband_IDs$ogID)
distance_mat <- kin2deg(kinship_mat, unrelated=100)

# 
ij <- t(combn(colnames(distance_mat), 2))
pairwise <- data.frame(ij, dist=distance_mat[ij])
colnames(pairwise) <- c("ID1","ID2","DEGREE")
pairwise <- filter(pairwise, DEGREE < 100) %>% 
  mutate(KNOWN = check_known_pedigrees(ID1,ID2,proband_IDs))

write.csv(pairwise, paste0(batchID,"/pairs_",batchID,".csv"), row.names = FALSE)
