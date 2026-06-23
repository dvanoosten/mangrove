library(tidyverse)
library(pedtools)

source("../scripts/ped_functions.R")

get_ped <- function(MgvIDs, trim_ped, ped_data, proband_IDs, masterlist) {
  # get pedigree object and filtered masterlist for superpedigree or ancestor table
  ped_data_fam <- filter(ped_data, ID %in% MgvIDs)
  ped_data_fam <- add_dummy_parents(ped_data_fam)
  
  masterlist_dummy <- filter(ped_data_fam, startsWith(ID, "Dummy")) %>% 
    select(ID, Sex) %>% rename(MgvID=ID)  %>% 
    mutate(Sex = case_match(Sex, 1~"M", 2~"F"))
  masterlist_fam <- filter(masterlist, MgvID %in% MgvIDs)
  masterlist_fam <- bind_rows(masterlist_fam, masterlist_dummy) %>%
    select(-c(Place_of_birth_code, Initials))
  
  proband_IDs_fam <- filter(proband_IDs, MgvID %in% MgvIDs)
  proband_labs <- setNames(proband_IDs_fam$ogID, proband_IDs_fam$MgvID)
  
  ped_obj <- ped(id = ped_data_fam$ID, fid = ped_data_fam$Father, mid = ped_data_fam$Mother,
                 sex = ped_data_fam$Sex, isConnected = TRUE)
  
  ped_labs <- relabel(ped_obj, "generations", returnLabs=TRUE)
  masterlist_fam <- mutate(masterlist_fam, Label=ifelse(MgvID %in% names(proband_labs),
                                                        proband_labs[MgvID], ped_labs[MgvID])) %>%
    relocate(Label) %>% relocate(MgvID, .after=last_col())
  
  all_labs <- setNames(masterlist_fam$Label, masterlist_fam$MgvID)
  inv_labs <- setNames(masterlist_fam$MgvID, masterlist_fam$Label)
  
  ped_obj <- relabel(ped_obj, new=masterlist_fam$Label, old=masterlist_fam$MgvID)
  
  if (trim_ped) {
    ped_obj <- trim_ped(ped_obj, ped_data_fam, proband_labs, inv_labs, all_labs, "R")
    masterlist_fam <- filter(masterlist_fam, Label %in% ped_obj$ID)
  }
  
  return(list("ind_table"=masterlist_fam, "ped_obj"=ped_obj))
  
}

plot_ped <- function(ped_obj, title) {
  # plot pedigree of superfamily
  plot(ped_obj, aff=Filter(function(x) startsWith(x,"R"), ped_obj$ID),
       title = title)
  
}
