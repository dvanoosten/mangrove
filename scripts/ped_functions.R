library(tidyverse)
library(pedtools)

add_dummy_parents <- function(ped_data) {
  # check for individuals with only one parent, add dummy parent
  mothers <- filter(ped_data, Father=="" & Mother!="")$Mother
  for (mid in unique(mothers)) {
    dummy_id <- paste0("Dummy_", mid)
    ped_data <- mutate(ped_data, Fathers = 
                         case_when(Mother==mid & Father=="" ~ dummy_id, .default = Father))
    if (!(dummy_id %in% ped_data$Individual_ID)) {
      ped_data <- rbind(ped_data, data.frame(
        ID = dummy_id, Father = "", Mother = "", Sex = 1,
        SupPEDID = filter(ped_data, Mother==mid)$SupPEDID, Proband = NA))
    }
  }
  fathers <- filter(ped_data, Mother=="" & Father!="")$Father
  for (fid in unique(fathers)) {
    dummy_id <- paste0("Dummy_", fid)
    ped_data <- mutate(ped_data, Mother = 
                         case_when(Father==fid & Mother=="" ~ dummy_id, .default = Mother))
    if (!(dummy_id %in% ped_data$Individual_ID)) {
      ped_data <- rbind(ped_data, data.frame(
        ID = dummy_id, Father = "", Mother = "",Sex = 2,
        SupPEDID = filter(ped_data, Father==fid)$SupPEDID, Proband = NA))
    }
  }
  return(ped_data)
}

trim_ped <- function(ped_obj, ped_data_fam, proband_labs, inv_labs, all_labs, prefix="R") {
  # trim pedigree to only include ancestors related to multiple probands
  if (length(proband_labs) == 1) {
    return(ped_obj)
  }
  relateds_all <- c()
  for (proband in proband_labs) {
    relateds_all <- append(relateds_all,
                           Filter(function(x) ! x %in% unrelated(ped_obj, proband), ped_obj$ID)
    )
  }
  relateds_sel <- c()
  for (proband in proband_labs) {
    relateds_prob <- Filter(function(x) ! x %in% unrelated(ped_obj, proband), ped_obj$ID)
    n_rel_prob <- length(Filter(function(x) startsWith(x, prefix), relateds_prob))
    relateds_prob <- Filter(function(x) ! sum(relateds_all == x) < n_rel_prob, relateds_prob)
    relateds_sel <- append(relateds_sel, relateds_prob)
  }
  
  relateds <- inv_labs[unique(relateds_sel)]
  to_keep <- inv_labs[proband_labs]
  for (id in relateds) {
    to_keep <- append(to_keep, id)
    spouses <- c()
    if (id %in% ped_data_fam$Father) {
      spouses <- filter(ped_data_fam, Father == id)$Mother
    }
    else if (id %in% ped_data_fam$Mother) {
      spouses <- filter(ped_data_fam, Mother == id)$Father
    }
    if (length(spouses) != 0 & any(! spouses %in% relateds)) {
      to_keep <- append(to_keep, spouses)
    }
  }
  
  to_keep <- all_labs[unique(to_keep)]
  ped_obj_trim <- subset(ped_obj, to_keep)
  return(ped_obj_trim)
}

check_known_pedigrees <- function(ID1, ID2, proband_IDs) {
  # check if probands are from same family
  mapply(function(ID1, ID2) {
    ped1 <- filter(proband_IDs, ogID==ID1)$PEDID
    ped2 <- filter(proband_IDs, ogID==ID2)$PEDID
    return(ped1 == ped2 & ped1 != "X")}, 
    ID1, ID2)}
