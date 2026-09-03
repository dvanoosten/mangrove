library(tidyverse)
library(pedtools)
library(ribd)

add_dummy_parents <- function(ped_data) {
  # check for individuals with only one parent, add dummy parent
  mothers <- filter(ped_data, Father=="" & Mother!="")$Mother
  for (mid in unique(mothers)) {
    dummy_id <- paste0("Dummy_", mid)
    ped_data <- mutate(ped_data, Fathers = 
                         case_when(Mother==mid & Father=="" ~ dummy_id, .default = Father))
    if (!(dummy_id %in% ped_data$ID)) {
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
    if (!(dummy_id %in% ped_data$ID)) {
      ped_data <- rbind(ped_data, data.frame(
        ID = dummy_id, Father = "", Mother = "",Sex = 2,
        SupPEDID = filter(ped_data, Father==fid)$SupPEDID, Proband = NA))
    }
  }
  return(ped_data)
}

check_subset <- function(probands, kinship_mat, superpedid="superped") {
  # check if all probands are related, make subsets if needed
  if (length(probands) < 3) return(list(probands) |> setNames(superpedid))
  if (! any(kinship_mat[probands, probands] == 0)) return(list(probands) |> setNames(superpedid))
  
  i <- 1
  subsets <- list()
  for (k in (length(probands)-1):2) {
    subsets_k <- combn(probands, k, simplify = FALSE)
    for (subset in subsets_k) {
      if(any(kinship_mat[subset,subset] == 0) |
         any(mapply(function(x,y) all(y %in% x), subsets, list(subset)))) next
      subsets[[paste(superpedid, i, sep="_")]] <- subset
      i <- i + 1
    }
  }
  return(subsets)
}

trim_ped <- function(ped_obj, ped_data_fam, proband_labs) {
  # trim pedigree to only include ancestors related to multiple probands
  kinship_mat <- kinship(ped_obj, simplify=FALSE)
  proband_subsets <- check_subset(proband_labs, kinship_mat)
  shared <- c()
  for (subset in proband_subsets) {
    n <- length(subset)
    ID_list <- c(subset, unlist(sapply(subset, function(x) names(which(kinship_mat[,x] > 0, arr.ind = TRUE)))))
    ID_list <- Filter(function(x) sum(ID_list == x) >= n, ID_list)
    shared <- append(shared, c(shared, ID_list))
  }
  
  ped_obj_trim <- subset(ped_obj, unique(shared), missingParents="include")
  
  if (names(ped_obj_trim)[1] != "ID") {
    shared <- c()
    for (subset in proband_subsets) {
      ID_list <- c(subset, unlist(sapply(subset, function(x) names(which(kinship_mat[,x] > 0, arr.ind = TRUE)))))
      ID_list <- Filter(function(x) sum(ID_list == x) >= 2, ID_list)
      shared <- append(shared, c(shared, ID_list))
    }
    
    ped_obj_trim <- subset(ped_obj, unique(shared), missingParents="include")
  }
  
  return(ped_obj_trim)
}

check_known_pedigrees <- function(ID1, ID2, proband_IDs) {
  # check if probands are from same family
  mapply(function(ID1, ID2) {
    ped1 <- filter(proband_IDs, ogID==ID1)$PEDID
    ped2 <- filter(proband_IDs, ogID==ID2)$PEDID
    return(ped1 == ped2 & ped1 != "X")}, 
    ID1, ID2)}
