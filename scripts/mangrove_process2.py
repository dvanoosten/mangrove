import sys
import pandas as pd

sys.path.insert(1, "scripts/")
import mangrove_process_functions as man_fun

batchID = "royals" # CHECK
false_negatives = False

# read in masterlist and detected matches
masterlist = pd.read_csv(batchID+"/masterlist_"+batchID+"_unmatched.csv", converters={"All_IDs": pd.eval}) # CHECK
detected_matches = pd.read_csv(batchID+"/matches_"+batchID+"_checked.csv", usecols=[0,1,6,7,13,14], converters={"AncIDs2": pd.eval}) # CHECK
detected_matches = detected_matches[detected_matches["Merge"] == 1]

if false_negatives:
    extra_matches = pd.read_csv(batchID+"/falsenegatives_"+batchID+".csv", usecols=[0,1,6,7,13,14], converters={"AncIDs2": pd.eval}) # CHECK
    detected_matches = pd.concat([detected_matches, extra_matches], ignore_index=True)

# merge duplicate records in masterlist
for pair in detected_matches.itertuples():
    if pair.Remove == 1:
        if len(masterlist[masterlist["MgvID"] == pair.MgvID1]) > 0:
            transfer_IDs = masterlist.loc[masterlist["MgvID"] == pair.MgvID1, "All_IDs"].values[0]
            masterlist.drop(masterlist[masterlist["MgvID"] == pair.MgvID1].index, inplace=True)
            
            if pair.MgvID2 in masterlist["MgvID"].tolist():
                masterlist.loc[masterlist["MgvID"] == pair.MgvID2, "All_IDs"].values[0] += transfer_IDs
            else:
                if len(masterlist[masterlist['All_IDs'].apply(lambda x: all([n in x for n in pair.AncIDs2]))]) > 1:
                    print("Warning: AncIDs of record 2 in match present in multiple records", pair.MgvID2)
                masterlist.loc[masterlist['All_IDs'].apply(lambda x: all([n in x for n in pair.AncIDs2])), "All_IDs"].values[0] += transfer_IDs 
    
    elif pair.Remove == 2:
        if len(masterlist[masterlist["MgvID"] == pair.MgvID2]) > 0:
            transfer_IDs = masterlist.loc[masterlist["MgvID"] == pair.MgvID2, "All_IDs"].values[0]
            masterlist.drop(masterlist[masterlist["MgvID"] == pair.MgvID2].index, inplace=True)
            
            if pair.MgvID1 in masterlist["MgvID"].tolist():
                masterlist.loc[masterlist["MgvID"] == pair.MgvID1, "All_IDs"].values[0] += transfer_IDs
            else:
                if len(masterlist[masterlist['All_IDs'].apply(lambda x: pair.AncID1 in x)]) > 1:
                    print("Warning: AncIDs of record 1 in match present in multiple records", pair.MgvID1)
                masterlist.loc[masterlist['All_IDs'].apply(lambda x: pair.AncID1 in x), "All_IDs"].values[0] += transfer_IDs

man_fun.check_child_IDs(masterlist)
masterlist["All_IDs"] = masterlist["All_IDs"].apply(lambda x: sorted(x))
masterlist.to_csv(batchID+"/masterlist_"+batchID+".csv", index=False)

# make overview of superpedigrees and ped file
superped_dict = man_fun.make_superped_dict(masterlist, batchID)    
ped_ids = pd.read_csv(batchID+"/og_pedids_royals_example.csv", dtype=str) # CHECK
ped_dict = dict(zip(ped_ids["ogID"], ped_ids["PEDID"]))

with open(batchID+"/superpeds_"+batchID+".csv", "w") as f: # CHECK
    f.write("SupPEDID,ogIDs,PEDIDs\n")
    for i in superped_dict.keys():
        ped_ids_comb = sorted(set([ped_dict.get(ind.split("_")[1], "0") for ind in superped_dict[i]]))
        f.write(i+",\""+str(list(superped_dict[i]))+"\",\""+ \
              "_".join([str(ped_id) for ped_id in sorted(set(ped_ids_comb))])+"\"\n")

man_fun.double_check_parents(masterlist)
ped = man_fun.make_ped_file(masterlist)

# make overview of all probands
for i in superped_dict.keys():
    superped_inds = masterlist[masterlist["All_IDs"].apply(man_fun.check_superpedigree, args=([superped_dict[i]]))]["MgvID"].tolist()
    ped.loc[ped["ID"].isin(superped_inds), "SupPEDID"] = i

proband_IDs = masterlist[masterlist["All_IDs"].apply(man_fun.check_proband)]["MgvID"].tolist()
ped.loc[ped["ID"].isin(proband_IDs), "Proband"] = 1
ped.to_csv(batchID+"/ped_"+batchID+".csv", index=False) # CHECK

with open(batchID+"/proband_IDs_"+batchID+".csv", "w") as f: # CHECK
    f.write("MgvID,ogID,SupPEDID,PEDID\n")
    for MgvID in proband_IDs:
        IDlist = masterlist.loc[masterlist["MgvID"] == MgvID, "All_IDs"].values[0]
        ogID = [ID.split("_")[1] for ID in IDlist if  ID.endswith("_1")][0]
        SupPEDID = ped.loc[ped["ID"] == MgvID, "SupPEDID"].values[0]
        f.write(MgvID+","+ogID+","+ \
                (str(SupPEDID) if pd.notnull(SupPEDID) else "")+","+ped_dict.get(ogID, "")+"\n")
