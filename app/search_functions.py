import pandas as pd
from jellyfish import damerau_levenshtein_distance
from haversine import haversine

# read in geocodes
with open("../royals/geocodes.txt", "r") as f: # CHECK
  city_codes = dict(x.rstrip().split(";", 1) for x in f)
with open("../royals/geocodes_manual.txt", "r") as f: # CHECK
  city_codes.update(dict(x.rstrip().split(";", 1) for x in f))

def search_ogID(IDlist, ogID_list, multiple):
    # search individuals based on one or more ALS numbers
    if multiple:
        if any([any([ogID in ID for ID in IDlist]) for ogID in ogID_list]):
            return(True)
        else:
            return(False)
    else:
        if any([ogID_list in ID for ID in IDlist]):
            return(True)
        else:
            return(False)

def search_ogID_IDs(ogID_list, masterlist, multiple):
    # get MgvIDs of individuals from ALS number search
    masterlist["All_IDs"] = masterlist["All_IDs"].apply(lambda x: x[1:-1].split(','))
    IDs = masterlist[masterlist["All_IDs"].apply(search_ogID, args=[ogID_list, multiple])]["MgvID"].tolist()
    return(IDs)
        
def search_name(name_target, name_query, DL, contains):
    # search individuals based on name, allowing typos or containing string
    name_target_low = name_target.lower()
    name_query_low = name_query.lower()
    if name_target_low == name_query_low or damerau_levenshtein_distance(name_target_low, name_query_low) <= DL:
        return(True)
    elif contains and name_query_low in name_target_low:
        return(True)
    else:
        return(False)
    
def search_name_IDs(name_query, masterlist, DL, contains):
    # get MgvIDs of individuals from name search
    IDs = masterlist[masterlist["Last_name"].apply(search_name, args=[name_query, DL, contains])]["MgvID"].tolist()
    return(IDs)
    
def search_city(city_target_code, city_query, city_query_code, dist):
    # search individuals based on place of birth, allowing distance range
    if city_target_code == " ":
        return(False)
    elif city_target_code == city_query_code or \
        haversine((float(city_target_code.split(",")[0]), float(city_target_code.split(",")[1])), \
                  (float(city_query_code.split(",")[0]), float(city_query_code.split(",")[1]))) < dist:
        return(True)
    else:
        return(False)
    
def search_city_IDs(city_query, masterlist, dist=10):
    # get MgvIDs of individuals from place of birth search
    if city_query in city_codes.keys():
      city_query_code = city_codes[city_query]
      IDs = masterlist[masterlist["Place_of_birth_code"].apply(search_city, args=[city_query, city_query_code, dist])]["MgvID"].tolist()
      return(IDs)
        
def search_date(date_target, date_query, DL, day):
    # search individuals based on date of birth/death, allowing typos or different day
    if date_target == date_query or damerau_levenshtein_distance(date_target, date_query) <= DL:
        return(True)
    elif day and date_target[:-2] == date_query[:-2]:
        return(True)
    else:
        return(False)
    
def search_dob_IDs(date_query, masterlist, DL, day):
    # get MgvIDs of individuals from date of birth/death search
    IDs = masterlist[masterlist["Date_of_birth"].apply(search_date, args=[date_query, DL, day])]["MgvID"].tolist()
    return(IDs)
    
def search_dod_IDs(date_query, masterlist, DL, day):
    # get MgvIDs of individuals from date of birth/death search
    IDs = masterlist[masterlist["Death_of_death"].apply(search_date, args=[date_query, DL, day])]["MgvID"].tolist()
    return(IDs)

def get_MgvIDs(searchtype, query, masterlist, ped_data, proband_IDs):
    # get MgvIDs of individuals in superpedigree or ancestor table
    masterlist["All_IDs"] = masterlist["All_IDs"].apply(lambda x: x[1:-1].split(','))
    if searchtype == "SupPEDID":
        return({"MgvIDs":ped_data.loc[ped_data["SupPEDID"]==query, "ID"].values.tolist(), "title":"Superpedigree "+query})
    elif searchtype == "PEDID":
        SupPEDID = sorted(list(set(proband_IDs.loc[proband_IDs["PEDID"]==query, "SupPEDID"].values)))
        if len(SupPEDID)==0:
            ogID = proband_IDs.loc[proband_IDs["PEDID"]==query, "ogID"].values[0]
            MgvIDs = masterlist.loc[masterlist["All_IDs"].apply(lambda x: any([ogID in ID for ID in x])), "MgvID"].values.tolist()
            return({"MgvIDs":MgvIDs, "title":"Ancestor table "+ogID})
        elif len(SupPEDID)==1:
            return({"MgvIDs":ped_data.loc[ped_data["SupPEDID"]==SupPEDID[0], "ID"].values.tolist(), "title":"Superpedigree "+SupPEDID[0]})
        else:
            return({"MgvIDs":ped_data.loc[ped_data["SupPEDID"]==SupPEDID[0], "ID"].values.tolist(), \
                    "title":"Superpedigree "+SupPEDID[0]+"\nNote: there are more probands from pedigree "+query+" in superpedigrees "+", ".join(SupPEDID[1:])})
    elif searchtype == "MgvID":
        SupPEDID = ped_data.loc[ped_data["ID"]==query, "SupPEDID"].values[0]
        if SupPEDID=="":
            ogID = masterlist.loc[masterlist["MgvID"]==query, "All_IDs"].values[0][0].split("_")[1]
            MgvIDs = masterlist.loc[masterlist["All_IDs"].apply(lambda x: any([ogID in ID for ID in x])), "MgvID"].values.tolist()
            return({"MgvIDs":MgvIDs, "title":"Ancestor table "+ogID})
        else:
            return({"MgvIDs":ped_data.loc[ped_data["SupPEDID"]==SupPEDID, "ID"].values.tolist(), "title":"Superpedigree "+SupPEDID})
    elif searchtype == "ogID":
        SupPEDID = proband_IDs.loc[proband_IDs["ogID"]==query, "SupPEDID"].values[0]
        if SupPEDID=="":
            MgvIDs = masterlist.loc[masterlist["All_IDs"].apply(lambda x: any([query in ID for ID in x])), "MgvID"].values.tolist()
            return({"MgvIDs":MgvIDs, "title":"Ancestor table "+query})
        else:
            return({"MgvIDs":ped_data.loc[ped_data["SupPEDID"]==SupPEDID, "ID"].values.tolist(), "title":"Superpedigree "+SupPEDID})
