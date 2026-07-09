import pandas as pd
from datetime import datetime
from jellyfish import damerau_levenshtein_distance
from haversine import haversine
import itertools

# NOTE: all functions based on anomalies in input so far, may need to be adjusted

def fix_name(name):
    # clean (first or last) names: remove spaces, keep first version of name (if separated by commas, slashes or brackets)
    if type(name) == str:
        name_fix = name.split("(")[0].split(",")[0].split("/")[0].strip()
        return(name_fix)
    else:
        return(name)

def get_initials(names):
    # get initial(s) from first name(s), first 3 letters of first name included to avoid FPs
    initials = names[:3]+"".join([x[0] for x in names.split()[1:]])
    return(initials)

def fix_sex(AncID, sex):
    # return sex based on records (probands) or kwartier ID (ancestors)
    sex_dict = {"Man":"M", "Woman":"F"}
    if AncID.endswith("_1"):
        sex_fix = sex
    elif AncID.endswith(("_2","_4","_6","_8","_10","_12","_14")):
        if not pd.isna(sex) and sex.lower().strip() != "man":
            print("Warning: female individual with male AncID:", AncID)
        sex_fix = "Man"
    else:
        if not pd.isna(sex) and sex.lower().strip() != "woman":
            print("Warning: male individual with female AncID", AncID)
        sex_fix = "Woman"
    return(sex_dict[sex_fix])

def fix_date(date):
    # clean dates: remove other characters, split merged day-month or month-year, correct impossible dates
    change = 0
    if type(date) == str or type(date) == int:
        date_in = str(date)
        date_clean = date_in.rstrip().lstrip().replace("+","").replace("!","1").replace("--","-").\
            replace("*", "01").replace("xx", "01").replace("00", "01").replace("XXXX", "1800")
        date_split = date_clean.split("-")
        if len(date_split) == 1:
            year = date_split[0]
            if len(year) == 4:
                day = "01"
                month = "01"
        elif len(date_split) == 2:
            day = date_split[0]
            year = date_split[1]
            if len(year) > 4:
                month = year[:-4]
                year = year[-4:]
            elif len(day) == 2:
                month = day[1:]
                day = day[:1]
            elif len(day) == 4:
                month = day[2:]
                day = day[:2]
        else:
            day = date_split[-3]
            month = date_split[-2]
            year = date_split[-1]
        day = day[:2]
        month = month[:2]
        year = year[:4]
        if int(month) > 12:
            if int(day) <= 12:
                month, day = day, month
            else:
                month = month[::-1]
        if (int(month) in [4,6,9,11] and int(day) == 31) or \
            (int(month) == 2 and int(day) == 29 and int(year)%4 != 0):
            day = int(day)-1
        elif int(day)-1 > 30 or (int(month) in [4,6,9,11] and int(day)-1 > 29) or \
            (int(month) == 2 and int(day)-1 > 27 and int(year)%4 != 0):
            day = day[::-1]
        if str(day)+"-"+str(month)+"-"+str(year) != date_in:
            change = 1
        return((datetime(int(year), int(month), int(day)).strftime("\'%Y-%m-%d"), change))
   
    elif type(date) == datetime:
        return((date.strftime("\'%Y-%m-%d"), change))

    else:
        return((date, change))
    
def record_match(rec1, rec2):
    # check if records of ancestors match based on name, place and date of birth
    init1 = rec1.Initials.lower()
    name1 = rec1.Last_name.lower()
    init2 = rec2.Initials.lower()
    name2 = rec2.Last_name.lower()
    if init1+name1 == init2+name2 or (init1 == init2 and \
                                      damerau_levenshtein_distance(name1, name2) < 2):
        
        dob1 = rec1.Date_of_birth
        dob2 = rec2.Date_of_birth
        if (dob1 == " " or dob2 == " ") or dob1 == dob2 or dob1[:-2] == dob2[:-2] or \
            damerau_levenshtein_distance(dob1, dob2) < 2:
        
            # dod1 = rec1.Sterfdatum
            # dod2 = rec2.Sterfdatum
            # if (dod1 == " " or dod2 == " ") or dod1 == dod2 or dod1[:-2] == dod2[:-2] or \
            #     damerau_levenshtein_distance(dod1, dod2) < 2:
                
                pob1 = rec1.Place_of_birth_code
                pob2 = rec2.Place_of_birth_code
                if (pob1 == " " or pob2 == " ") or pob1 == pob2 or \
                    haversine((float(pob1.split(",")[0]), float(pob1.split(",")[1])), \
                              (float(pob2.split(",")[0]), float(pob2.split(",")[1]))) < 20:
                    
                        prefix = "_".join(rec1.AncID.split("_")[:2])+"_"
                        suffix = int(rec1.AncID.split("_")[2])
                        if rec1.Sex == "M":
                            descendants = [prefix+str(int(suffix/n)) for n in [2,4,8]]
                        elif rec1.Sex == "F":
                            descendants = [prefix+str(int((suffix-1)/n)) for n in [2,4,8]]
                        if not any(ind in rec2.All_IDs for ind in descendants):
                            
                            if rec1.First_names == rec2.First_names and \
                                rec1.Last_name == rec2.Last_name and \
                                rec1.Date_of_birth == rec2.Date_of_birth and \
                                rec1.Place_of_birth == rec2.Place_of_birth:
                                return(True)
                            else:
                                return(False)

def check_child_IDs(masterlist):
    # check if children are linked to additional probands, add IDs
    for ind in masterlist.itertuples():    
        prefixes = ["_".join(i.split("_")[:2]) for i in ind.All_IDs]
        suffixes = [int(i.split("_")[2]) for i in ind.All_IDs]
        
        if ind.Sex == "M" and any([suff%2 != 0 and suff != 1 for suff in suffixes]):
            print("Warning: male individual with female AncIDs\n", ind)
        elif ind.Sex == "F" and any([suff%2 == 0 for suff in suffixes]):
            print("Warning: female individual with male AncIDs\n", ind)
        
        if any([suff > 7 for suff in suffixes]):
            if ind.Sex == "M":
                child_ogID_list = [pref + "_" + str(int(suff/2)) for pref,suff in zip(prefixes, suffixes)]
            else:
                child_ogID_list = [pref + "_" + str(int((suff-1)/2)) for pref,suff in zip(prefixes, suffixes)]
            
            child_ID_list = []
            for child_ogID in child_ogID_list:
                child_IDs = masterlist.loc[masterlist['All_IDs'].apply(lambda x: child_ogID in x), "All_IDs"].values[0]
                if child_IDs not in child_ID_list:
                    child_ID_list.append(child_IDs)
            
            i = 0
            for child_IDs in child_ID_list:
                extra_children = [ID for ID in child_IDs if "_".join(ID.split("_")[:2]) not in prefixes]
                if len(extra_children) > 0:
                    if ind.Sex == "M":
                        extra_IDs = ["_".join(c.split("_")[:2]) +"_"+ str(int(c.split("_")[2])*2) for c in extra_children]
                    else:
                        extra_IDs = ["_".join(c.split("_")[:2]) +"_"+ str((int(c.split("_")[2])*2)+1) for c in extra_children]
                    
                    masterlist.loc[ind.Index, "All_IDs"] += extra_IDs
                    i += 1
                    if i > 1:
                        print("Note: extra child IDs found from multiple individuals\n", ind)

def make_superped_dict(masterlist, batchID):
    # make dictionary of superpedigrees of matched probands
    superped_dict = {}
    superped_count = 0
    
    for ind in masterlist.itertuples():
        if len(ind.All_IDs) > 1:
            probands = ["_".join(ID.split("_")[:2]) for ID in ind.All_IDs]
            in_dict = "no"
            for i in superped_dict.keys():
                if not set(probands).isdisjoint(superped_dict[i]):
                    for proband in probands:
                        superped_dict[i].add(proband)
                    in_dict = "yes"
                    break
            if in_dict == "no":
                superped_count += 1
                superped_dict[f"{batchID}_{superped_count:04}"] = set(probands)
    superped_overlap = []
    for i,j in itertools.combinations(superped_dict.keys(), 2):
        if not superped_dict[j].isdisjoint(superped_dict[i]):
            superped_overlap.append([i,j])
    for overlap in superped_overlap:
        i,j = overlap
        if i not in superped_dict.keys() or j not in superped_dict.keys():
            print("Warning: one of overlapping superpedigree IDs no longer in dictionary\n", i,j)
            continue
        for proband in superped_dict[j]:
            superped_dict[i].add(proband)
        superped_dict.pop(j)
    superped_dict = dict(zip([f"{batchID}_{i:04}" for i in range(1,len(superped_dict)+1)], superped_dict.values()))
    return(superped_dict)

def double_check_parents(masterlist, WWW_probands = []):
    # double check if records with multiple probands map to one parent pair
    for ind in masterlist.itertuples():
        if len(ind.All_IDs) > 1:
            if len(set(ind.All_IDs)) < len(ind.All_IDs):
                print("Warning: duplicate AncIDs present in ID list\n", ind)
            fathers = []
            mothers = []
            for ID in ind.All_IDs:
                prefix = "_".join(ID.split("_")[:2])
                suffix = int(ID.split("_")[2])
            
                if suffix < 8 or prefix in WWW_probands:
                    pat_ogID = prefix + "_" + str(suffix*2)
                    mat_ogID = prefix + "_" + str((suffix*2)+1)
                    
                    if len(masterlist[masterlist['All_IDs'].apply(lambda x: pat_ogID in x)]) > 0:
                        pat_ALP = masterlist.loc[masterlist['All_IDs'].apply(lambda x: pat_ogID in x), "MgvID"].values[0]
                        fathers.append(pat_ALP)
                    if len(masterlist[masterlist['All_IDs'].apply(lambda x: mat_ogID in x)]) > 0:
                        mat_ALP = masterlist.loc[masterlist['All_IDs'].apply(lambda x: mat_ogID in x), "MgvID"].values[0]
                        mothers.append(mat_ALP)
            if len(set(fathers)) > 1 or len(set(mothers)) > 1:
                print("Warning: IDs in record map do not map to one parent pair\n", ind, fathers, mothers)            

def make_ped_file(masterlist, WWW_probands = []):
    # make ped file with paternal and maternal IDs for each individual
    rowlist = []
    for ind in masterlist.itertuples():
        pat_ALP, mat_ALP = "", ""
        
        prefixes = ["_".join(i.split("_")[:2]) for i in ind.All_IDs]
        suffixes = [int(i.split("_")[2]) for i in ind.All_IDs]
        closest = min(suffixes)
            
        if closest < 8 or all([prefix in WWW_probands for prefix in prefixes]):
            prefix = prefixes[suffixes.index(closest)]
            suffix = suffixes[suffixes.index(closest)]
    
            pat_ogID = prefix + "_" + str(suffix*2)
            mat_ogID = prefix + "_" + str((suffix*2)+1)
            
            if len(masterlist[masterlist['All_IDs'].apply(lambda x: pat_ogID in x)]) == 1:
                pat_ALP = masterlist.loc[masterlist['All_IDs'].apply(lambda x: pat_ogID in x), "MgvID"].values[0]
            elif len(masterlist[masterlist['All_IDs'].apply(lambda x: pat_ogID in x)]) > 1:
                print("Warning: multiple fathers present for record\n", ind, pat_ogID)
            if len(masterlist[masterlist['All_IDs'].apply(lambda x: mat_ogID in x)]) == 1:
                mat_ALP = masterlist.loc[masterlist['All_IDs'].apply(lambda x: mat_ogID in x), "MgvID"].values[0]
            elif len(masterlist[masterlist['All_IDs'].apply(lambda x: mat_ogID in x)]) > 1:
                print("Warning: multiple mothers present for record\n",ind, mat_ogID)
            
        rowlist.append(pd.DataFrame({"ID":ind.MgvID, "Father":pat_ALP, "Mother":mat_ALP, "Sex":ind.Sex}, \
                                    index=[int(ind.MgvID[3:])]))           
    return(pd.concat(rowlist))

def check_proband(IDlist):
    # check if person is proband in a family
    return(any([ID.endswith("_1") for ID in IDlist]))

def check_superpedigree(IDlist, superped_IDs):
    # check if IDs associated with person are part of superpedigree
    return(all("_".join(ID.split("_")[:2]) in superped_IDs for ID in IDlist))
