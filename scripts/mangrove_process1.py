import sys
import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter

sys.path.insert(1, "scripts/")
import mangrove_process_functions as man_fun

na_values = ["X"]

in_file = "royals_example.xlsx" # CHECK
batchID = "royals" # CHECK
prev_batch = "" # CHECK

# read in data, clean records
kwartier = pd.read_excel(batchID+"/"+in_file, na_values=na_values) # CHECK

kwartier["AncID"] = batchID +"_"+ kwartier["AncID"]
kwartier["First_names"] = kwartier["First_names"].apply(man_fun.fix_name)
kwartier["Last_name"] = kwartier["Last_name"].apply(man_fun.fix_name)
kwartier = kwartier[~((kwartier["First_names"].isna()) | (kwartier["Last_name"].isna()))]

kwartier["Initials"] = kwartier["First_names"].apply(man_fun.get_initials)
kwartier["Sex"] = kwartier.apply(lambda x: man_fun.fix_sex(x.AncID, x.Sex), axis=1)
kwartier["Date_of_birth"] = kwartier["Date_of_birth"].apply(man_fun.fix_date)
kwartier["Date_of_death"] = kwartier["Date_of_death"].apply(man_fun.fix_date)

# get geocodes for all unique cities, add to records
geolocator = Nominatim(user_agent="geopy_mangrove")
geocode = RateLimiter(geolocator.geocode, min_delay_seconds=1)

kwartier["Place_of_birth"]=kwartier["Place_of_birth"].str.lower().str.strip()
cities = kwartier["Place_of_birth"].dropna().unique()

with open("royals/geocodes.txt", "r") as f: # CHECK
  city_codes = dict(x.rstrip().split(";", 1) for x in f)
with open("royals/geocodes_manual.txt", "r") as f: # CHECK
  city_codes.update(dict(x.rstrip().split(";", 1) for x in f))
  
for city in cities:
    if city not in city_codes:
        gps = geocode({"city":city}, timeout=10) # if applicable: add "country:[country]" in the dictionary
        if gps == None:
            print(city)
        else:
            city_codes[city] = str(gps.latitude)+","+str(gps.longitude)
            with open("geocodes.txt", "a") as f:
                f.write(city+";"+str(gps.latitude)+","+str(gps.longitude)+"\n")

kwartier = kwartier.assign(Place_of_birth_code = kwartier.Place_of_birth.map(city_codes))
kwartier = kwartier.fillna(" ")

# check all new individuals against masterlist to detect matches
if len(prev_batch) > 0:
    masterlist = pd.read_csv(prev_batch+"/masterlist_"+prev_batch+".csv", converters={"All_IDs": pd.eval})
    id_count = max(masterlist["MgvID"].str.lstrip("Mgv").astype(int))
else:
    masterlist = pd.DataFrame(columns=["MgvID","First_names","Initials","Last_name", \
                                       "Place_of_birth","Place_of_birth_code","Date_of_birth","Date_of_death", \
                                       "Sex","All_IDs"])
    id_count = 0

detected_matches = []

for rec1 in kwartier.itertuples():
    id_count += 1
    MgvID = f"Mgv{id_count:06}"
    for rec2 in masterlist[masterlist["Sex"] == rec1.Sex].itertuples():
        matching = man_fun.record_match(rec1, rec2)
        if matching != None:
            detected_matches.append(pd.DataFrame({"AncID1":rec1.AncID, "MgvID1":MgvID, \
                                                  "First_names_1":rec1.First_names, "Last_name_1":rec1.Last_name, \
                                                  "Place_of_birth_1":rec1.Place_of_birth, "Date_of_birth_1":rec1.Date_of_birth,\
                                                  "AncIDs2":[rec2.All_IDs], "MgvID2":rec2.MgvID, \
                                                  "First_names_2":rec2.First_names, "Last_name_2":rec2.Last_name, \
                                                  "Place_of_birth_2":rec2.Place_of_birth, "Date_of_birth_2":rec2.Date_of_birth, \
                                                  "Exact":matching}))
    masterlist = pd.concat([masterlist, pd.DataFrame({"MgvID":MgvID,"First_names":rec1.First_names, \
                                                     "Initials":rec1.Initials,"Last_name":rec1.Last_name, \
                                                     "Place_of_birth":rec1.Place_of_birth,"Place_of_birth_code":rec1.Place_of_birth_code, \
                                                     "Date_of_birth":rec1.Date_of_birth,"Date_of_death":rec1.Date_of_death, \
                                                     "Sex":rec1.Sex,"All_IDs":[[rec1.AncID]]})], ignore_index=True)

masterlist.to_csv(batchID+"/masterlist_"+batchID+"_unmatched.csv", index=False) # CHECK
pd.concat(detected_matches, ignore_index=True).to_csv(batchID+"/matches_"+batchID+".csv", index=False) # CHECK
