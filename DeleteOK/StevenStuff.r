#############
# R code for combining CEDEN and SURF data based on common columns
#############

CEDEN <- read.csv("G:/Upper San Francisco Project/Data/CEDEN_SURF_Data/All_CEDEN_w_RegionID.csv") #read in CEDEN data

SURF < - read.csv("G:/Upper San Francisco Project/Data/CEDEN_SURF_Data/All_SURF_w_RegionID.csv") #read in SURF

CEDEN.subset <- CEDEN[, c("Region", "Analyte", "Result", "MDL", "SampleDate", "TargetLatitude",
                          "TargetLongitude", "CollectionMethodName", "Unit")] 

#subset CEDEN columns that exist as synonyms within SURF

SURF.subset <- SURF[, c("Region", "Chemical_name", "Concentration__ppb_", "Method_detection_level__ppb_",
                        "Sample_date", "Latitude", "Longitude", "Sample_type")] 

#subset SURF columns that exist as synonyms within CEDEN

SURF.subset["Unit"] <- "ug/L" #add column "Unit" to SURF. SURF data is ppb; ug/L is a close approximation based on USGS protocols

CEDEN.subset["Source"] <- "CEDEN" #add data source "CEDEN"

SURF.subset["Source"] <- "SURF" #add data source "SURF'

CEDEN.subset$SampleDate <- as.Date(CEDEN$SampleDate, format = "%m/%d/%Y") #format CEDEN date column

names(CEDEN)[names(CEDEN)=="SampleDate"] <- "Date" #Change coulmn name to "Date"

SURF.subset$Sample_date <- as.Date(SURF$Sample_date, format = "%d-%b-%y") #format SURF date column

names(SURF)[names(SURF)=="Sample_date"] <- "Date" #Change coulmn name to "Date"

new.col.names <- c("Region", "Analyte", "Result", "MDL", "Date", "Latitude", "Longitude",
                   "CollectionMethod", "Unit", "Source") #create vector of new column names for combined CEDEN-SURF dataset

CEDEN.colnames <- colnames(CEDEN.subset) #rename CEDEN subset column names, not sure if this is a necessary step

SURF.colnames <- colnames(SURF.subset) #rename SURF subset column names, not sure if this is a necessary step

colnames(CEDEN.subset) <- new.col.names #give CEDEN subset new colnames

colnames(SURF.subset) <- new.col.names #give SURF subset new colnames

CEDEN_SURF_comb <- rbind(CEDEN.subset, SURF.subset) #combine CEDEN and SURF subsets