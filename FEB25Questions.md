---
title: "OG Data"
author: "Erika W"
date: "2/25/2021"
output:
  html_document:
    code_download: true
    keep_md: true
    toc: true
    toc_float:
      toc_collapsed: true
    toc_depth: 3
    theme: lumen
---




```r
rm(list = ls())
library(data.table)
library(lubridate)
library(sf)
library(tidyverse)
library(zoo)
```

## Morning Questions

#### 1. Does changing projection change lat/long data? 
--> YES it will... but Skyler will be fixing those by EOD today


#### 2. Does ComID column exist in our datasets?

--> No it doesn't. Ask Dan about that if lat/long doesn't work.

#### 3. Missing columns from mod data we want?

--> Low confidence in SURF being original data - missing many columns used by Dan to differentiate composite samples

--> CEDEN data column **AnalyteFraction** described in guidelines but not in our data

--> CEDEN data column **CollectionDeviceDescription** might be helpful under some circumstances, ie: to know when integrated depth sampling was done. 


## Load Data 

**These questions were queried using the originally downloaded CEDEN data and CEDEN_Mod, prior to 2/25 updates**

**See Q4 for comparison of original WQ to new WQ**

CEDEN Water


```r
# Read in Data
CEDEN.wq <- fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/CV_WaterQuality_20201241641.txt")
```

```
## Warning in fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/
## CV_WaterQuality_20201241641.txt"): Found and resolved improper quoting out-of-
## sample. First healed line 72831: <<Sacramento River Watershed Program Sacramento
## River Watershed Program Sacramento River Watershed Program 1999 Sacramento River
## at Veterans Bridge (SVWQC) 519XSRAVB 1999-12-13 11:05:00 Not Recorded -88 m
## Integrated 1 1 99SRWPWQ_NotRecorded S912185-03 samplewater EPA 200.7 Iron, Total
## mg/L 0.356 -88 0.01 = NR NR NR "-SRVET-98" and "-SRVET-99" reported as a single
## combined sample (i.e., "-SRVET-98/99") by Sequoia. WQ SRWP QAPP 2003-2004 SRWTP
## Not Recorded 38.674683 -121.627517700195 Not Record>>. If the fields are not
## quoted (e.g. field separator does not appear within any field), try quote="" to
## avoid this warning.
```


```r
# ID start and end of data range
zSeq<- zoo(CEDEN.wq, as.Date(CEDEN.wq$SampleDate))
```

```
## Warning in zoo(CEDEN.wq, as.Date(CEDEN.wq$SampleDate)): some methods for "zoo"
## objects do not work if the index entries in 'order.by' are not unique
```

```r
start(zSeq)
```

```
## [1] "1991-08-02"
```

```r
end(zSeq)
```

```
## [1] "2019-10-30"
```


```r
# Read in Data
CEDEN.tox <- fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/CV_Toxicity_202012911342.txt")
```

```
## Warning in fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/
## CV_Toxicity_202012911342.txt"): Found and resolved improper quoting out-of-
## sample. First healed line 5845: <<Sacramento River Watershed Program Sacramento
## River Watershed Program Sacramento River Watershed Program 2000 Arcade Creek at
## Norwood Ave. ARCNW 2000-10-18 16:05:00 -88 Site1 cm Grab 1 0 SRWP_PER_CD_102700
## Not Recorded samplewater Not Recorded 7 days Ceriodaphnia dubia Oxygen,
## Dissolved, Not Recorded mg/L overlyingwater high 12.8 = NR NR NR Com Atox
## "Baseline" test (unmanipulated ARCNW) for TIE; retest 2 WQ SRWP QAPP 2003-2004
## Pacific Ecorisk 0 Water_Grab 38.6245 -121.456 Peristaltic Water >>. If the
## fields are not quoted (e.g. field separator does not appear within any field),
## try quote="" to avoid this warning.
```


```r
# ID start and end of data range
zSeq<- zoo(CEDEN.tox, as.Date(CEDEN.tox$SampleDate))
```

```
## Warning in zoo(CEDEN.tox, as.Date(CEDEN.tox$SampleDate)): some methods for "zoo"
## objects do not work if the index entries in 'order.by' are not unique
```

```r
start(zSeq)
```

```
## [1] "1993-03-05"
```

```r
end(zSeq)
```

```
## [1] "2019-12-03"
```

SURF Water


```r
# Read in data
SURF.wq <- fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/SURF_water.csv")
```


```r
# ID start and end of data range
zSeq <- zoo(SURF.wq, as.Date(SURF.wq$Sample_date)) 
```

```
## Warning in zoo(SURF.wq, as.Date(SURF.wq$Sample_date)): some methods for "zoo"
## objects do not work if the index entries in 'order.by' are not unique
```

```r
start(zSeq)
```

```
## [1] "1925-06-27"
```

```r
end(zSeq)
```

```
## [1] "2020-04-14"
```


```r
# Read in data
SURF.sed <- fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/SURF_SED.csv")
```


```r
# ID start and end of data range
zSeq <- zoo(SURF.sed, as.Date(SURF.sed$Sample_date)) 
```

```
## Warning in zoo(SURF.sed, as.Date(SURF.sed$Sample_date)): some methods for "zoo"
## objects do not work if the index entries in 'order.by' are not unique
```

```r
start(zSeq)
```

```
## [1] "1970-10-15"
```

```r
end(zSeq)
```

```
## [1] "2020-06-02"
```

## Question 2. 


```r
# Look for comID column - CEDEN
C<- unique(names(CEDEN.wq))
C[grepl('Com', C)]
```

```
## [1] "ComplianceCode"           "SampleComments"          
## [3] "CollectionComments"       "ResultsComments"         
## [5] "BatchComments"            "LocationDetailWQComments"
## [7] "LocationDetailBAComments"
```

```r
C[grepl('ID', C)]
```

```
## [1] "LabSampleID" "SampleID"
```


```r
# Look for comID column - SURF
S <- unique(names(SURF.wq))
S[grepl("Com", S)]
```

```
## character(0)
```

```r
S[grepl("date", S)]
```

```
## [1] "Sample_date"
```

## Question 3: Missing columns we should keep?

### SURF

Dan said she uses the following columns:

SampleType
coll_meth_cd
anly_meth_cd
sampler_cd
samp_type_cd
storm_flag

SURF_mod uses all the columns present in the "original" SURF data on our harddrive. That said, it is missing 5 of 6 columns that Dan named:

1. coll_meth_cd
2. anyl_meth_cd (less interested in this one)
3. sampler_cd
4. samp_type_cd
5. storm_flag

This suggests that we do not have all the data. 


```r
names(SURF.sed)
```

```
##  [1] "County"                        "Site_code"                    
##  [3] "Site_name"                     "Latitude"                     
##  [5] "Longitude"                     "Sample_date"                  
##  [7] "Chemical_name"                 "Concentration..ppb."          
##  [9] "Level_of_quantification..ppb." "Method_detection_level..ppb." 
## [11] "Sample_type"                   "Total organic carbon (%)"     
## [13] "Study_cd"                      "Study_description"            
## [15] "Study_weblink"                 "Data.source"                  
## [17] "Agency"                        "Record_id"
```

### CEDEN columns we ditched/should keep?

I went to the CEDEN online guidance to understand the columns and determine which are most helpful:
http://ceden.org/docs/2019_documentation/ceden_tox_guidance_2019_0108.pdf

Many of the descriptions referenced SWAMP LookUpLists for further detail: https://swamp.waterboards.ca.gov/swamp_checker/LookUpLists.aspx

If we care about depth-integrated samples, we should include the **"CollectionDeviceDescription"** column

It is interestign to note that the CEDEN data does not have an **AnalyteFraction** column, which the CEDEN instructions suggest is a separate record from analyte, and should be where designate total vs unionized. Incorrect data upload practices if not separated? CEDEN QA should catch?

All other columns omitted seem fine to me.

#### Tox data


```r
unique(CEDEN.tox$CollectionMethodName)
```

```
## [1] "Water_Grab"                          
## [2] "Sediment_Grab"                       
## [3] "Not Recorded"                        
## [4] "Sediment_Core"                       
## [5] "Auto sampler automatically triggered"
```

```r
# None suggesting an issue; maybe auto sampler automatically triggered.

# unique(CEDEN.tox$SampleTypeCode)
# Non-issue

# unique(CEDEN.tox$CollectionDeviceDescription)
# Collection device less helpful
```

#### WQ Data


```r
unique(CEDEN.wq$SampleTypeCode) 
```

```
## [1] "Grab"         "Not Recorded" "Integrated"   "Field_Grab"
```

```r
# None indicative of samples over time. Good

unique(CEDEN.wq$CollectionMethodName)
```

```
##  [1] "Water_Grab"                          
##  [2] "Sediment_Grab"                       
##  [3] "24 hour auto sampler"                
##  [4] "4 day auto sampler"                  
##  [5] "Field Method"                        
##  [6] "7 day auto sampler"                  
##  [7] "Not Recorded"                        
##  [8] "Algae_EMAP"                          
##  [9] "Not Applicable"                      
## [10] "Algae_SWAMP"                         
## [11] "AutoSampler"                         
## [12] "Auto sampler manually triggered"     
## [13] "Auto sampler automatically triggered"
## [14] "Algae_EPA_NWS"                       
## [15] "Habitat_SWAMP"                       
## [16] "Other"
```

```r
# 24 hour auto sampler, 4 day auto sampler, 7 day auto sampler = Concerning

# unique(CEDEN.wq$CollectionDeviceDescription)
# Collection Device does show where depth-integrating devices used, but lots of non-issue.
```

## Question 4. What happened to all our WQ points?

The original "CEDENMod_WQ had 117,114 records, of which about 1,536 were removed as presumed duplicates during my analyses.

After separating analyte from modifiers, there had been 910 unique analytes. 

In the new data, there are only 60,429 records to start with, and 757 unique analytes.

What are these differences?


```r
# Create Column Selection List
CEDEN.selectList <-
  c("ID", "Analyte", "Result", "OrganismName", "Project", "ParentProject", "Program", "VariableResult", "MatrixName", "StationName", "StationCode", "LocationCode", "MDL", "RL", "CommonName", "TissueName", "Date"="SampleDate", "Latitude"="TargetLatitude", "Longitude"="TargetLongitude", "CollectionMethod"="CollectionMethodName", "Unit", "Datum", "regional_board", "rb_number", "Phylum", "Class", "Orders", "Family", "Genus", "Species", "Counts", "BAResult")

CEDEN.WQ_New <- fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/CEDEN_WQ_20212259571.txt", 
                      colClasses = c(Result = 'numeric'))%>%
  select(any_of(CEDEN.selectList))
```

```
## Warning in fread("C:/Users/Erika/Documents/GitHub/CEDENSURF-mod/Data/
## CEDEN_WQ_20212259571.txt", : Found and resolved improper quoting out-of-
## sample. First healed line 46522: <<Surface Water Ambient Monitoring Program
## SWAMP Perennial Stream Surveys Statewide Perennial Streams Assessment 2010
## Fairfax Creek above Senic Rd. 203PS0070 2010-07-19 13:00:00 X 0.1 m Grab 1
## 1 WPCL_6448_W_OPO4 L-413-10-01 samplewater EPA 365.1M OrthoPhosphate as P,
## Dissolved mg/L 0.0270 0.0020 0.0050 = BV VAC Qual collection time updated
## to time from WPCL LIMs "BV", sample received and ran outside holding time.
## BA SWAMP_2007_WS DFW-ABL Water_Grab 37.990532 -122.593185424805 Individual
## Collec>>. If the fields are not quoted (e.g. field separator does not appear
## within any field), try quote="" to avoid this warning.
```

```r
# Returns 1024111 obsv

CEDEN.WQ_New <- CEDEN.WQ_New %>%
  filter(!Latitude == 'NULL')  %>%
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))

## TELL SKYLER W/IN FILTER() NEEDS TO BE CHANGED TO LATITUDE

# Returns 1024104 obsv

CEDEN.WQ_New <- CEDEN.WQ_New %>% 
  filter(Result != "NA") # remove character NULLs

# Returns 1024104  (no fewer)

CEDEN.WQ_NUM <- CEDEN.WQ_New %>% 
  filter(!is.na(as.numeric(as.character(Result))))
```

```
## Warning in mask$eval_all_filter(dots, env_filter): NAs introduced by coercion
```

```r
# returns only 75,3825

DIFF <- setdiff(CEDEN.WQ_New$Result, CEDEN.WQ_NUM$Result)
 
DIFF
```

```
## [1] ""       "NR"     "none"   "Clear"  "Brown"  "Cloudy" ".289.6"
```

Should we retain the one with "none"? Nope, all odor. Sweet.


```r
CEDEN.WQ_New[CEDEN.WQ_New$Result == "none"]
```

```
##    Analyte Result                                       Project
## 1:    Odor   none CA Dept of Transit NPDES Monitoring 2014-2015
## 2:    Odor   none CA Dept of Transit NPDES Monitoring 2014-2015
## 3:   Color   none CA Dept of Transit NPDES Monitoring 2014-2015
## 4:    Odor   none CA Dept of Transit NPDES Monitoring 2014-2015
## 5:    Odor   none CA Dept of Transit NPDES Monitoring 2014-2015
## 6:   Color   none CA Dept of Transit NPDES Monitoring 2014-2015
## 7:    Odor   none CA Dept of Transit NPDES Monitoring 2014-2015
##                                    ParentProject
## 1: CA Dept of Transit NPDES Monitoring 2014-2015
## 2: CA Dept of Transit NPDES Monitoring 2014-2015
## 3: CA Dept of Transit NPDES Monitoring 2014-2015
## 4: CA Dept of Transit NPDES Monitoring 2014-2015
## 5: CA Dept of Transit NPDES Monitoring 2014-2015
## 6: CA Dept of Transit NPDES Monitoring 2014-2015
## 7: CA Dept of Transit NPDES Monitoring 2014-2015
##                                         Program  MatrixName StationName
## 1: CA Department of Transportation NPDES Permit samplewater  FIT012-ORW
## 2: CA Department of Transportation NPDES Permit samplewater      FIT012
## 3: CA Department of Transportation NPDES Permit samplewater  FIT012-ORW
## 4: CA Department of Transportation NPDES Permit samplewater  FIT012-ORW
## 5: CA Department of Transportation NPDES Permit samplewater      FIT012
## 6: CA Department of Transportation NPDES Permit samplewater  FIT012-ORW
## 7: CA Department of Transportation NPDES Permit samplewater  FIT012-ORW
##    StationCode LocationCode MDL RL       Date Latitude Longitude
## 1:       4-342 Not Recorded  NA NA 2014-02-06  37.5308  -122.518
## 2:       4-341 Not Recorded  NA NA 2014-02-06  37.5311  -122.518
## 3:       4-342 Not Recorded  NA NA 2014-02-06  37.5308  -122.518
## 4:       4-342 Not Recorded  NA NA 2014-02-25  37.5308  -122.518
## 5:       4-341 Not Recorded  NA NA 2014-02-26  37.5311  -122.518
## 6:       4-342 Not Recorded  NA NA 2014-02-26  37.5308  -122.518
## 7:       4-342 Not Recorded  NA NA 2014-02-26  37.5308  -122.518
##    CollectionMethod Unit    regional_board rb_number
## 1:     Field Method none San Francisco Bay         2
## 2:     Field Method none San Francisco Bay         2
## 3:     Field Method none San Francisco Bay         2
## 4:     Field Method none San Francisco Bay         2
## 5:     Field Method none San Francisco Bay         2
## 6:     Field Method none San Francisco Bay         2
## 7:     Field Method none San Francisco Bay         2
```


