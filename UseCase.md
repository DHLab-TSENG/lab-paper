A use case in lab package paper
================
Yi-Ju Tseng, Chun Ju Chen

*lab*: An R package for generating analysis-ready data from laboratory
records

``` r
remotes::install_github("DHLab-TSENG/lab")
remotes::install_github("DHLab-TSENG/dxpr")
```

``` r
library(dxpr)
library(lab)
library(dplyr)
library(data.table)
```

``` r
load("newborn/newborn_ICD.rda")
```

# Case Identification (with dxpr package)

``` r
Case <- selectCases(dxDataFile = newborn_ICD,
                    idColName = SUBJECT_ID,
                    icdColName = ICD9_CODE,
                    dateColName = ADMITTIME,
                    icd10usingDate = "9999/01/01",
                    groupDataType = ICD,
                    caseCondition = "^7470",
                    caseCount = 1,
                    caseName = "PDA")
table(Case$selectedCase)
```

    ## 
    ## non-PDA     PDA 
    ##    7452     381

``` r
head(Case[selectedCase=="PDA"])
```

|    ID | selectedCase | count | firstCaseDate | endCaseDate | period | MostCommonICD | MostCommonICDCount |
|------:|:-------------|------:|:--------------|:------------|:-------|:--------------|-------------------:|
| 15188 | PDA          |     2 | 2146-05-06    | 2146-05-15  | 9 days | 7470          |                  2 |
|    72 | PDA          |     1 | 2163-09-22    | 2163-09-22  | 0 days | 7470          |                  1 |
|    93 | PDA          |     1 | 2128-03-17    | 2128-03-17  | 0 days | 7470          |                  1 |
|   126 | PDA          |     1 | 2129-08-10    | 2129-08-10  | 0 days | 7470          |                  1 |
|   139 | PDA          |     1 | 2177-09-30    | 2177-09-30  | 0 days | 7470          |                  1 |
|   517 | PDA          |     1 | 2158-09-09    | 2158-09-09  | 0 days | 7470          |                  1 |

``` r
head(Case[selectedCase=="non-PDA"])
```

|  ID | selectedCase | count | firstCaseDate | endCaseDate | period  | MostCommonICD | MostCommonICDCount |
|----:|:-------------|------:|:--------------|:------------|:--------|:--------------|-------------------:|
|   2 | non-PDA      |    NA | NA            | NA          | NA days | NA            |                 NA |
|   5 | non-PDA      |    NA | NA            | NA          | NA days | NA            |                 NA |
|   7 | non-PDA      |    NA | NA            | NA          | NA days | NA            |                 NA |
|   8 | non-PDA      |    NA | NA            | NA          | NA days | NA            |                 NA |
|  10 | non-PDA      |    NA | NA            | NA          | NA days | NA            |                 NA |
|  16 | non-PDA      |    NA | NA            | NA          | NA days | NA            |                 NA |

# Load Laboratory Records of Cases and Controls

Load laboratory records. Data is not included due to the terms of use.
Please check [Medical Information Mart for Intensive
Care](https://mimic.mit.edu/) for more information.

``` r
if(file.exists("newborn/PDALab.rds")){
  PDALab<-readRDS("newborn/PDALab.rds")
}else{
  LABEVENTS <- fread("newborn/LABEVENTS.csv")
  PDALab<-LABEVENTS[SUBJECT_ID %in% Case$ID]
  PDALab<-PDALab %>% select(-ROW_ID,-HADM_ID)
  saveRDS(PDALab,"newborn/PDALab.rds")
}
LONICMap<-fread("newborn/D_LABITEMS.csv")
LONICMap<-LONICMap %>% select(-ROW_ID)
Patients<-fread("newborn/PATIENTS.csv")
Patients<-Patients %>% select(-ROW_ID)
```

Select laboratory tests which were given to more than 50% of individuals
in the study population.

``` r
PDAItem50<-
  PDALab %>% group_by(ITEMID) %>%
  summarise(Ind=n_distinct(SUBJECT_ID),
            Total=length(unique(PDALab$SUBJECT_ID)),
            Perc=Ind/Total) %>%
  arrange(desc(Perc)) %>%
  filter(Perc>0.5)
PDALab50<-PDALab %>% filter(ITEMID %in% PDAItem50$ITEMID)
```

``` r
head(PDALab50)
```

| SUBJECT_ID | ITEMID | CHARTTIME           | VALUE | VALUENUM | VALUEUOM | FLAG     |
|-----------:|-------:|:--------------------|:------|---------:|:---------|:---------|
|          2 |  51143 | 2138-07-17 20:48:00 | 0     |        0 | %        |          |
|          2 |  51144 | 2138-07-17 20:48:00 | 0     |        0 | %        |          |
|          2 |  51146 | 2138-07-17 20:48:00 | 0     |        0 | %        |          |
|          2 |  51200 | 2138-07-17 20:48:00 | 0     |        0 | %        |          |
|          2 |  51221 | 2138-07-17 20:48:00 | 0     |        0 | %        | abnormal |
|          2 |  51222 | 2138-07-17 20:48:00 | 0     |        0 | g/dL     | abnormal |

# LONIC Mapping

Map laboratory item code (ITEMID) with LOINC. The mapping table
`LONICMap` is provided by [MIMIC](https://mimic.mit.edu/)

``` r
PDALabLONIC <- mapLOINC(labData = PDALab50, 
                        labItemColName = ITEMID, 
                        mappingTable = LONICMap)
```

``` r
head(PDALabLONIC)
```

| ITEMID | SUBJECT_ID | CHARTTIME           | VALUE | VALUENUM | VALUEUOM | FLAG | LABEL             | FLUID | CATEGORY  | LOINC_CODE |
|-------:|-----------:|:--------------------|:------|---------:|:---------|:-----|:------------------|:------|:----------|:-----------|
|  50883 |          2 | 2138-07-20 06:30:00 | 0.3   |      0.3 | mg/dL    |      | Bilirubin, Direct | Blood | Chemistry | 1968-7     |
|  50883 |          7 | 2121-05-26 22:00:00 | 0.3   |      0.3 | mg/dL    |      | Bilirubin, Direct | Blood | Chemistry | 1968-7     |
|  50883 |          8 | 2117-11-22 04:30:00 | 0.2   |      0.2 | mg/dL    |      | Bilirubin, Direct | Blood | Chemistry | 1968-7     |
|  50883 |          8 | 2117-11-24 01:00:00 | 0.3   |      0.3 | mg/dL    |      | Bilirubin, Direct | Blood | Chemistry | 1968-7     |
|  50883 |          8 | 2117-11-24 11:00:00 | 0.2   |      0.2 | mg/dL    |      | Bilirubin, Direct | Blood | Chemistry | 1968-7     |
|  50883 |         10 | 2103-06-29 11:15:00 | 0.2   |      0.2 | mg/dL    |      | Bilirubin, Direct | Blood | Chemistry | 1968-7     |

## Normal or Abnormal Test Results Identificaiton

Use reference range information `refLOINC` provided by LONIC to identify
normal and abnormal test results. The `Patients` table is used to
provide gender information because reference ranges are different across
gender.

``` r
if(!"LOINC_CODE" %in% colnames(refLOINC)){
  refLOINC<-rename(refLOINC,LOINC_CODE=LOINC)
}
PDALabLONIC_ab <- getAbnormalMark(labData = PDALabLONIC,
                                     idColName = SUBJECT_ID,
                                     labItemColName = LOINC_CODE,
                                     valueColName = VALUENUM,
                                     genderColName = GENDER,
                                     genderTable = Patients,
                                     referenceTable = refLOINC)
```

``` r
head(PDALabLONIC_ab)
```

| ITEMID |  ID | CHARTTIME           | VALUE      | Value | VALUEUOM | FLAG | LABEL         | FLUID | CATEGORY   | LOINC_CODE | ABMark |
|-------:|----:|:--------------------|:-----------|------:|:---------|:-----|:--------------|:------|:-----------|:-----------|:-------|
|  51268 |   2 | 2138-07-17 21:10:00 | OCCASIONAL |    NA |          |      | Polychromasia | Blood | Hematology | 10378-8    | NA     |
|  51268 |   7 | 2121-05-25 02:30:00 | 1+         |    NA |          |      | Polychromasia | Blood | Hematology | 10378-8    | NA     |
|  51268 |   8 | 2117-11-20 14:00:00 | 1+         |    NA |          |      | Polychromasia | Blood | Hematology | 10378-8    | NA     |
|  51268 |  10 | 2103-06-28 11:10:00 | 2+         |    NA |          |      | Polychromasia | Blood | Hematology | 10378-8    | NA     |
|  51268 |  16 | 2178-02-03 08:15:00 | 2+         |    NA |          |      | Polychromasia | Blood | Hematology | 10378-8    | NA     |
|  51268 |  27 | 2191-11-30 23:00:00 | 1+         |    NA |          |      | Polychromasia | Blood | Hematology | 10378-8    | NA     |

# Time Series Analysis

## Deciding Width of Windows for Slicing Data

Then we need to decide a proper width of window for slicing laboratory
records into time-series window. `plotWindowProportion` helps users
explore the porpotion of missing values in each slicing window.

``` r
windowProportion <- plotWindowProportion(labData = PDALabLONIC,
                     idColName = SUBJECT_ID,
                     labItemColName = LOINC_CODE,
                     dateColName = CHARTTIME,
                     indexDate = first,
                     gapDate = c(1, 3, 7, 14, 30),
                     topN = 5)

print(windowProportion$graph)
```

![](UseCase_files/figure-gfm/window-1.png)<!-- -->

The figure shows that using 1 or 3 days window may generate large amount
of missing records and it could affect the analysis results,.

``` r
head(windowProportion$missingData)
```

| LAB     | Gap | Method     | Proportion |
|:--------|:----|:-----------|-----------:|
| 10378-8 | 1   | Missing ID |  0.3405377 |
| 10378-8 | 3   | Missing ID |  0.3172114 |
| 10378-8 | 7   | Missing ID |  0.2965208 |
| 10378-8 | 14  | Missing ID |  0.2702952 |
| 10378-8 | 30  | Missing ID |  0.2511861 |
| 1968-7  | 1   | Missing ID |  0.5183184 |

## Slice the Data into Time-series Window

Based on the above figure, we choose 7-day window in this analysis.

``` r
timeSeriesData <- getTimeSeriesLab(labData = PDALabLONIC,
                                   idColName = SUBJECT_ID,
                                   labItemColName = LOINC_CODE + LABEL,
                                   dateColName = CHARTTIME,
                                   valueColName = VALUENUM,
                                   indexDate = first,
                                   gapDate = 7,
                                   completeWindows = TRUE)
```

``` r
head(timeSeriesData)
```

|  ID | LOINC_CODE | LABEL               | Window | Count | Max | Min | Mean | Nearest | firstRecord | lastRecode |
|----:|:-----------|:--------------------|-------:|------:|----:|----:|-----:|--------:|:------------|:-----------|
|   2 | 10378-8    | Polychromasia       |      1 |     1 |  NA |  NA |   NA |      NA | 2138-07-17  | 2138-07-17 |
|   2 | 1968-7     | Bilirubin, Direct   |      1 |     1 | 0.3 | 0.3 |  0.3 |     0.3 | 2138-07-20  | 2138-07-20 |
|   2 | 1971-1     | Bilirubin, Indirect |      1 |     1 | 9.0 | 9.0 |  9.0 |     9.0 | 2138-07-20  | 2138-07-20 |
|   2 | 1975-2     | Bilirubin, Total    |      1 |     1 | 9.3 | 9.3 |  9.3 |     9.3 | 2138-07-20  | 2138-07-20 |
|   2 | 26498-6    | Myelocytes          |      1 |     2 | 0.0 | 0.0 |  0.0 |     0.0 | 2138-07-17  | 2138-07-17 |
|   2 | 28541-1    | Metamyelocytes      |      1 |     2 | 0.0 | 0.0 |  0.0 |     0.0 | 2138-07-17  | 2138-07-17 |

## Time-series Window Visualization

For some individuals which need further data explorision, users can
visulize the time seriers data. We randomly choose 5 individuals in this
use case, and found that

``` r
timeSeriesDataInd<-timeSeriesData %>% 
  filter(ID %in% c(87,92,93,126,159))
timeSeriesPlot <- plotTimeSeriesLab(labData = timeSeriesDataInd,
                                    idColName = ID,
                                    labItemColName = LOINC_CODE + LABEL,
                                    timeMarkColName = Window,
                                    valueColName = Nearest,
                                    timeStart = 1,
                                    timeEnd  = 5,
                                    abnormalMarkColName = NULL)

plot(timeSeriesPlot)
```

![](UseCase_files/figure-gfm/time-series%20plot-1.png)<!-- -->

## Analysis Ready Data Generation

``` r
WideTimeSeriesData <- wideTimeSeriesLab(labData = timeSeriesData,
                                        idColName = ID,
                                        labItemColName = LOINC_CODE+ LABEL,
                                        windowColName = Window,
                                        valueColName = Nearest)
```

``` r
head(WideTimeSeriesData)
```

|  ID | Window | 10378-8_Polychromasia | 1968-7_Bilirubin, Direct | 1971-1_Bilirubin, Indirect | 1975-2_Bilirubin, Total | 26498-6_Myelocytes | 28541-1_Metamyelocytes | 4544-3_Hematocrit | 702-1_Anisocytosis | 704-7_Basophils | 711-2_Eosinophils | 718-7_Hemoglobin | 728-6_Hypochromia | 731-0_Lymphocytes | 733-6_Atypical Lymphocytes | 738-5_Macrocytes | 741-9_Microcytes | 742-7_Monocytes | 761-7_Neutrophils | 763-3_Bands | 772-4_Nucleated Red Cells | 777-3_Platelet Count | 779-9_Poikilocytosis | 785-6_MCH | 786-4_MCHC | 787-2_MCV | 788-0_RDW | 789-8_Red Blood Cells | 804-5_White Blood Cells |
|----:|-------:|----------------------:|-------------------------:|---------------------------:|------------------------:|-------------------:|-----------------------:|------------------:|-------------------:|----------------:|------------------:|-----------------:|------------------:|------------------:|---------------------------:|-----------------:|-----------------:|----------------:|------------------:|------------:|--------------------------:|---------------------:|---------------------:|----------:|-----------:|----------:|----------:|----------------------:|------------------------:|
|   2 |      1 |                    NA |                      0.3 |                        9.0 |                     9.3 |                  0 |                      0 |               0.0 |                 NA |               0 |                 0 |              0.0 |                NA |                 0 |                          0 |               NA |               NA |               0 |               100 |           0 |                         1 |                    5 |                   NA |       0.0 |        0.0 |         0 |       0.0 |                  0.00 |                     0.1 |
|   5 |      1 |                    NA |                       NA |                         NA |                      NA |                  0 |                      0 |              43.0 |                 NA |               0 |                 0 |             14.9 |                NA |                16 |                          0 |               NA |               NA |               5 |                79 |           0 |                         3 |                  309 |                   NA |      37.6 |       34.7 |       109 |      16.7 |                  3.96 |                    13.9 |
|   7 |      1 |                    NA |                      0.3 |                        4.4 |                     4.7 |                  2 |                      0 |              53.0 |                 NA |               0 |                 6 |             17.6 |                NA |                24 |                          0 |               NA |               NA |               5 |                63 |           0 |                        NA |                  223 |                   NA |      35.2 |       33.2 |       106 |      18.2 |                  5.01 |                    22.8 |
|   8 |      1 |                    NA |                      0.2 |                        8.1 |                     8.3 |                  3 |                      0 |              52.0 |                 NA |               0 |                 6 |             17.5 |                NA |                24 |                          0 |               NA |               NA |               6 |                61 |           0 |                         1 |                  263 |                   NA |      34.8 |       33.6 |       104 |      17.1 |                  5.02 |                    18.7 |
|  10 |      1 |                    NA |                      0.2 |                        4.4 |                     4.6 |                  1 |                      0 |              42.8 |                 NA |               0 |                 3 |             14.8 |                NA |                58 |                          0 |               NA |               NA |              11 |                27 |           0 |                         2 |                  414 |                   NA |      35.8 |       34.5 |       104 |      16.3 |                  4.12 |                     9.2 |
|  10 |      2 |                    NA |                      0.3 |                        4.8 |                     5.1 |                 NA |                     NA |                NA |                 NA |              NA |                NA |               NA |                NA |                NA |                         NA |               NA |               NA |              NA |                NA |          NA |                        NA |                   NA |                   NA |        NA |         NA |        NA |        NA |                    NA |                      NA |

## Compare Laboratory Results between PDA and non-PDA groups

``` r
CaseLab<-
  inner_join(Case,WideTimeSeriesData,by="ID")
var<-colnames(CaseLab)[10:37]
var
```

    ##  [1] "10378-8_Polychromasia"      "1968-7_Bilirubin, Direct"  
    ##  [3] "1971-1_Bilirubin, Indirect" "1975-2_Bilirubin, Total"   
    ##  [5] "26498-6_Myelocytes"         "28541-1_Metamyelocytes"    
    ##  [7] "4544-3_Hematocrit"          "702-1_Anisocytosis"        
    ##  [9] "704-7_Basophils"            "711-2_Eosinophils"         
    ## [11] "718-7_Hemoglobin"           "728-6_Hypochromia"         
    ## [13] "731-0_Lymphocytes"          "733-6_Atypical Lymphocytes"
    ## [15] "738-5_Macrocytes"           "741-9_Microcytes"          
    ## [17] "742-7_Monocytes"            "761-7_Neutrophils"         
    ## [19] "763-3_Bands"                "772-4_Nucleated Red Cells" 
    ## [21] "777-3_Platelet Count"       "779-9_Poikilocytosis"      
    ## [23] "785-6_MCH"                  "786-4_MCHC"                
    ## [25] "787-2_MCV"                  "788-0_RDW"                 
    ## [27] "789-8_Red Blood Cells"      "804-5_White Blood Cells"

We can compare laboratory results in selected window (for example, 1)
between PDA and non-PDA groups. As the table shown, some results were
different between PDA and non-PDA groups.

``` r
t1<-tableone::CreateTableOne(data=CaseLab %>% filter(Window==1),
                         strata = c("selectedCase"),
                        var=var)
```

``` r
t1p<-print(t1)
```

``` r
knitr::kable(t1p)
```

|                                        | non-PDA        | PDA            | p       | test |
|:---------------------------------------|:---------------|:---------------|:--------|:-----|
| n                                      | 7209           | 379            |         |      |
| 1968-7_Bilirubin, Direct (mean (SD))   | 0.29 (0.12)    | 0.26 (0.10)    | \<0.001 |      |
| 1971-1_Bilirubin, Indirect (mean (SD)) | 7.11 (3.11)    | 4.57 (2.53)    | \<0.001 |      |
| 1975-2_Bilirubin, Total (mean (SD))    | 7.37 (3.12)    | 4.79 (2.50)    | \<0.001 |      |
| 26498-6_Myelocytes (mean (SD))         | 0.14 (0.57)    | 0.24 (1.03)    | 0.002   |      |
| 28541-1_Metamyelocytes (mean (SD))     | 0.24 (0.69)    | 0.32 (1.04)    | 0.043   |      |
| 4544-3_Hematocrit (mean (SD))          | 50.51 (6.48)   | 46.61 (6.48)   | \<0.001 |      |
| 704-7_Basophils (mean (SD))            | 0.25 (0.53)    | 0.24 (0.55)    | 0.665   |      |
| 711-2_Eosinophils (mean (SD))          | 2.19 (2.21)    | 1.83 (2.04)    | 0.003   |      |
| 718-7_Hemoglobin (mean (SD))           | 16.98 (2.17)   | 15.53 (2.22)   | \<0.001 |      |
| 731-0_Lymphocytes (mean (SD))          | 38.34 (19.24)  | 54.82 (20.24)  | \<0.001 |      |
| 733-6_Atypical Lymphocytes (mean (SD)) | 0.96 (2.17)    | 1.39 (2.44)    | \<0.001 |      |
| 742-7_Monocytes (mean (SD))            | 6.98 (3.63)    | 7.62 (4.49)    | 0.001   |      |
| 761-7_Neutrophils (mean (SD))          | 48.81 (19.18)  | 31.92 (18.41)  | \<0.001 |      |
| 763-3_Bands (mean (SD))                | 2.08 (3.47)    | 1.66 (3.51)    | 0.024   |      |
| 772-4_Nucleated Red Cells (mean (SD))  | 14.07 (40.54)  | 47.10 (116.26) | \<0.001 |      |
| 777-3_Platelet Count (mean (SD))       | 285.71 (81.36) | 236.04 (82.42) | \<0.001 |      |
| 785-6_MCH (mean (SD))                  | 36.16 (2.19)   | 38.04 (2.59)   | \<0.001 |      |
| 786-4_MCHC (mean (SD))                 | 33.64 (1.10)   | 33.31 (1.01)   | \<0.001 |      |
| 787-2_MCV (mean (SD))                  | 107.58 (6.84)  | 114.31 (8.20)  | \<0.001 |      |
| 788-0_RDW (mean (SD))                  | 16.95 (1.27)   | 17.06 (1.50)   | 0.124   |      |
| 789-8_Red Blood Cells (mean (SD))      | 4.71 (0.65)    | 4.09 (0.61)    | \<0.001 |      |
| 804-5_White Blood Cells (mean (SD))    | 15.36 (6.31)   | 10.07 (7.07)   | \<0.001 |      |

## Missing Value Imputation

If the data is prepared for model development, some missing values are
not allowed in some machine learning algorithms. Users can impute the
missing values with NOCB strategy (next observation carried backward)
with `imputeTimeSeriesLab`.

``` r
fullTimeSeriesData <- imputeTimeSeriesLab(labData = timeSeriesData,
                                   idColName = ID,
                                   labItemColName = LOINC_CODE + LABEL,
                                   windowColName = Window,
                                   valueColName = Mean & Nearest,
                                   impMethod = NOCB)
```

``` r
head(fullTimeSeriesData)
```

|  ID | LOINC_CODE | LABEL               | Window | Mean | Nearest |
|----:|:-----------|:--------------------|-------:|-----:|--------:|
|   2 | 10378-8    | Polychromasia       |      1 |   NA |      NA |
|   2 | 1968-7     | Bilirubin, Direct   |      1 |  0.3 |     0.3 |
|   2 | 1971-1     | Bilirubin, Indirect |      1 |  9.0 |     9.0 |
|   2 | 1975-2     | Bilirubin, Total    |      1 |  9.3 |     9.3 |
|   2 | 26498-6    | Myelocytes          |      1 |  0.0 |     0.0 |
|   2 | 28541-1    | Metamyelocytes      |      1 |  0.0 |     0.0 |

After imputation, we can convert the records into wide format to
generate the analysis read data.

``` r
FullWideTimeSeriesData <- wideTimeSeriesLab(labData = fullTimeSeriesData,
                                        idColName = ID,
                                        labItemColName = LOINC_CODE+ LABEL,
                                        windowColName = Window,
                                        valueColName = Nearest)
```

``` r
head(FullWideTimeSeriesData)
```

|  ID | Window | 10378-8_Polychromasia | 1968-7_Bilirubin, Direct | 1971-1_Bilirubin, Indirect | 1975-2_Bilirubin, Total | 26498-6_Myelocytes | 28541-1_Metamyelocytes | 4544-3_Hematocrit | 702-1_Anisocytosis | 704-7_Basophils | 711-2_Eosinophils | 718-7_Hemoglobin | 728-6_Hypochromia | 731-0_Lymphocytes | 733-6_Atypical Lymphocytes | 738-5_Macrocytes | 741-9_Microcytes | 742-7_Monocytes | 761-7_Neutrophils | 763-3_Bands | 772-4_Nucleated Red Cells | 777-3_Platelet Count | 779-9_Poikilocytosis | 785-6_MCH | 786-4_MCHC | 787-2_MCV | 788-0_RDW | 789-8_Red Blood Cells | 804-5_White Blood Cells |
|----:|-------:|----------------------:|-------------------------:|---------------------------:|------------------------:|-------------------:|-----------------------:|------------------:|-------------------:|----------------:|------------------:|-----------------:|------------------:|------------------:|---------------------------:|-----------------:|-----------------:|----------------:|------------------:|------------:|--------------------------:|---------------------:|---------------------:|----------:|-----------:|----------:|----------:|----------------------:|------------------------:|
|   2 |      1 |                    NA |                      0.3 |                        9.0 |                     9.3 |                  0 |                      0 |               0.0 |                 NA |               0 |                 0 |              0.0 |                NA |                 0 |                          0 |               NA |               NA |               0 |               100 |           0 |                         1 |                    5 |                   NA |       0.0 |        0.0 |         0 |       0.0 |                  0.00 |                     0.1 |
|   5 |      1 |                    NA |                       NA |                         NA |                      NA |                  0 |                      0 |              43.0 |                 NA |               0 |                 0 |             14.9 |                NA |                16 |                          0 |               NA |               NA |               5 |                79 |           0 |                         3 |                  309 |                   NA |      37.6 |       34.7 |       109 |      16.7 |                  3.96 |                    13.9 |
|   7 |      1 |                    NA |                      0.3 |                        4.4 |                     4.7 |                  2 |                      0 |              53.0 |                 NA |               0 |                 6 |             17.6 |                NA |                24 |                          0 |               NA |               NA |               5 |                63 |           0 |                        NA |                  223 |                   NA |      35.2 |       33.2 |       106 |      18.2 |                  5.01 |                    22.8 |
|   8 |      1 |                    NA |                      0.2 |                        8.1 |                     8.3 |                  3 |                      0 |              52.0 |                 NA |               0 |                 6 |             17.5 |                NA |                24 |                          0 |               NA |               NA |               6 |                61 |           0 |                         1 |                  263 |                   NA |      34.8 |       33.6 |       104 |      17.1 |                  5.02 |                    18.7 |
|  10 |      1 |                    NA |                      0.2 |                        4.4 |                     4.6 |                  1 |                      0 |              42.8 |                 NA |               0 |                 3 |             14.8 |                NA |                58 |                          0 |               NA |               NA |              11 |                27 |           0 |                         2 |                  414 |                   NA |      35.8 |       34.5 |       104 |      16.3 |                  4.12 |                     9.2 |
|  10 |      2 |                    NA |                      0.3 |                        4.8 |                     5.1 |                 NA |                     NA |                NA |                 NA |              NA |                NA |               NA |                NA |                NA |                         NA |               NA |               NA |              NA |                NA |          NA |                        NA |                   NA |                   NA |        NA |         NA |        NA |        NA |                    NA |                      NA |

## Deep Learning Model Development with Analysis Ready Data

``` r
#FullWideTimeSeriesData 
```
