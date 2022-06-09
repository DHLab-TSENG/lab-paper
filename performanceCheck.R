library(dxpr)
library(tictoc)
library(dplyr)
load("DeliveryCCSDataLong.rda")
#devtools::install_github("DHLab-TSENG/dxpr")
#devtools::install_github("jabiru/tictoc")


DeliveryCCSDataLong<-
  dplyr::rename(DeliveryCCSDataLong,Date=DateServiceStarted,ID=FinalID)
nRuns<-100
AllTimeLog<-NULL

######################################################
# run time start
######################################################

tic.clearlog()
tagName<-"AHRQ grouping"
for (i in 1:nRuns){
  tic(tagName)
  AHRQData<-icdDxToComorbid(DeliveryCCSDataLong,
                            idColName = ID,
                            icdColName = ICD,
                            dateColName = Date,
                            icd10usingDate = "2015-10-1",
                            comorbidMethod = ahrq
  )
  toc(log = TRUE, quiet = TRUE)
  
}
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
TimeLog<-data.frame(tagName,timings,stringsAsFactors = F)
AllTimeLog<-bind_rows(AllTimeLog,TimeLog)


tic.clearlog()
tagName<-"Analysis ready data generation"
for (i in 1:nRuns){
  tic(tagName)
  
  groupedData_Wide <- groupedDataLongToWide(dxDataFile  = AHRQData$groupedDT,
                                            idColName = ID,
                                            categoryColName = Comorbidity,
                                            dateColName = Date,
                                            reDup = TRUE,
                                            numericOrBinary = B,
                                            count = 2)
  
  toc(log = TRUE, quiet = TRUE)
}
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
TimeLog<-data.frame(tagName,timings,stringsAsFactors = F)
AllTimeLog<-bind_rows(AllTimeLog,TimeLog)

rm(AHRQData)
rm(groupedData_Wide)

tic.clearlog()
tagName<-"Case selection"
for (i in 1:nRuns){
  tic(tagName)
  case <- selectCases(dxDataFile = DeliveryCCSDataLong,
                      idColName = ID,
                      icdColName = ICD,
                      dateColName = Date,
                      icd10usingDate = "2015-10-1",
                      groupDataType = ccslvl2,
                      caseCondition = "Diseases of the urinary system",
                      caseCount = 1,
                      caseName = "US")
  toc(log = TRUE, quiet = TRUE)
}
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
TimeLog<-data.frame(tagName,timings,stringsAsFactors = F)
AllTimeLog<-bind_rows(AllTimeLog,TimeLog)
rm(case)

tic.clearlog()

tagName<-"Eligible period identification"
for (i in 1:nRuns){
  tic(tagName)
  eligible <- getEligiblePeriod(dxDataFile = DeliveryCCSDataLong,
                                idColName = ID,
                                dateColName = Date)
  
  toc(log = TRUE, quiet = TRUE)
}
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
TimeLog<-data.frame(tagName,timings,stringsAsFactors = F)
AllTimeLog<-bind_rows(AllTimeLog,TimeLog)

tic.clearlog()



library(dplyr)
indexDateTable <- eligible %>% 
  select(ID,endRecordDate) %>% 
  rename(indexDate=endRecordDate)
rm(eligible)
tagName<-"Data split"
for (i in 1:nRuns){
  tic(tagName)
  split<-splitDataByDate(dxDataFile = DeliveryCCSDataLong,
                         idColName = ID,
                         icdColName = ICD,
                         dateColName = Date,
                         indexDateFile = indexDateTable,
                         gap = 30)
  toc(log = TRUE, quiet = TRUE)
}
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
TimeLog<-data.frame(tagName,timings,stringsAsFactors = F)
AllTimeLog<-bind_rows(AllTimeLog,TimeLog)






######################################################
# wait for debug
######################################################
tic.clearlog()
tagName<-"Condition era generation"
for (i in 1:nRuns){
  tic(tagName)
  Era <- getConditionEra(dxDataFile = DeliveryCCSDataLong,
                         idColName = ID,
                         icdColName = ICD,
                         dateColName = Date,
                         icd10usingDate = "2015/10/01",
                         groupDataType = CCS,
                         isDescription = FALSE,
                         gapDate = 30)
  toc(log = TRUE, quiet = TRUE)
}
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
TimeLog<-data.frame(tagName,timings,stringsAsFactors = F)
AllTimeLog<-bind_rows(AllTimeLog,TimeLog)

tic.clearlog()


######################################################
# analysis
# done 20201112
######################################################

AllTimeLog

saveRDS(AllTimeLog,paste0("AllTimeLog",Sys.Date(),"ConditionEra.rds"))
AllTimeLog %>% group_by(tagName) %>% 
  summarise(count=n(),meanT=mean(timings),sdT=sd(timings))


AllTimeLog<-
  readRDS("AllTimeLog2020-12-01.rds")
AllTimeLog %>% group_by(tagName) %>% 
  summarise(count=n(),meanT=mean(timings),sdT=sd(timings))


