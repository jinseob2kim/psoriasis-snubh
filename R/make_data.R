library(fst);library(data.table);library(magrittr);library(parallel);library(lubridate)
#setwd("~/ShinyApps/kbori87/psoriasis-snubh/")
path.KNHIS <- "/home/js/KNHIS"

t20s <- read_fst("t20s.fst", as.data.table = T);setkey(t20s, "KEY_SEQ")
t30s <- read_fst("t30s.fst", as.data.table = T);setkey(t30s, "KEY_SEQ")
t40s <- read_fst("t40s.fst", as.data.table = T);setkey(t40s, "KEY_SEQ")
t60s <- read_fst("t60s.fst", as.data.table = T);setkey(t60s, "KEY_SEQ")
jk <- read_fst(file.path(path.KNHIS, "jk.fst"), as.data.table = T)


## 2006~, >=40

## 2 time L40 in 1yr, after 2006
data.st <- t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][order(RECU_FR_DT), .(diff = diff(ymd(RECU_FR_DT)), Indexdate = RECU_FR_DT[-.N]), keyby = "PERSON_ID"][diff <= 365 & Indexdate >= 20060101, .SD[1], keyby = "PERSON_ID"]


## L50
t20s[like(MAIN_SICK, "L50") | like(SUB_SICK, "L50")][order(RECU_FR_DT), .(diff = diff(ymd(RECU_FR_DT)), Indexdate = RECU_FR_DT[-.N]), keyby = "PERSON_ID"][diff <= 365 , .SD[1], keyby = "PERSON_ID"]


## N attr
attr <- list(
  "L40" = t20s[, length(unique(PERSON_ID))],
  "2 time L40 in 1yr, after 2006" = nrow(data.st))





code.dx <- list(
  CVD = c("I24","I21"),
  Stroke = paste0("I", 60:63),
  COPD = "J34",
  CKD = "N18",
  LC = c("K4", "K703"),
  HF = c("I50"),
  Cancer = c(paste0("C0",0:9), paste0("C", 10:97), paste0("D0", 0:9), "D45", "D46", "D471", "D473", "D474", "D475"),
  DM = paste0("E", 11:14),
  HTN = "I10",
  Dysplipidemia = "E38"
)







