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


## AGE >= 40, SEX, Death: last date in month
data.40 <- data.st[, `:=`(STND_Y = as.integer(substr(Indexdate, 1, 4)), Indexdate = ymd(Indexdate))] %>%
  merge(jk[, .(STND_Y,SEX, AGE_GROUP, PERSON_ID)], by = c("STND_Y", "PERSON_ID")) %>%
  merge(jk[!is.na(DTH_YM), .(PERSON_ID, deathdate = ym(DTH_YM) - 1, DTH_CODE1, DTH_CODE2)], by = "PERSON_ID", all.x = T) %>%
  .[AGE_GROUP >= 9]


## Dx code
code.dx <- list(
  Psoriasis = "L40",
  CVD = c("I24","I21"),
  Stroke = paste0("I", 60:63),
  COPD = "J44",
  CKD = "N18",
  LC = c("K74", "K703"),
  HF = c("I50"),
  Cancer = c(paste0("C0",0:9), paste0("C", 10:97), paste0("D0", 0:9), "D45", "D46", "D471", "D473", "D474", "D475"),
  DM = paste0("E", 10:14),
  HTN = "I10",
  Dysplipidemia = "E78"
)

## Previous disease
range.prev_dx <- c(-99999, -1)  ## Range to search

prev_dx <- mclapply(code.dx, function(v){
  merge(t20s[, .(PERSON_ID, KEY_SEQ)], t40s[like(SICK_SYM, paste(v, collapse = "|")), .(dxdate = RECU_FR_DT[1]), keyby = "KEY_SEQ"], by = "KEY_SEQ")[, dxdate := ymd(dxdate)] %>%
    merge(data.40[, .(PERSON_ID, Indexdate)], by = "PERSON_ID", all.y = T) %>%
    .[, .(dx = as.integer(any(dxdate >= Indexdate + range.prev_dx[1] & dxdate <= Indexdate + range.prev_dx[2]))), keyby = "PERSON_ID"] %>%
    .[, .(dx = ifelse(is.na(dx), 0, dx))] %>% .$dx

}, mc.cores = length(code.dx)) %>% do.call(cbind, .)
colnames(prev_dx) <- paste0("prev_", colnames(prev_dx))

## Exclude prev CVD, Stroke, others
data.ex <- cbind(data.40, prev_dx)[!apply(prev_dx[, -1], 1, any)]



## N attr
attr <- list(
  "L40" = t20s[, length(unique(PERSON_ID))],
  "2 time L40 in 1yr, after 2006" = nrow(data.st),
  "AGE ≥40" = nrow(data.40),
  "Exclude prev CVD/Stroke/Others" = nrow(data.ex)
)









