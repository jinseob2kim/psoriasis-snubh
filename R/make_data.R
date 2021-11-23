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
  .[AGE_GROUP >= 5]


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

## Previous range
range.prev <- c(-99999, -1)  ## Range to search

prev_dx <- mclapply(code.dx, function(v){
  merge(t20s[, .(PERSON_ID, KEY_SEQ)], t40s[like(SICK_SYM, paste(v, collapse = "|")), .(dxdate = RECU_FR_DT[1]), keyby = "KEY_SEQ"], by = "KEY_SEQ")[, dxdate := ymd(dxdate)] %>%
    merge(data.40[, .(PERSON_ID, Indexdate)], by = "PERSON_ID", all.y = T) %>%
    .[, .(dx = as.integer(any(dxdate >= Indexdate + range.prev[1] & dxdate <= Indexdate + range.prev[2]))), keyby = "PERSON_ID"] %>%
    .[, .(dx = ifelse(is.na(dx), 0, dx))] %>% .$dx

}, mc.cores = length(code.dx)) %>% do.call(cbind, .)
colnames(prev_dx) <- paste0("prev_", colnames(prev_dx))


## Drug
#dname <- readxl::excel_sheets("Medication 재정리_2021-08-05.xlsx")
#code.drug <- lapply(dname, function(x){readxl::read_excel("Medication 재정리_2021-08-05.xlsx", sheet = x)[[1]]})
#names(code.drug) <- dname
#saveRDS(code.drug, "druginfo.RDS")
code.drug <- readRDS("druginfo.RDS")


prev_drug <- mclapply(code.drug[c("Statin", "Beta-Blocker")], function(v){
  merge(t20s[, .(PERSON_ID, KEY_SEQ)], t60s[GNL_NM_CD %in% v, .(drdate = RECU_FR_DT[1]), keyby = "KEY_SEQ"], by = "KEY_SEQ")[, drdate := ymd(drdate)] %>%
    merge(data.40[, .(PERSON_ID, Indexdate)], by = "PERSON_ID", all.y = T) %>%
    .[, .(dr = as.integer(any(drdate >= Indexdate + range.prev[1] & drdate <= Indexdate + range.prev[2]))), keyby = "PERSON_ID"] %>%
    .[, .(dr = ifelse(is.na(dr), 0, dr))] %>% .$dr
}) %>% do.call(cbind, .)
colnames(prev_drug) <- paste0("prev_", c("Statin", "Beta-Blocker"))


## Exclude prev CVD, Stroke, others
data.ex <- cbind(data.40, prev_dx, prev_drug)[!apply(prev_dx[, paste0("prev_", c("COPD", "CKD", "LC", "HF"))], 1, any)]

## N attr
attr <- list(
  "L40" = t20s[, length(unique(PERSON_ID))],
  "2 time L40 in 1yr, after 2006" = nrow(data.st),
  "AGE ≥20" = nrow(data.40),
  "Exclude prev CVD/Stroke/COPD/CKD/LC/HF" = nrow(data.ex)
)


## Treat
code.uvB <- c("MM331", "MM332", "MM333", "MM334")  ## t30
code.txdrug <- list(
  Cyclosporine = c("139201ACS", "139203ALQ", "139204ACS", "194701ACH", "194701ALQ", "194702ACH", "194701ACS",
                   "194702ACS", "194703ACS", "194730ALQ", "194731ALQ", "139202BIJ", "139230BIJ"),
  Methotrexate = c("192101ATB", "192107ATB", "192102BIJ", "192103BIJ", "192104BIJ", "192105BIJ", "192106BIJ",
                   "192107BIJ", "192108BIJ", "192109BIJ", "192110BIJ", "192111BIJ", "192132BIJ", "192134BIJ",
                   "192136BIJ", "192138BIJ", "192139BIJ", "192140BIJ", "192141BIJ", "192142BIJ", "192143BIJ", "192144BIJ"),
  Acitretin = c("102301ACH", "102302ACH")
)

code.biologics <- list(
  `TNF-α inhibitor` = c("455801BIJ", "455802BIJ", "455803BIJ", "455830BIJ", "455831BIJ", "383501BIJ", "383502BIJ",
                        "488401BIJ", "488430BIJ", "488431BIJ", "488433BIJ"),
  `Anti-IL-12/23p40` = c("615001BIJ", "615002BIJ", "615032BIJ", "615031BIJ", "615030BIJ"),
  `IL-17A antagonist` = c("644601BIJ", "644602BIJ", "667801BIJ"),
  `IL-23 antagonist` = c("670901BIJ", "686201BIJ")
)


t60.txdrug <- merge(t20s[, .(PERSON_ID, KEY_SEQ)],
                    t60s[GNL_NM_CD %in% unlist(code.txdrug)][order(RECU_FR_DT, MDCN_EXEC_FREQ), .SD[.N], keyby = "KEY_SEQ"], by = "KEY_SEQ") %>%
  .[, RECU_FR_DT := ymd(RECU_FR_DT)] %>% .[]

t30.uvb <- merge(t20s[, .(PERSON_ID, KEY_SEQ)],
                 t30s[DIV_CD %in% code.uvB][order(RECU_FR_DT, MDCN_EXEC_FREQ), .SD[.N], keyby = "KEY_SEQ"], by = "KEY_SEQ") %>%
  .[, RECU_FR_DT := ymd(RECU_FR_DT)] %>% .[]



## Function- duration: Drug duration, Gap: gap
dur_conti <- function(indi, duration = 90, gap = 30, washout = 365){

  data.ind <- t60.txdrug[PERSON_ID == indi, .(start = RECU_FR_DT, MDCN_EXEC_FREQ)][start >= data.ex[PERSON_ID == indi]$Indexdate]
  if (nrow(data.ind) == 0){
    return(data.table(PERSON_ID = NA, start = ymd(NA), dur_conti = NA))
  }

  ## Drug date list
  datelist <- lapply(1:nrow(data.ind), function(x){data.ind[x, seq(start, start + MDCN_EXEC_FREQ, by = 1)]}) %>%
    Reduce(c, .) %>% unique %>% sort
  df <- diff(datelist)
  ## Gap change
  df[df <= gap] <- 1

  ## New datelist
  datelist2 <- datelist[1] + c(0, cumsum(as.integer(df)))

  ## Conti duration
  res <- data.table(PERSON_ID = indi,
                    start = datelist,
                    dur_conti = sapply(seq_along(datelist2), function(v){
                      zz <- datelist2[v:length(datelist2)]
                      return(ifelse(any(diff(zz) > 1), which(diff(zz) > 1)[1] - 1, length(zz) - 1))
                    }))
  return(res[dur_conti >= duration & start <= data.ex[PERSON_ID == indi]$Indexdate + washout][1])
}

## Function for UBV
dur_conti_uvb <- function(indi, duration = 90, nprocedure = 12, washout = 365){
  data.ind <- t30.uvb[PERSON_ID == indi, .(start = RECU_FR_DT, MDCN_EXEC_FREQ)][start >= data.ex[PERSON_ID == indi]$Indexdate]
  if (nrow(data.ind) == 0){
    return(data.table(PERSON_ID = NA, start = ymd(NA), n_procedure = NA))
  }


  ## Drug date list
  datelist <- lapply(1:nrow(data.ind), function(x){data.ind[x, seq(start, start + MDCN_EXEC_FREQ, by = 1)]}) %>%
    Reduce(c, .) %>% unique %>% sort

  res <- data.table(PERSON_ID = indi,
                    start = datelist,
                    n_procedure = sapply(seq_along(datelist), function(x){
                      sum(datelist[x:length(datelist)] >= datelist[x] & datelist[x:length(datelist)] <= datelist[x] + duration)
                    }))

  return(res[n_procedure >= nprocedure & start <= data.ex[PERSON_ID == indi]$Indexdate + washout][1])
}


## Result
cc <- mclapply(intersect(data.ex$PERSON_ID, t60.txdrug$PERSON_ID), dur_conti, duration = 90) %>% rbindlist() %>% .[!is.na(PERSON_ID)]

cc1 <- mclapply(intersect(data.ex$PERSON_ID, t30.uvb$PERSON_ID), dur_conti_uvb, duration = 90, nprocedure = 12) %>% rbindlist() %>% .[!is.na(PERSON_ID)]





