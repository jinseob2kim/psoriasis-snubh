# ## Only L40
# sub_t20 <- merge(t20, t20[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40"), .SD[1], keyby = "PERSON_ID"][, .(PERSON_ID)], by = "PERSON_ID")
# sub_t30 <- merge(read_fst(file.path(path.KNHIS, "t1_130.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
# sub_t40 <- merge(read_fst(file.path(path.KNHIS, "t1_140.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
# sub_t60 <- merge(read_fst(file.path(path.KNHIS, "t1_160.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
library(fst);library(data.table);library(magrittr);library(lubridate)
# ## Save
# for (v in c(20, 30, 40, 60)){
#   code.save <- paste0("fst::write_fst(sub_t", v, ", 't", v, "s.fst')")
#     eval(parse(text=code.save))
# }

# read.data ---------------------------------------------------------------
path.KNHIS <- "/home/js/KNHIS"

t20s <- read_fst("t20s.fst", as.data.table = T);setkey(t20s, "KEY_SEQ")
t30s <- read_fst("t30s.fst", as.data.table = T);setkey(t30s, "KEY_SEQ")
t40s <- read_fst("t40s.fst", as.data.table = T);setkey(t40s, "KEY_SEQ")
t60s <- read_fst("t60s.fst", as.data.table = T);setkey(t60s, "KEY_SEQ")
jk <- read_fst(file.path(path.KNHIS, "jk.fst"), as.data.table = T)

## 2006~, >=40

## 2 time L40 in 1yr, after 2006
#data.st <- t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][order(RECU_FR_DT), .(diff = diff(ymd(RECU_FR_DT)), Indexdate = RECU_FR_DT[-.N]), keyby = "PERSON_ID"][diff <= 365 & Indexdate >= 20060101, .SD[1], keyby = "PERSON_ID"]

########################################################################
#[diff <= 365 & Indexdate >= 20060101, .SD[1], keyby = "PERSON_ID"]#####
#####만약 첫진단을 받고, 1년이 지난 다음 진단을 다시 받았고 그 이후로 자주 진단을 받는 사람은 제외됨. 이를 아래와 같이수정시 9975명
# data.st<-t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][order(RECU_FR_DT), .(diff = diff(ymd(RECU_FR_DT)), Indexdate =ymd(RECU_FR_DT[-.N])), keyby = "PERSON_ID"][min(which(diff<= 365))  & Indexdate >= ymd(20060101), .SD[1], keyby = "PERSON_ID"]
# data.st[,Indexdate:=ymd(Indexdate)]
# saveRDS(data.st,file = "data.st.RDS")
data.st<-readRDS("data.st.RDS")

age_upper_40<- data.st[, `:=`(STND_Y = as.integer(substr(Indexdate, 1, 4)), Indexdate = ymd(Indexdate))] %>%
  merge(jk[, .(STND_Y,SEX, AGE_GROUP, PERSON_ID)], by = c("STND_Y", "PERSON_ID")) %>%
  merge(jk[!is.na(DTH_YM), .(PERSON_ID, deathdate = ym(DTH_YM) - 1, DTH_CODE1, DTH_CODE2)], by = "PERSON_ID", all.x = T) %>%
  .[AGE_GROUP >= 9]
data.st_age_filtered<- data.st[PERSON_ID%in%age_upper_40$PERSON_ID]
#사망한 사람들은 그 전달 말일에 사망한걸로 간주=>이렇게 하는 이유가 있나요?
death_date<-jk[PERSON_ID%in%age_upper_40$PERSON_ID][!is.na(DTH_YM)][,.(PERSON_ID,DTH_YM=ym(DTH_YM)-1)]
data.st_age_filtered<-death_date[data.st_age_filtered,on="PERSON_ID"]# left_join


#뇌/심혈관 이력
exclusion_criteria<-paste0(c(as.vector(unlist(code.dx[1:2]))),collapse = "|")
#추가 제외기준 이력
additional_exclusion_criteria<-paste0(c(as.vector(unlist(code.dx[3:7]))),collapse = "|")


before_disease<-t20s[PERSON_ID%in%age_upper_40$PERSON_ID][like(MAIN_SICK, exclusion_criteria) | like(SUB_SICK, exclusion_criteria)][order(RECU_FR_DT),.SD[1],keyby = "PERSON_ID"][,.(PERSON_ID,Diseasedate=ymd(RECU_FR_DT))]
excluded_patients<-before_disease[data.st_age_filtered,on="PERSON_ID"][Indexdate>=Diseasedate][,.(PERSON_ID,Diseasedate,Indexdate)]

additional_before_disease<-t20s[PERSON_ID%in%age_upper_40$PERSON_ID][like(MAIN_SICK, additional_exclusion_criteria) | like(SUB_SICK, additional_exclusion_criteria)][order(RECU_FR_DT),.SD[1],keyby = "PERSON_ID"][,.(PERSON_ID,Diseasedate=ymd(RECU_FR_DT))]
additional_excluded_patients<-additional_before_disease[data.st_age_filtered,on="PERSON_ID"][Indexdate>=Diseasedate][,.(PERSON_ID,Diseasedate,Indexdate)]

nrow(excluded_patients)  #이 사람들은 병력을 남겨야 함.=> 후에 병 유발관련 연구진행을 위함
nrow(additional_excluded_patients) #이 사람들은 그냥 제외


data.st_age_additional_excluded<-data.st_age_filtered[!(PERSON_ID%in%additional_excluded_patients$PERSON_ID)]

# 1. (건선진단 이후 질병 발생 여부),2. (생존기간 변수) 추가 및 뇌/심혈관 질환이력 있는사람 제거

data.st_disease_filtered <- before_disease[data.st_age_additional_excluded,on="PERSON_ID"][,.(PERSON_ID,Indexdate,disease_index=ifelse(is.na(Diseasedate),0,1),survival_period=ifelse(is.na(DTH_YM)&is.na(Diseasedate),ymd(20131231)-Indexdate, ifelse(is.na(Diseasedate),DTH_YM-Indexdate,Diseasedate-Indexdate)))][survival_period>=0]
hist(data.st_disease_filtered$survival_period)

# 치료기준 만족 -----------------------------------------------------------------
# 관찰기간 1년 observe <-365
# Index 후 1년 내 6개월이상 연속치료 받은적이 있는 사람. duration <- 180
# 30일 이내 치료기간은 연속으로 인정 continuous<-30
#일단 병력있는 사람을 제외한 사람들의 건선진단이력들만 필터링 후 imsi로 저장
imsi<-t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][PERSON_ID%in%data.st_disease_filtered$PERSON_ID][,RECU_FR_DT:=ymd(RECU_FR_DT)]
# 약물, 광선치료 여부 변수 추가
treat_uvB<-t30s[KEY_SEQ%in%imsi$KEY_SEQ][like(DIV_CD, paste0(code.uvB,collapse = "|"))][,.(KEY_SEQ,RECU_FR_DT=ymd(RECU_FR_DT),MDCN_EXEC_FREQ)]
treat_uvB[,uvB:=1]
imsi<-treat_uvB[imsi,on=c("KEY_SEQ","RECU_FR_DT")]
treat_txdrug<-t60s[KEY_SEQ%in%imsi$KEY_SEQ][like(GNL_NM_CD,paste0(as.vector(unlist(code.txdrug)),collapse = "|"))][,.(KEY_SEQ,RECU_FR_DT=ymd(RECU_FR_DT),MDCN_EXEC_FREQ)]
treat_txdrug[,txdrug:=1]
imsi<-treat_txdrug[imsi,on=c("KEY_SEQ","RECU_FR_DT")]

# group_nest 함수 정의 (person_id 별로 진단기록 리스트화)--------------------------------------------------------
group_nest_dt <- function(dt, ..., .key = "data"){
  stopifnot(is.data.table(dt))
  by <- substitute(list(...))
  dt <- dt[, list(list(.SD)), by = eval(by)]
  setnames(dt, old = "V1", new = .key)
  dt
}
#drug 함수 정의
make_drug_index<-function(data,observe_date,duration_date,continuous_date){
  stopifnot(is.data.table(data))
  imsi2<-data.st_disease_filtered[data,on="PERSON_ID"][order(RECU_FR_DT), .(MDCN_EXEC_FREQ,txdrug,Indexdate,RECU_FR_DT,diff_from_index=ymd(RECU_FR_DT)-Indexdate), keyby = "PERSON_ID"][diff_from_index<=observe_date&diff_from_index>=0]
  imsi2<-imsi2[!is.na(txdrug)]
  imsi3<-imsi2[order(RECU_FR_DT), .(MDCN_EXEC_FREQ,diff_days = c(0,diff(RECU_FR_DT))), keyby = "PERSON_ID"]
  imsi4<-imsi3[,.(MDCN_EXEC_FREQ,diff_days=as.numeric(diff_days),cumulsum=cumsum(as.numeric(diff_days))),keyby = "PERSON_ID"]
  imsi4<-group_nest_dt(imsi4,PERSON_ID)
  # 최소 5번이상 진료기록이 있어야 함 (일반식으로 계산)
  imsi5<-imsi4[unlist(lapply(imsi4$data,nrow))>=ceiling((duration_date_drug-continuous_date)/continuous_date)]
  imsi5$drug_index<-0  #1이면 조건 만족, 0이면 조건 미달
  imsi5$max_cumsum<-0  # 최대연속치료기간

  for(i in 1:nrow(imsi5)){
    dummy<-as.numeric(imsi5$data[[i]]$diff_days)
    dummy2<-imsi5$data[[i]]$MDCN_EXEC_FREQ
    dummy3<-imsi5$data[[i]]$cumulsum
    csum=0
    for(k in 1:(length(dummy)-1)){
      if(dummy[k+1]-dummy2[k]<=continuous_date){ #다음진료까지의 시간- (이전 처방일수)가 continuous_date일 이하면,
        csum=csum+dummy3[k+1]-dummy3[k]
        if(csum>imsi5$max_cumsum[i]){
          imsi5$max_cumsum[i]<-csum
          if(imsi5$max_cumsum[i]>=(duration_date_drug-continuous_date)-dummy2[k]){ # 누적치료일수가 150일-(마지막날 처방일수) 이면, 이후 30일까지 연속으로 보므로 6개월연속 치료조건 만족
            imsi5$drug_index[i]<-1
            break
          }
        }
      }
      else{csum=0}# continuous_date일을 초과하면 그날부로 다시 셈.
    }


  }


  return(imsi5[drug_index==1,.(PERSON_ID,drug_index)]$PERSON_ID)
}

observe_date <-365     #관찰기간
duration_date_drug <- 180   #연속치료기간
continuous_date <- 30  #연속 인정 일수
#system.time(make_drug_index(imsi[!is.na(txdrug)],observe_date,duration_date_drug,continuous_date))
treat_txdrug_person<-make_drug_index(imsi[!is.na(txdrug)],observe_date,duration_date_drug,continuous_date) # 이떄 imsi 는 질병이력으로 필터링한 환자들의 건선 진단 이력들

# uvB 함수정의 ----------------------------------------------------------------
make_uvB_index<-function(data,observe_date,duration_date,treat_n){
  imsi2<-data.st_disease_filtered[imsi,on="PERSON_ID"][order(RECU_FR_DT), .(i.MDCN_EXEC_FREQ,uvB,Indexdate,RECU_FR_DT,diff_from_index=ymd(RECU_FR_DT)-Indexdate), keyby = "PERSON_ID"][diff_from_index<=observe_date&diff_from_index>=0]
  imsi2<-imsi2[!is.na(uvB)]
  imsi3<-group_nest_dt(imsi2,PERSON_ID)
  #imsi3<-imsi3[,.nroow:=unlist(lapply(imsi3$data,nrow))]
  imsi3$uvB_index<-0
  for(k in 1:nrow(imsi3)){
    ya<-imsi3$data[[k]]
    for(i in seq_along(nrow(ya))){
      sum_frec=0
      j=0
      while(as.numeric(ya$diff_from_index[i])+duration_date_uvB>=as.numeric(ya$diff_from_index[i+j])){
        sum_frec=sum_frec+ya$i.MDCN_EXEC_FREQ[i+j]
        if(sum_frec>=treat_n){
          imsi3$uvB_index[k]<-1
          break
        }
        j=j+1
        if(is.na(as.numeric(ya$diff_from_index[i+j]))){
          imsi3$uvB_index[k]<-0
          break
        }
      }
    }
  }
  return(imsi3[uvB_index==1]$PERSON_ID)

}

duration_date_uvB <- 90   #치료기간
treat_n <- 12 #치료횟수
treat_uvB_person<- make_uvB_index(imsi[!is.na(uvB)],observe_date,duration_date_uvB,treat_n) # 이떄 imsi 는 질병이력으로 필터링한 환자들의 건선 진단 이력들


mean(data.st_disease_filtered[disease_index==1][PERSON_ID%in%treat_txdrug_person]$survival_period )
mean(data.st_disease_filtered[disease_index==1][PERSON_ID%in%treat_uvB_person]$survival_period)
mean(data.st_disease_filtered[disease_index==1][!(PERSON_ID%in%union(treat_txdrug_person,treat_uvB_person))]$survival_period)


treat_txdrug_person
data.st_disease_filtered[,txdrug:=ifelse(PERSON_ID%in%treat_txdrug_person,1,0)]
data.st_disease_filtered[,uvB:=ifelse(PERSON_ID%in%treat_uvB_person,1,0)]
table(data.st_disease_filtered$uvB)
table(data.st_disease_filtered$txdrug)

## N attr
attr <- list(
  "L40" = t20s[, length(unique(PERSON_ID))],
  "2 time L40 in 1yr, after 2006" = nrow(data.st),
  " %>%  age>=40" =nrow(data.st_age_filtered),
  " %>% disease filtered"=nrow(data.st_disease_filtered)
)
attr
