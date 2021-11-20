# ## Only L40
# sub_t20 <- merge(t20, t20[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40"), .SD[1], keyby = "PERSON_ID"][, .(PERSON_ID)], by = "PERSON_ID")
# sub_t30 <- merge(read_fst(file.path(path.KNHIS, "t1_130.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
# sub_t40 <- merge(read_fst(file.path(path.KNHIS, "t1_140.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
# sub_t60 <- merge(read_fst(file.path(path.KNHIS, "t1_160.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")

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

length(unique(jk$PERSON_ID)) ## N

## 2006~, >=40

## 2 time L40 in 1yr, after 2006
#data.st <- t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][order(RECU_FR_DT), .(diff = diff(ymd(RECU_FR_DT)), Indexdate = RECU_FR_DT[-.N]), keyby = "PERSON_ID"][diff <= 365 & Indexdate >= 20060101, .SD[1], keyby = "PERSON_ID"]

########################################################################
#[diff <= 365 & Indexdate >= 20060101, .SD[1], keyby = "PERSON_ID"]#####
#####만약 첫진단을 받고, 1년이 지난 다음 진단을 다시 받았고 그 이후로 자주 진단을 받는 사람은 제외됨. 이를 아래와 같이수정시 9975명
data.st<-t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][order(RECU_FR_DT), .(diff = diff(ymd(RECU_FR_DT)), Indexdate =ymd(RECU_FR_DT[-.N])), keyby = "PERSON_ID"][min(which(diff<= 365))  & Indexdate >= ymd(20060101), .SD[1], keyby = "PERSON_ID"]


age_upper_40<-unique(jk[STND_Y>=2006&AGE_GROUP>=9][PERSON_ID%in%unique(data.st$PERSON_ID)]$PERSON_ID)
data.st_age_filtered<- data.st[PERSON_ID%in%age_upper_40]
#사망한 사람들은 그 달 1일에 사망한걸로 간주
death_date<-jk[PERSON_ID%in%age_upper_40][!is.na(DTH_YM)][,.(PERSON_ID,DTH_YM=ym(DTH_YM))]
data.st_age_filtered<-death_date[data.st_age_filtered,on="PERSON_ID"]# left_join


#뇌/심혈관 이력
exclusion_criteria<-paste0(c(as.vector(unlist(code.dx[1:2]))),collapse = "|")
#추가 제외기준 이력
additional_exclusion_criteria<-paste0(c(as.vector(unlist(code.dx[3:7]))),collapse = "|")


before_disease<-t20s[PERSON_ID%in%age_upper_40][like(MAIN_SICK, exclusion_criteria) | like(SUB_SICK, exclusion_criteria)][order(RECU_FR_DT),.SD[1],keyby = "PERSON_ID"][,.(PERSON_ID,Diseasedate=ymd(RECU_FR_DT))]
excluded_patients<-before_disease[data.st_age_filtered,on="PERSON_ID"][Indexdate>=Diseasedate][,.(PERSON_ID,Diseasedate,Indexdate)]

additional_before_disease<-t20s[PERSON_ID%in%age_upper_40][like(MAIN_SICK, additional_exclusion_criteria) | like(SUB_SICK, additional_exclusion_criteria)][order(RECU_FR_DT),.SD[1],keyby = "PERSON_ID"][,.(PERSON_ID,Diseasedate=ymd(RECU_FR_DT))]
additional_excluded_patients<-additional_before_disease[data.st_age_filtered,on="PERSON_ID"][Indexdate>=Diseasedate][,.(PERSON_ID,Diseasedate,Indexdate)]

nrow(excluded_patients)  #이 사람들은 병력을 남겨야 함.
nrow(additional_excluded_patients) #이 사람들은 그냥 제외


data.st_age_additional_excluded<-data.st_age_filtered[!(PERSON_ID%in%additional_excluded_patients$PERSON_ID)]

# 1. (건선진단 이후 질병 발생 여부),2. (생존기간 변수) 추가 및 뇌/심혈관 질환이력 있는사람 제거

data.st_disease_filtered <- before_disease[data.st_age_additional_excluded,on="PERSON_ID"][,.(PERSON_ID,Indexdate,disease_index=ifelse(is.na(Diseasedate),0,1),survival_period=ifelse(is.na(DTH_YM)&is.na(Diseasedate),ymd(20131231)-Indexdate, ifelse(is.na(Diseasedate),DTH_YM-Indexdate,Diseasedate-Indexdate)))][survival_period>=0]
hist(data.st_disease_filtered$survival_period)


# 치료기준 만족 -----------------------------------------------------------------
# 관찰기간 1년 observe <-365
# Index 후 1년 내 6개월이상 연속치료 받은적이 있는 사람. duration <- 180
# 30일 이내 치료기간은 연속으로 인정 continuous<-30
# => 진단일 기록을 오름차순으로 정렬뒤 진단일 끼리 1차 차분한것에 -30.

#일단 병력있는 사람을 제외한 사람들의 건선진단이력들만 필터링 후 imsi로 저장
imsi<-t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][PERSON_ID%in%data.st_disease_filtered$PERSON_ID][,RECU_FR_DT:=ymd(RECU_FR_DT)]

#함수 정의
make_severe_index<-function(data,observe_date,duration_date,continuous_date){
  imsi2<-data.st_disease_filtered[imsi,on="PERSON_ID"][order(RECU_FR_DT), .(Indexdate,RECU_FR_DT,diff_from_index=ymd(RECU_FR_DT)-Indexdate), keyby = "PERSON_ID"][diff_from_index<=observe_date&diff_from_index>=0]
  imsi3<-imsi2[,.SD][order(RECU_FR_DT), .(diff_days = diff(RECU_FR_DT)), keyby = "PERSON_ID"]
  imsi4<-imsi3[,.(diff_days=as.numeric(diff_days),cumulsum=cumsum(as.numeric(diff_days))),keyby = "PERSON_ID"]
  group_nest_dt <- function(dt, ..., .key = "data"){
    stopifnot(is.data.table(dt))
    by <- substitute(list(...))
    dt <- dt[, list(list(.SD)), by = eval(by)]
    setnames(dt, old = "V1", new = .key)
    dt
  }
  imsi4<-group_nest_dt(imsi4,PERSON_ID)

  # 최소 5번이상 진료기록이 있어야 함 (일반식으로 계산)
  imsi5<-imsi4[unlist(lapply(imsi4$data,nrow))>=ceiling((duration_date-continuous_date)/continuous_date)]
  imsi5$data[1000][[1]]

  imsi5$data[1000][[1]]$diff_days

  imsi5$severe_index<-0  #중증건선 여부 저장하는 변수. 1이면 조건 만족, 0이면 조건 미달
  imsi5$max_cumsum<-0  # 최대연속치료기간
  for(i in seq_along(imsi5$PERSON_ID)){
    dummy<-imsi5$data[i][[1]]$diff_days
    csum=0
    for(k in 1:length(dummy)){
      if(dummy[k]<=continuous_date){ #다음진료까지의 시간이 continuous_date일 이하면,
        csum=csum+dummy[k]
        if(csum>imsi5$max_cumsum[i]){
          imsi5$max_cumsum[i]<-csum
        }else{csum<-csum}
      }else{csum=0}# continuous_date일을 초과하면 그날부로 다시 셈.
    }
    ifelse(imsi5$max_cumsum[i]>=(duration_date-continuous_date),imsi5$severe_index[i]<-1,imsi5$severe_index[i]<-0)
    # 더해서 150일이 누적되어있는상태라면, 이후 30일까지 연속으로 보므로 6개월연속 치료조건 만족
  }
  imsi<-imsi5[severe_index==1,.(PERSON_ID,severe_index)]
  return(imsi)
}

observe_date <-365     #관찰기간
duration_date <- 180   #연속치료기간
continuous_date <- 30  #연속 인정 일수

severe<-make_severe_index(imsi,observe_date,duration_date,continuous_date) # 이떄 imsi 는 질병이력으로 필터링한 환자들의 건선 진단 이력들

final_result<-severe[data.st_disease_filtered, on="PERSON_ID"]
final_result[,severe_index:=ifelse(is.na(severe_index),0,1)]
final_result



## N attr
attr <- list(
  "L40" = t20s[, length(unique(PERSON_ID))],
  "2 time L40 in 1yr, after 2006" = nrow(data.st),
  " %>%  age>=40" =nrow(data.st_age_filtered),
  " %>% disease filtered"=nrow(data.st_disease_filtered),
  "severe_index"=nrow(severe)
)
attr
