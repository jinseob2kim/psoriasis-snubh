#일단 병력있는 사람을 제외한 사람들의 건선진단이력들만 필터링 후 imsi로 저장
imsi<-t20s[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40")][PERSON_ID%in%data.st_disease_filtered$PERSON_ID]
imsi[,RECU_FR_DT:=ymd(RECU_FR_DT)]

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
# table(imsi5$severe_index)
# imsi5 %>% filter(severe_index==1) %>% arrange(n)
imsi5[severe_index==1,.(PERSON_ID,severe_index)]






















#해야할일
# covariable - drug, 비당고흡 -------------------------------------------------


drug_key_seq<-sub_t60[sub_t60$GNL_NM_CD %in%st_bb,]$KEY_SEQ
# 이 처방전 교부한 내용을 환자별로 묶어서 봐야됨.
# procedure NB-UVB가 어딨는지 모르겠음.
dummy<-filtered_t20

cov_bdgh_before_main<-grep("E10|E11|E12|E13|E14|I10|E78",dummy$MAIN_SICK)
cov_bdgh_before_sub<-grep("E10|E11|E12|E13|E14|I10|E78",dummy$SUB_SICK)
cov_bdgh_before_sub_t20<-dummy[union(cov_bdgh_before_main,cov_bdgh_before_sub)] # 주/부 상병코드 중 하나라도 심/뇌혈관질환자
#rm(dummy)
length(unique(cov_bdgh_before_sub_t20$PERSON_ID))

filtered_index_date<-result[result$PERSON_ID %in% unique(before_sub_t20$PERSON_ID),]


