
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


