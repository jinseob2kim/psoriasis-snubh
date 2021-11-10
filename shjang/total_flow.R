library(fst);library(parallel);library(data.table);library(magrittr)
library(dplyr);library(lubridate)
## PATH: file folder
path.KNHIS <- "/home/js/KNHIS"


## Read 20 30 40 60
t20 <- read_fst(file.path(path.KNHIS, "t1_120.fst"), as.data.table = T)
t30 <- read_fst(file.path(path.KNHIS, "t1_130.fst"), as.data.table = T)
t40 <- read_fst(file.path(path.KNHIS, "t1_140.fst"), as.data.table = T)
t60 <- read_fst(file.path(path.KNHIS, "t1_160.fst"), as.data.table = T)




## jk : SEX, AGE_group, Death, Death 원인 등
jk <- read_fst(file.path(path.KNHIS, "jk.fst"), as.data.table = T)


# subset뽑아놓기 --------------------------------------------------------------

main<-grep("L40",t20$MAIN_SICK)
sub<-grep("L40",t20$SUB_SICK)
sub_t20<-t20[union(main,sub)] # 주/부 상병코드 중 하나라도 L40의 형태를 포함하면 sub에 저장(AL40, ??L40?? 전부 포함)


sub_jk<-jk[jk$PERSON_ID %in% unique(sub_t20$PERSON_ID)] #건선한번이라도 진료받은사람 자격레이아웃 필터링
sub_t30 <- t30[t30$KEY_SEQ %in% unique(sub_t20$KEY_SEQ)]
sub_t40 <- t40[t40$KEY_SEQ %in% unique(sub_t20$KEY_SEQ)]
sub_t60 <- t60[t60$KEY_SEQ %in% unique(sub_t20$KEY_SEQ)]


write.csv(sub_jk,"sub_jk.csv",row.names = F)
write.csv(sub_t20,"sub_t20.csv",row.names = F)
write.csv(sub_t30,"sub_t30.csv",row.names = F)
write.csv(sub_t40,"sub_t40.csv",row.names = F)
write.csv(sub_t60,"sub_t60.csv",row.names = F)



# subset 읽고 분석 시작하기 -------------------------------------------------------

sub_jk<-read.csv("sub_jk.csv")
sub_t20<-read.csv("sub_t20.csv")
sub_t30<-read.csv("sub_t30.csv")
sub_t40<-read.csv("sub_t40.csv")
sub_t60<-read.csv("sub_t60.csv")




# *처리해야 할 일* ----------------------------------------------------------------



### 장기입원환자=>최대 입원기간이라는 변수 계산?or 총 입원일수?, cancer진단
### 건선진단일, 뇌/심혈관 최초진단일간의 날짜 계산
### 1)건선 치료받은 여부 및 2)어떤 치료받았는지 3개의 범주로 만들어 구분하기


# 지금까지 한 일 ----------------------------------------------------------------

###.건선 진단 유무로 subset 뽑아 csv저장

# _0. 나이필터링 ---------------------------------------------------------------

sub_jk_age_filtered<-sub_jk %>%  filter(AGE_GROUP>=9) %>% distinct(PERSON_ID) #40세이상
sub_t20<-sub_t20[sub_t20$PERSON_ID%in%sub_jk_age_filtered$PERSON_ID,]
sub_t30 <- sub_t30[sub_t30$KEY_SEQ %in% unique(sub_t20$KEY_SEQ),]
sub_t40 <- sub_t40[sub_t40$KEY_SEQ %in% unique(sub_t20$KEY_SEQ),]
sub_t60 <- sub_t60[sub_t60$KEY_SEQ %in% unique(sub_t20$KEY_SEQ),]




# _1. 1년의 기간안에 2회이상 건선으로 진료받은 사람을 분석군으로 필터링 --------------------------------


# 진료기록이 2번이상인 사람

at_least_2<-
  sub_t20 %>% group_by(PERSON_ID) %>% tally() %>% filter(n>1) %>% arrange(desc(n))


# 적어도 두번 기록있는사람들을 imsi에 개인일련번호로 그룹화하여 리스트로 저장
imsi<-sub_t20[sub_t20$PERSON_ID %in%at_least_2$PERSON_ID,] %>% nest_by(PERSON_ID)



imsi$index<-0 #주진단에 1년에 최소 L40 2회이상을 구분하는 index변수 생성하기
imsi$index_date<-as_date(0)#최초진단일을 저장할 변수
for(i in seq_along(imsi$PERSON_ID)){
  dummy1<-sort(as.numeric(ymd(imsi$data[[i]]$RECU_FR_DT))) #환자별 건선진단일을 오름차순 정렬
  dummy2<-diff(dummy1) #정렬된 벡터를 1차 차분하여 진단일간 날짜 차이 계산
  for(k in 1:length(dummy2)){
    if(dummy2[k]<=365){ #지정 기간내에 두번이상 건선진료받은 사람=1(여기서는 365일)
      imsi$index[i]<-1
      imsi$index_date[i]<-as_date(dummy1[k]) #진단일간 날짜차이가 365보다 작으면, 그중 빠른 날짜를 최초진단일로 정의
      break
    }else{
      imsi$index[i]<-0
    }
  }
}
table(imsi$index)

index<-imsi[imsi$index==1,] #원하는 분석군의 개인식별번호 추출
index[,c(1,4)]
#최종필터링 결과물 (개인식별번호, 최초 진단일)
result<-index[,c(1,4)]
result


# _2. 건선 진단일 이전 뇌/심혈관진단이력 있는 사람 필터링 ----------------------------------------

#t20 <- read_fst(file.path(path.KNHIS, "t1_120.fst"), as.data.table = T)
filtered_t20<-t20[t20$PERSON_ID %in% result$PERSON_ID] # inclusion에 포함된 환자들의 모든 진단이력 필터링
#rm(t20)
dummy<-filtered_t20
before_main<-grep("I24|I21|I60|I61|I62|I63",dummy$MAIN_SICK) #추가 제외기준J44|N18|K4|K703|I50은 따로 만들어야됨
before_sub<-grep("I24|I21|I60|I61|I62|I63",dummy$SUB_SICK)
before_sub_t20<-dummy[union(before_main,before_sub)] # 주/부 상병코드 중 하나라도 심/뇌혈관질환자
#rm(dummy)
length(unique(before_sub_t20$PERSON_ID))

filtered_index_date<-result[result$PERSON_ID %in% unique(before_sub_t20$PERSON_ID),]



imsi2<-before_sub_t20 %>% nest_by(PERSON_ID)
imsi2<-left_join(imsi2,filtered_index_date,by="PERSON_ID")

imsi2$before_index<-0 #건선 이전에 뇌/심혈관 질환 진단이력 여부 확인하는 변수
imsi2$brain_heart_index_date<-as_date(0) #뇌/심혈관 질환 최초진단일
imsi2$date_duration<- 0   # 건선진단일과 뇌/심혈관 질환 진단일 사이 기간
#뇌/심혈관 질환의 최초진단이 index_date보다 빠르면 제외
for(i in seq_along(imsi2$PERSON_ID)){
  dummy1 <- min(as.numeric(ymd(imsi2$data[[i]]$RECU_FR_DT))) #뇌/심혈관 진단일중 최초진단일을 dummy1에 저장
  imsi2$brain_heart_index_date[i] <- as_date(dummy1)
  imsi2$date_duration[i] <- dummy1-as.numeric(imsi2$index_date[i])

  ifelse(imsi2$date_duration[i]>0,imsi2$before_index[i]<-1,imsi2$before_index[i]<-0 )

}

table(imsi2$before_index)
exclusion_list_0<-imsi2 %>% filter(before_index==0) %>% select(PERSON_ID) # 건선 진단일 이전에 이미 뇌/심혈관 진단이력 있는사람
exclusion_list_1<-imsi2 %>% filter(before_index==1) %>% select(PERSON_ID) # 건선 진단일 이후  뇌/심혈관 진단이력 있는사람
# %in% 의 반대 함수 정의
'%!in%'<-function(x,y){
  !('%in%'(x,y))
}

imsi2
result<-left_join(result,imsi2,by=c("PERSON_ID","index_date")) %>%select(-data)
result #필요한 변수 업데이트




# _3. 추가제외기준필터링 -----------------------------------------------------------

#추가 제외기준J44|N18|K4|K703|I50

dummy<-filtered_t20
extra_main<-grep("J44|N18|K4|K703|I50",dummy$MAIN_SICK)
extra_sub<-grep("J44|N18|K4|K703|I50",dummy$SUB_SICK)
extra_sub_t20<-dummy[union(extra_main,extra_sub)] # 주/부 상병코드 중 하나라도 추가제외기준
#rm(dummy)
length(unique(extra_sub_t20$PERSON_ID))

extra_index_date<-result[result$PERSON_ID %in% unique(extra_sub_t20$PERSON_ID),]



imsi3<-extra_sub_t20 %>% nest_by(PERSON_ID)
imsi3<-left_join(imsi3,extra_index_date,by="PERSON_ID")

imsi3$extra_index<-0 #건선 이전에 추가제외기준 병력 질환 진단이력 여부 확인하는 변수
imsi3$extra_index_date<-as_date(0) #추가제외기준 병력 질환 최초진단일
imsi3$date_duration<- 0   # 건선진단일과 추가제외기준 병력 질환 진단일 사이 기간
#추가제외기준 병력 질환의 최초진단이 index_date보다 빠르면 제외
for(i in seq_along(imsi3$PERSON_ID)){
  dummy1 <- min(as.numeric(ymd(imsi3$data[[i]]$RECU_FR_DT)))
  imsi3$extra_index_date[i] <- as_date(dummy1)
  imsi3$date_duration[i] <- dummy1-as.numeric(imsi3$index_date[i])

  ifelse(imsi3$date_duration[i]>0,imsi3$extra_index[i]<-1,imsi3$extra_index[i]<-0 )

}

table(imsi3$extra_index)
extra_exclusion_list_0<-imsi3 %>% filter(extra_index==0) %>% select(PERSON_ID) # 건선 진단일 이전에 추가제외기준 병력 진단이력 있는사람
extra_exclusion_list_1<-imsi3 %>% filter(extra_index==1) %>% select(PERSON_ID)

# 이사람들은 제외
extra_exclusion_list_0
result<-result[result$PERSON_ID%!in%extra_exclusion_list_0$PERSON_ID,] #업데이트
#step3<-nrow(result)




#_4. 건선진단일로부터 사망,뇌/심혈관진단, 2013/12/31까지의 날짜 계산 --------------------------------

# 이때 만약 중간에 뇌/심혈관계 질환이 있는 사람은 뇌/심혈관 최초진단일까지,
# 중간에 사망이 있으면 사망일까지의 날짜 계산  (생존분석을 위함)

# 2006년이후 진단
mid_result <- result %>% filter(index_date>='2006-01-01'&(is.na(before_index)|before_index==1))


mid_sub_jk<-sub_jk[sub_jk$PERSON_ID%in%mid_result$PERSON_ID,]

length(unique(mid_sub_jk$PERSON_ID))

nrow(mid_sub_jk[!is.na(mid_sub_jk$DTH_YM),]) # 2006년이후 건선진단 받은 환자 4120명중 사망자117명
death_people<-mid_sub_jk[!is.na(mid_sub_jk$DTH_YM),] %>% select(PERSON_ID,DTH_YM)
#rownames(death_people)=NULL
death_people %<>% mutate(DTH_YM=(ym(DTH_YM))) #정확한 사망일을 모르므로 일단 1일로 일괄 설정
mid_result<-left_join(mid_result,death_people,by='PERSON_ID')

#생존기간 변수 survival_date 정의
mid_result %<>% mutate(survival_date= ifelse(is.na(before_index)&is.na(DTH_YM),
                                             as.numeric(as_date('2013-12-31'))-as.numeric(index_date), #if_true
                                             ifelse(is.na(before_index)&!is.na(DTH_YM),#if_false
                                                    as.numeric(as_date(DTH_YM))-as.numeric(index_date),
                                                    date_duration)
)
)

mid_result


# _5. 치료기준만족(중증건선) ---------------------------------------------------------
# Index 후 1년 내 6개월이상 연속치료 받은적이 있는 사람.
# 30일 이내 치료기간은 연속으로 인정
# => 진단일 기록을 오름차순으로 정렬뒤 진단일 끼리 1차 차분한것에 -30.
# ifelse( x>0,x,0) , 이 벡터를 모두 곱해서 0이아니면 6개월연속
# 적어도 두번 기록있는사람들을 imsi에 개인일련번호로 그룹화하여 리스트로 저장
imsi4<-sub_t20[sub_t20$PERSON_ID %in%unique(mid_result$PERSON_ID),] %>% nest_by(PERSON_ID)
# imsi4 %<>% filter(nrow(data)>=5)# 최소 5번은 있어야됨.하지만 나중에 어떻게 바뀔지 모르니 그냥 진행
imsi4<-left_join(imsi4,mid_result[,c(1,2)],by="PERSON_ID")
#1. index_date로부터 지정 기간내의 기록만 필터링하는 코드



imsi4 %<>% mutate(n=nrow(data)) %>% arrange(-n)

imsi4$severe_index<-0  #중증건선 여부 저장하는 변수. 1이면 조건 만족, 0이면 조건 미달
imsi4$max_cumsum<-0  # 최대연속치료기간
for(i in seq_along(imsi4$PERSON_ID)){
  dummy1<-sort(as.numeric(ymd(imsi4$data[[i]]$RECU_FR_DT))-as.numeric(imsi4$index_date[i]))
  dummy1<-dummy1[dummy1<=365]#진단일로부터 관찰기간 지정 (여기서는 365일)
  dummy2<-diff(dummy1)
  csum=0
  for(k in 1:length(dummy2)){
    if(dummy2[k]<=30){ #다음진료까지의 시간이 30일을 이하면,
      csum=csum+dummy2[k]
      if(csum>imsi4$max_cumsum[i]){
        imsi4$max_cumsum[i]<-csum
      }else{csum<-csum}
    }else{csum=0}# 30일을 초과하면 그날부로 다시 셈.
  }
  ifelse(imsi4$max_cumsum[i]>=(180-30),imsi4$severe_index[i]<-1,imsi4$severe_index[i]<-0)
  # 더해서 150일이 누적되어있는상태라면, 이후 30일까지 연속으로 보므로 6개월연속 치료조건 만족
}
table(imsi4$severe_index)
imsi4 %>% filter(severe_index==1) %>% arrange(n)
# imsi4를 View통해서 볼때 임의의 열에 필터링 적용 후 보면 뭔가 뒤틀리게 보이는데 필터링 안하고 보면 잘 맞는듯 합니다.

almost_final_result<-left_join(imsi4[,-c(2,4)],mid_result,by=c("PERSON_ID","index_date"))
almost_final_result %>% group_by(severe_index,before_index) %>% summarise(mean_suv=mean(survival_date))

#              severe_index   before_index    mean_suv
#              <dbl>          <dbl>           <dbl>
# 1              0               1             889.
# 2              0              NA            1488.
# 3              1               1            1215.
# 4              1              NA            1439.

#아주 단순히 중증건선과 심혈관질환 있고없고를 살펴보니,
#중증건선인 집단에서 심혈관/정상 그룹의 생존기간차이가
#중증건선이 아닌집단(치료를 덜받은?)의 심혈관/정상 그룹 생존기간차이보다 작다.


#  필터링에 따른 표본 수  변화 과정 ------------------------------------------------------------

#지금은 일단 어수선해서 아직 정리 안했습니다.

filtered_n <- list(at_least_1=length(unique(sub_t20$PERSON_ID)),
                   at_least_2=nrow(at_least_2),
                   index_num=nrow(index),
                   after_disease_filtered=step3
                   #과거질병이력까지 필터링
)

filtered_n

#10808/1125691*100 #  전체표본에서 index에 속한 인원의 비율(%)



