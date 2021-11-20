library(readxl)
fn = "Medication 재정리_2021-08-05.xlsx"
vSh <- excel_sheets(fn)
vSh

statin<-read_excel(fn, sheet="Statin")
beta_blocker<-read_excel(fn, sheet="Beta-Blocker")

dummy1<-unique(beta_blocker[nchar(beta_blocker$추가)==9,][,1])
dummy2<-unique(statin[nchar(statin$추가)==9,][,1])

st_bb<-(rbind(dummy1,dummy2))$추가


#https://kuduz.tistory.com/1085  #data.table 패키지 사용하는 방법들
#?data.table::.SD
