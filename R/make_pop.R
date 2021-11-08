library(fst);library(data.table);library(magrittr)
## PATH: file folder
path.KNHIS <- "/home/js/KNHIS"


## Read 20 30 40 60
t20 <- read_fst(file.path(path.KNHIS, "t1_120.fst"), as.data.table = T)
#t30 <- read_fst(file.path(path.KNHIS, "t1_130.fst"), as.data.table = T)
#t40 <- read_fst(file.path(path.KNHIS, "t1_140.fst"), as.data.table = T)
#t60 <- read_fst(file.path(path.KNHIS, "t1_160.fst"), as.data.table = T)


## jk : SEX, AGE_group, Death, Death 원인 등
#jk <- read_fst(file.path(path.KNHIS, "jk.fst"), as.data.table = T)

length(unique(jk$PERSON_ID)) ## N


## Only L40
sub_t20 <- merge(t20, t20[like(MAIN_SICK, "L40") | like(SUB_SICK, "L40"), .SD[1], keyby = "PERSON_ID"][, .(PERSON_ID)], by = "PERSON_ID")
sub_t30 <- merge(read_fst(file.path(path.KNHIS, "t1_130.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
sub_t40 <- merge(read_fst(file.path(path.KNHIS, "t1_140.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")
sub_t60 <- merge(read_fst(file.path(path.KNHIS, "t1_160.fst"), as.data.table = T), sub_t20[, .(KEY_SEQ)], by = "KEY_SEQ")

## Save
for (v in c(20, 30, 40, 60)){
  code.save <- paste0("fst::write_fst(sub_t", v, ", '/home/js/ShinyApps/kbori87/psoriasis/t", v, "s.fst')")
  eval(parse(text=code.save))
}


