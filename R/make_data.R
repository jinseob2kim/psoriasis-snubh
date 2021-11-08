library(fst);library(data.table);library(magrittr);library(parallel)
#setwd("~/ShinyApps/kbori87/psoriasis-snubh/")
path.KNHIS <- "/home/js/KNHIS"

t20s <- read_fst("t20s.fst", as.data.table = T);setkey(t20s, "KEY_SEQ")
t30s <- read_fst("t30s.fst", as.data.table = T);setkey(t30s, "KEY_SEQ")
t40s <- read_fst("t40s.fst", as.data.table = T);setkey(t40s, "KEY_SEQ")
t60s <- read_fst("t60s.fst", as.data.table = T);setkey(t60s, "KEY_SEQ")
jk <- read_fst(file.path(path.KNHIS, "jk.fst"), as.data.table = T)      ## original




