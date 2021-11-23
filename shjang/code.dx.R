code.dx <- list(   #새로운 행은 마지막에 추가할 것. 행에 있는 세부 코드는 수정 해도 무관함
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


## Treat
code.uvB <- c("MM331", "MM332", "MM333", "MM334")  ## t30
code.txdrug <- list(
  Cyclosporine = c("139201ACS", "139203ALQ", "139204ACS", "194701ACH", "194701ALQ", "194702ACH", "194701ACS",
                   "194702ACS", "194703ACS", "194730ALQ", "194731ALQ", "139202BIJ", "139230BIJ"),
  Methotrexate = c("192101ATB", "192107ATB", "192102BIJ", "192103BIJ", "192104BIJ", "192105BIJ", "192106BIJ",
                   "192107BIJ", "192108BIJ", "192109BIJ", "192110BIJ", "192111BIJ", "192132BIJ", "192134BIJ",
                   "192136BIJ", "192138BIJ", "192139BIJ", "192140BIJ", "192141BIJ", "192142BIJ", "192143BIJ", "192144BIJ"),
  Acitretin = c("102301ACH", "102302ACH"),
  `TNF-α inhibitor` = c("455801BIJ", "455802BIJ", "455803BIJ", "455830BIJ", "455831BIJ", "383501BIJ", "383502BIJ",
                        "488401BIJ", "488430BIJ", "488431BIJ", "488433BIJ"),
  `Anti-IL-12/23p40` = c("615001BIJ", "615002BIJ", "615032BIJ", "615031BIJ", "615030BIJ"),
  `IL-17A antagonist` = c("644601BIJ", "644602BIJ", "667801BIJ"),
  `IL-23 antagonist` = c("670901BIJ", "686201BIJ")
)




paste0(c(),collapse = "|")

paste0(c(as.vector(unlist(code.dx[1:5]))),collapse = "|")

