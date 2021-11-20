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


paste0(c(),collapse = "|")

paste0(c(as.vector(unlist(code.dx[1:5]))),collapse = "|")

