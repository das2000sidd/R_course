
library(haven)
datos <- read_sav("GitHub/R_course/Data_for_exercises/DATOS COMPLETOS.sav")
View(datos)

library(compareGroups)
sel <- grep("TT", names(datos))
vars <- c("exitus", names(datos)[sel])
datosTT <- datos[, vars]
descr <- compareGroups(exitus ~ ., data=datosTT)
createTable(descr)

datos$superv <- Surv(datos$seguimientoa, datos$exitus=="1")
descr2 <- compareGroups(superv ~ ., data=datos)
table <- createTable(descr2, show.ratio = TRUE)
export2word(table, file="table.docx")
