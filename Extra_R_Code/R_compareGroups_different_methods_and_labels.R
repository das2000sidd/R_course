
reti <- read.delim("retinol.txt")
head(reti)

# nos inventamos esta variable  ...
reti$var <- sample(c(0,1), nrow(reti))

# recode some variables
reti$var.c <- factor(reti$var, labels=c("no", "si"))
summary(reti$var.c)


library(compareGroups)
ans <- compareGroups(fumador ~ var.c + alcohol + betadiet + 
                       retdiet + grasa, 
                     data = reti, 
                     method = c(grasa=2, retdiet=2))
ans

createTable(ans)


# Poner etiquetas a las variables

library(Hmisc)
Hmisc::label(reti$var.c) <- "Variable simulada"
ans <- compareGroups(fumador ~ var.c + alcohol + betadiet + 
                       retdiet + grasa, 
                     data = reti, 
                     method = c(grasa=2, retdiet=2))
createTable(ans)
