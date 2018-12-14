
reti <- read.delim("retinol.txt")
head(reti)


mod.retdiet <- lm(retplas ~ retdiet, data=reti)
summary(mod.retdiet)

mod.colest <- lm(retplas ~ colest, data=reti)
summary(mod.colest)

mod.fibra <- lm(retplas ~ fibra, data=reti)
summary(mod.fibra)

mod.grasa <- lm(retplas ~ grasa, data=reti)
summary(mod.grasa)

mod.calorias <- lm(retplas ~ calorias, data=reti)
summary(mod.calorias)

mod.vitamin <- lm(retplas ~ vitamin, data=reti)
summary(mod.vitamin)


reti$fumador <- relevel(reti$fumador, 3)
mod.fumador <- lm(retplas ~ fumador, data=reti)
summary(mod.fumador)

# Task 2


# Task 3
library(MASS)
modAll <- lm(retplas ~ ., data=reti)

summary(modAll)

modBack <- stepAIC(modAll, direction="back", trace=0)
summary(modBack)

modBoth <- stepAIC(modAll, direction="both", trace=0)
summary(modBoth)

predict(modBoth, data.frame(edad=24, sexo="Mujer", 
                            fumador="Fumador",
                            grasa=124.4))

#
# Transformation
#

hist(reti$retplas)
ks.test(reti$retplas, "pnorm")
hist(log(reti$retplas))

reti$logretplas <- log(reti$retplas)

modAll.log <- lm(logretplas ~ . -retplas, data=reti)
mod.log <- stepAIC(modAll.log, direction="both", trace=0)
summary(mod.log)


modFinal <- lm(logretplas ~ sexo + edad + fumador + fibra +
                 betaplas, data=reti)
summary(modFinal)

