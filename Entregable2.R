setwd("example")

library("car")
library("lsmeans")
options(contrasts = c("contr.sum","contr.treatment"))

data<-read.csv2("Fernández Cadenas, Dídac.csv")
attach(data)
summary(data)

## Apartat 1: Homogeneitat en els grups
# Tests d'homogeneitat de les categoriques
chisq.test(data$Sexo,data$Grupo,correct = FALSE)
chisq.test(data$NivEst,data$Grupo,correct = FALSE)
chisq.test(data$EstCiv,data$Grupo,correct = FALSE)
chisq.test(data$Diag,data$Grupo,correct = FALSE)

# Test d'homogeneitat de les continues
# Edat
Edad_GC = data[which(Grupo=="GC"),]$Edad
Edad_GE = data[which(Grupo=="GE"),]$Edad
# Variancia
var.test(Edad_GC,Edad_GE)
# Esperança
t.test(Edad_GC,Edad_GE,var.equal=TRUE)

# Anys d'Evolució
AnEvol_GC = data[which(Grupo=="GC"),]$AnEvol
AnEvol_GE = data[which(Grupo=="GE"),]$AnEvol

var.test(AnEvol_GC,AnEvol_GE)
t.test(AnEvol_GC,AnEvol_GE,var.equal=TRUE)


## Apartat 2: Homogeneitat en les dades T-Score basals
# Aprenentatge Verbal, nivells basals
AprenVerbB_GC = data[which(Grupo=="GC"),]$AprenVerbB
AprenVerbB_GE = data[which(Grupo=="GE"),]$AprenVerbB

var.test(AprenVerbB_GC,AprenVerbB_GE)
t.test(AprenVerbB_GC,AprenVerbB_GE,var.equal=FALSE)

# Aprenentatge Visual, nivells basals
AprenVisB_GC = data[which(Grupo=="GC"),]$AprenVisB
AprenVisB_GE = data[which(Grupo=="GE"),]$AprenVisB

var.test(AprenVisB_GC,AprenVisB_GE)
t.test(AprenVisB_GC,AprenVisB_GE,var.equal=TRUE)


## Apartat 3: Homogeneitat en les dades T-Score basals
# Aprenentatge Verbal, nivells postexperimentals
AprenVerbP_GC = data[which(Grupo=="GC"),]$AprenVerbP
AprenVerbP_GE = data[which(Grupo=="GE"),]$AprenVerbP

var.test(AprenVerbP_GC,AprenVerbP_GE)
t.test(AprenVerbP_GC,AprenVerbP_GE,var.equal=TRUE)

# Aprenentatge Visual, nivells postexperimentals
AprenVisP_GC = data[which(Grupo=="GC"),]$AprenVisP
AprenVisP_GE = data[which(Grupo=="GE"),]$AprenVisP
# Variancia
var.test(AprenVisP_GC,AprenVisP_GE)
# Esperança
t.test(AprenVisP_GC,AprenVisP_GE,var.equal=TRUE)

## Apartat 4:
# Diferencia entre T-Scores
AprenVerbDif_GC = AprenVerbP_GC - AprenVerbB_GC
AprenVisDif_GC = AprenVisP_GC - AprenVisB_GC
AprenVerbDif_GE = AprenVerbP_GE - AprenVerbB_GE
AprenVisDif_GE = AprenVisP_GE - AprenVisB_GE

# Busquem els intervals de confiança per a cada diferencia (donats per t.test)
var.test(AprenVerbDif_GC,AprenVerbDif_GE)
t.test(AprenVerbDif_GC,AprenVerbDif_GE,var.equal=TRUE)

var.test(AprenVisDif_GC,AprenVisDif_GE)
t.test(AprenVisDif_GC,AprenVisDif_GE,var.equal=TRUE)

# Comparació de mitjanes basal-post 
# Grup Control
# Aprenentatge Verbal
var.test(AprenVerbB_GC,AprenVerbP_GC)
t.test(AprenVerbB_GC,AprenVerbP_GC,paired=TRUE,var.equal=FALSE)
# Aprenentatge Visual
var.test(AprenVisB_GC,AprenVisP_GC)
t.test(AprenVisB_GC,AprenVisP_GC,paired=TRUE,var.equal=TRUE)
# Grup Experimental
# Aprenentatge Verbal
var.test(AprenVerbB_GE,AprenVerbP_GE)
t.test(AprenVerbB_GE,AprenVerbP_GE,paired=TRUE,var.equal=TRUE)
# Aprenentatge Visual
var.test(AprenVisB_GE,AprenVisP_GE)
t.test(AprenVisB_GE,AprenVisP_GE,paired=TRUE,var.equal=TRUE)


## Apartat 5
# ANOVA sobre les diferencies basal-post
# Aprenentatge Verbal
AprenVerbDif = AprenVerbP - AprenVerbB
modVerb = lm(AprenVerbDif~Grupo,data)
summary(modVerb)
Anova(modVerb,type=3)
# Alternativament
var.test(AprenVerbDif_GC,AprenVerbDif_GE)
t.test(AprenVerbDif_GC,AprenVerbDif_GE,var.equal=TRUE)
# Aprenentatge Visual
AprenVisDif = AprenVisP - AprenVisB
modVis = lm(AprenVisDif~Grupo,data)
summary(modVis)
Anova(modVis,type=3)
# Alternativament
var.test(AprenVisDif_GC,AprenVisDif_GE)
t.test(AprenVisDif_GC,AprenVisDif_GE,var.equal=TRUE)

########################################################
########################################################

## Algunes quantitats d'interes (barcharts)

# Grup Control
dataGC <- data[which(data$Grupo=='GC'),]
# Grup Experimental
dataGE <- data[which(data$Grupo=='GE'),]
# Valors del Grup Control
Sex_H_GC <- nrow(dataGC[which(dataGC$Sexo=='H'),])
Sex_M_GC <- nrow(dataGC[which(dataGC$Sexo=='M'),])
Est_PI_GC <- nrow(dataGC[which(dataGC$NivEstud=='PI'),])
Est_PC_GC <- nrow(dataGC[which(dataGC$NivEstud=='PC'),])
Est_S_GC <- nrow(dataGC[which(dataGC$NivEstud=='S'),])
Civ_S_GC <- nrow(dataGC[which(dataGC$EstCiv=='S'),])
Civ_O_GC <- nrow(dataGC[which(dataGC$EstCiv=='O'),])
Diag_TP_GC <- nrow(dataGC[which(dataGC$Diag=='TP'),])
Diag_OT_GC <- nrow(dataGC[which(dataGC$Diag=='OT'),])
# Valors del Grup Experimental
Sex_H_GE <- nrow(dataGE[which(dataGE$Sexo=='H'),])
Sex_M_GE <- nrow(dataGE[which(dataGE$Sexo=='M'),])
Est_PI_GE <- nrow(dataGE[which(dataGE$NivEstud=='PI'),])
Est_PC_GE <- nrow(dataGE[which(dataGE$NivEstud=='PC'),])
Est_S_GE <- nrow(dataGE[which(dataGE$NivEstud=='S'),])
Civ_S_GE <- nrow(dataGE[which(dataGE$EstCiv=='S'),])
Civ_O_GE <- nrow(dataGE[which(dataGE$EstCiv=='O'),])
Diag_TP_GE <- nrow(dataGE[which(dataGE$Diag=='TP'),])
Diag_OT_GE <- nrow(dataGE[which(dataGE$Diag=='OT'),])
