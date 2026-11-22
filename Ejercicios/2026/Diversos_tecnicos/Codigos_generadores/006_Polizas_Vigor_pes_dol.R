#@uthor Act. Alejandro ALerto García de León Jiménez
####Este código presenta la construcción del insumo Polizas Vigor pesos-dol con vigencias.xlsx
###Que es de donde se filtrará en el Código phyton Act. Angelina Mina las pólizas en vigor a este corte
###Archivo vigor
library(readxl)
Vigor1 = read_excel("D:/Limites maximos de retencion/2026/Input_reservas/20260930_valuacion_reservas.xlsm", 
                   sheet = "Base_res",skip = 1)
##############
####El cambio respecto al primer trabajo se da en esta función
##Nótese es Polizas_VigorLMR() no Preparar_VigorLMR.
###Cambia en los filtros usados.
###No considera historia a corte sino pólizas con periodo actual de vigencia
DT1 = Polizas_VigorLMR(Vigor1)
####Solo si son pólizas TA
DT1$inicio_vig[grepl("^TA",DT1$poliza)] = DT1$F_INI_ENDS[grepl("^TA",DT1$poliza)]
DT1$Fin_vig[grepl("^TA",DT1$poliza)] = DT1$F_FIN_ENDS[grepl("^TA",DT1$poliza)]
#Exp = DT%>%filter(DurRem<5)
#names(DT)
#############################################333
##############Agrupaciones
Agrup_1 = Agrupacion_LMR(DT1)
Agrup_1 = RR4_LMR(Agrup_1)
########################################
Agrup_1$multianual = ifelse(pmax(Agrup_1$DPajustada,366)<=366,"A","M")
Agrup_1=  expandir(Agrup_1,"Cobertura_D","Consecutivo")
#############################3Se generan las variables de interés para dicho código
Agrupa_Vigor = data.frame(POL = Agrup_1$Cobertura_D)
Agrupa_Vigor$Consecutivo = Agrup_1$Consecutivo
Agrupa_Vigor$`Fecha Ini` = Agrup_1$inicio_vig
Agrupa_Vigor$`Fecha Fin` = Agrup_1$Fin_vig
Agrupa_Vigor$Duración = Agrup_1$DPajustada
###################Impresión de resultados
write.xlsx(Agrupa_Vigor,"D:/Limites maximos de retencion/2026/Polizas Vigor pesos-dol con vigencias.xlsx", sheetName = "Pesos Básica Depurada")
