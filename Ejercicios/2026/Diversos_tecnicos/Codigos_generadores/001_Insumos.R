#####Código para preparación de los datos
#Author Act. Alejandro Alberto García de León Jiménez
####
####2022–2025
###Fuente Vigor usado en cálculo de reservas(diversos técnicos) RR3. Auditoría actuarial
library(readxl)
#####Ubicacion_archivo
Vigor = read_excel("D:/Limites maximos de retencion/2026/Input_reservas/20260930_valuacion_reservas.xlsm", 
                                           sheet = "Base_res",skip = 1)
###########Preparación de los datos agrupación RR4
##################################
DT = Preparar_VigorLMR(Vigor)
##########Se tratan de manera diferente pólizas con certificados
DT$inicio_vig[grepl("^TA",DT$poliza)] = DT$F_INI_ENDS[grepl("^TA",DT$poliza)]
DT$Fin_vig[grepl("^TA",DT$poliza)] = DT$F_FIN_ENDS[grepl("^TA",DT$poliza)]
#########Validación
#Exp = DT%>%filter(DurRem<5)
##Se explora la base output
#names(DT)
#########Se agrupa la base
XC = Agrupacion_LMR(DT)
##############
####Se aplica el método RR4 modificado para considerar duración total
XC = RR4_LMR(XC)
########Se etiquetan productos anuales de multianules
XC$multianual = ifelse(pmax(XC$DPajustada,366)<=366,"A","M")
####En caso de que se deseé explorar cierta póliza
DFR = Vigor%>%filter(POL =="EE00001311")
