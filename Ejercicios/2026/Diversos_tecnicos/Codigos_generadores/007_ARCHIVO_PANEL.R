#@uthor Act. Alejandro Alerto García de León Jiménez
library(openxlsx)
#################En este código se construye el archivo panel_SPP_D.xlsx
###archivo que contendrá el número de siniestros y la exposición
####Construcción archivos panel
###################Se recupera el archivo de agrupación RR4 con duración real
Panel_2026  = XC
#############Se seleccionan las pólizas según contengan o no certificados
########Pólizas TA
Polizas_TA = Panel_2026[sapply(Panel_2026$Cobertura_D,function(x){grepl("^(TA)", x, perl = TRUE)}),]
######Se expanden por Cobertura-certificado
Polizas_TA =  expandir(Polizas_TA,"Cobertura_D","Consecutivo")
Polizas_TA = Polizas_TA%>%distinct()
#######################################3
###########Se generan las que no son Polizas TA
Polizas_ = Panel_2026[sapply(Panel_2026$Cobertura_D,function(x){grepl("^(?!TA)", x, perl = TRUE)}),]
Polizas_=  expandir(Polizas_,"Cobertura_D","Consecutivo")
Polizas_ = Polizas_%>%distinct()
############Se retiran pólizas con registros <=1 peso
Polizas_ = Polizas_%>%filter(abs(Polizas_$Sum_PEAn)>1)
###########
##############Generación Archivo Panel
##############Pólizas \neq TA
#########Para el archivo madre, se seleccionan del vigor inicial las variables
####No se usa la base preparada DT por eficiencia
Vigencia = Vigor%>%select("POL","F_EMI","F_INI_POL","F_FIN_POL", "SUBR","CERT")
###Por ende se filtra de acuerdo al corte
corte <- as.POSIXct("2026-09-30")
Vigencia = Vigencia%>%filter(F_INI_POL<=corte & F_EMI<=corte)
Vigencia = Vigencia%>%filter(SUBR==112)
###############################################33
Vigencia1  = Vigencia[sapply(Vigencia$POL,function(x){grepl("^(?!TA)", x, perl = TRUE)}),]
Vigencia1 = Vigencia1%>%distinct()
####Se cruzan las Polizas necesarias para recuperar las fechas de emisión y vigores
Vigencia1 = Vigencia1%>%left_join(Polizas_ , by = c("POL"="Cobertura_D", "CERT"="Consecutivo"))
######Variable 1 Poliza
Panel_2026_D1 = data.frame(Poliza = Vigencia1$POL)
######Variable 2 Certificado
Panel_2026_D1$CERT = Vigencia1$CERT
######Variable 3 y 4 Vigencia
Panel_2026_D1$`Fecha Ini` = Vigencia1$F_INI_POL
Panel_2026_D1$`Fecha Fin`= Vigencia1$F_FIN_POL
######Variable 5 Duración (RR4)
Panel_2026_D1$`DuraciÃ³n` = Vigencia1$DPajustada
######Variable 6 Año
Panel_2026_D1$anio = Vigencia1$Anio
######Variable 7 Exposición _anual
####Se expande por años en su debida proporción
###"365|366|97" con "2024|2025|2026"
Panel_2026_D1$exposicion_anual  =Vigencia1$Exposicion
Panel_2026_D1 = expandir(Panel_2026_D1,"exposicion_anual","anio")
###mínimo entre la exposisicion anual/365 y 1
###Si durara 366 dias => 366/365>1
###Esto pues el año tuvo 366 dias, se trunca a 1
Panel_2026_D1$exposicion_anual  =pmin(as.numeric(Panel_2026_D1$exposicion_anual)/365,1)
Panel_2026_D1 = Panel_2026_D1%>%distinct()
###########################SINIESTROS
################
###############Se pasa a los siniestros
####NO TA
library(dplyr)
Sin_2 = SDEF_2026%>%filter(grepl("^(?!TA)", Póliza, perl = TRUE))
Sin_2  =Sin_2%>%arrange(Póliza)
Sin_2 = Sin_2%>%distinct()
#####################################################
#########Se extienden por póliza y año 
###se calcula el número de Reclamaciones y se concatenan
##los registros para control
Resumen2 <- Sin_2%>%
  group_by(Póliza,Año) %>%
  summarise(
    No_Reclamaciones = n(),
    Reclamaciones = paste0(`No..Reclamación`, collapse = "|"),
    .groups = "drop"
  )
###El año se vuelve numérico para cruzar
Panel_2026_D1$anio  =as.numeric(Panel_2026_D1$anio)
######Se cruza el Panel_2026 del vigor anterior con duraciones
####con el resumen de siniestros
Panel_2026_D1 = Panel_2026_D1%>%left_join(Resumen2, by = c("Poliza"="Póliza", "anio"="Año"))
##Validaciones
#sum(Panel_2026_D1$No_Reclamaciones, na.rm = TRUE)
#sum(Panel_2026_D$No_Reclamaciones, na.rm = TRUE)
#########################Archivo Panel Pólizas TA
#######################################POLIZAS TA
Vigencia = Vigor%>%select("POL","F_EMI","F_INI_CERT","F_FIN_CERT", "SUBR", "CERT")
corte <- as.POSIXct("2026-09-30")
###Se usan las fechas siguientes, si no hay prolemas; por alguna razón
###pueden ser posteriores a las de la póliza
Vigencia$F_INI_POL = Vigencia$F_INI_CERT
Vigencia$F_FIN_POL = Vigencia$F_FIN_CERT
####Se aplica el corte
Vigencia = Vigencia%>%filter(F_INI_POL<=corte & F_EMI<=corte)
Vigencia = Vigencia%>%filter(SUBR==112)
####Se generan pólizas con iniciación TA seleccionando las variables
Vigencia2  = Vigencia[sapply(Vigencia$POL,function(x){grepl("^(TA)", x, perl = TRUE)}),]
Vigencia2 = Vigencia2%>%left_join(Polizas_TA, by = c("POL"="Cobertura_D", "CERT"="Consecutivo"))
####Base Panel
#Variable 1 Póliza
Panel_2026_D = data.frame(Poliza = Vigencia2$POL)
#Variable 2 Certificado
Panel_2026_D$CERT = Vigencia2$CERT
#Variable 3 Vigencia
Panel_2026_D$`Fecha Ini` = Vigencia2$F_INI_POL
Panel_2026_D$`Fecha Fin`= Vigencia2$F_FIN_POL
#Variable 4 Duración
Panel_2026_D$`DuraciÃ³n` = Vigencia2$DPajustada
#Variable 5 Año
Panel_2026_D$anio = Vigencia2$Anio
#Variable 6 Exposición anual
Panel_2026_D$exposicion_anual  =Vigencia2$Exposicion
#Se expande por exposisión y año
Panel_2026_D = expandir(Panel_2026_D,"exposicion_anual","anio")
Panel_2026_D$exposicion_anual  =pmin(as.numeric(Panel_2026_D$exposicion_anual)/365,1)
Panel_2026_D = Panel_2026_D%>%distinct()
##############################################
#######################Parentesis
####Se toman los siniestros con certificados proporcionados
##Se pidió a siniestros los certificados
#Sin_1 = SDEF_2026%>%filter(grepl("^(TA)", Póliza, perl = TRUE))
#write.xlsx(Sin_1,"D:/Limites maximos de retencion/2026/Siniestros_ramo_TA.xlsx")
Siniestros_ramo_TA_con_certificado <- read_excel("D:/Limites maximos de retencion/Siniestros_ramo_TA_con certificado.xlsx")
####Variables de interés
Siniestros_ramo_TA_con_certificado = Siniestros_ramo_TA_con_certificado%>%select("No..Reclamación","Certificado","Póliza")%>%arrange(Póliza)
library(dplyr)
Sin_1 = SDEF_2026%>%filter(grepl("^(TA)", Póliza, perl = TRUE))
Sin_1  =Sin_1%>%arrange(Póliza)
#Sin_1  = cbind(Sin_1,Siniestros_ramo_TA_con_certificado)
Sin_1 = Sin_1%>%left_join(Siniestros_ramo_TA_con_certificado, by = c("Póliza"="Póliza","No..Reclamación"="No..Reclamación"))
#Sin_1$bien = ifelse(Sin_1$No..Reclamación...1 == Sin_1$No..Reclamación...13,1,0)
#names(Sin_1)[1]  = "No..Reclamación"
#names(Sin_1)[3] = "Póliza"
Sin_1 = Sin_1%>%distinct()
##################################################
#################################################
#####################################################
#####Se generan las cuentas para estos siniestros
##póliza-certificado
Resumen <- Sin_1%>%
  group_by(Póliza,Año,Certificado) %>%
  summarise(
    No_Reclamaciones = n(),
    Reclamaciones = paste0(`No..Reclamación`, collapse = "|"),
    .groups = "drop"
  )
Panel_2026_D$anio  =as.numeric(Panel_2026_D$anio)
Panel_2026_D = Panel_2026_D%>%left_join(Resumen, by = c("Poliza"="Póliza", "anio"="Año", "CERT" = "Certificado"))
#######################################
###########ARCHIVO PANEL FINAL
###############Generación del Panel Definitivo
######Se concatenan ambos archivos panel
Panel_LMR_2026 = rbind(Panel_2026_D,Panel_2026_D1)
Panel_LMR_2026$`_merge` = "left_only"
##############################REORDENAMIENTO
names(Panel_LMR_2026)
orden <- c(
  "Poliza",
  "CERT",
  "Fecha Ini",
  "Fecha Fin",
  "DuraciÃ³n",
  "exposicion_anual",
  "anio",
  "No_Reclamaciones",
  "_merge"
)
Panel_LMR_2026 = Panel_LMR_2026[,orden]
##############33FORMATOS DE FECHA PARA IMPRESIÓN EN EXCEL
Panel_LMR_2026$`Fecha Ini` = as.Date(as.character(as.Date(Panel_LMR_2026$`Fecha Ini`)))
Panel_LMR_2026$`Fecha Fin` = as.Date(Panel_LMR_2026$`Fecha Fin`)
####SE RENOMBRA _siniestros
names(Panel_LMR_2026)[names(Panel_LMR_2026) == "No_Reclamaciones"] <- "n_siniestros"
#####Código de seguridad
#Panel_LMR_2026$n_siniestros[is.na(Panel_LMR_2026$n_siniestros)] = 0
#names(Panel_LMR_2026)[names(Panel_LMR_2026) == "No_Reclamaciones"] <- "n_siniestros"
###Impresión de valores
#write.xlsx(Panel_LMR_2026,"D:/Limites maximos de retencion/2026/panel_SPP_D.xlsx")
#################################################
################################################
