#@uthor Act. Alejandro ALberto García de León Jiménez
#####Cruce con suma asegurada
#####Se continua trabajando con el archivo Siniestros_ramo_112 insumo para cálculo de LMR
###PARTE 2
names(XC)
#####De la agrupación tipo RR4 con duración real se separan las concatenaciones de las 
##pólizas 
Def =XC%>%
  separate_longer_delim(Cobertura_D, delim = "|")
######Se determinan pólizas siniestradas en el ejercicio que no se encuentren en el corte específico
#k = setdiff(Complemento$Póliza,Def$Cobertura_D)
####revisión particular
#Vigor$Fin_vig[Vigor$poliza %in% k[c(13,15)]]
###Dada la naturaleza de los negocios no pueden tratarse igual ciertas pólizas
##################################################################
###############SECCIÓN 1
####Polizas TA
XC1 = XC[sapply(XC$Cobertura_D,function(x){grepl("^(TA)", x, perl = TRUE)}),]
seleccionador = XC1%>%select("Suma_LMR","Cobertura_D","Consecutivo")
seleccionador = seleccionador%>%separate_longer_delim(Cobertura_D, delim = "|")
seleccionador = seleccionador%>%separate_longer_delim(Consecutivo, delim = "|")
##########Separación póliza-certificado
seleccionador = seleccionador%>%distinct()
#####Como suma asegurada se recupera el máximo
seleccionador <- seleccionador %>%
  group_by(Cobertura_D,Consecutivo) %>%
  summarise(
    Suma_LMR = max(Suma_LMR, na.rm = TRUE),
    .groups = "drop"
  )
seleccionador1 = seleccionador%>%distinct()
##############
########En Complemento falta la suma asegurada
Complemento1 =  Complemento[sapply(Complemento$Póliza,function(x){grepl("^(TA)", x, perl = TRUE)}),]
####El área de siniestros proporcionó los certificados Nov/2026
###Importación de certificados
Siniestros_ramo_TA_con_certificado <- read_excel("D:/Limites maximos de retencion/Siniestros_ramo_TA_con certificado.xlsx")
#######Se seleccionan solo variables de interés de la información proporcionada
Tablero_TA = Siniestros_ramo_TA_con_certificado%>%select("Póliza","No..Reclamación","Certificado")%>%distinct
###Se cruzan los siniestros del ejercicio con el tablero anterior 
Complemento1 =Complemento1%>%left_join(Tablero_TA,by=c("Póliza", "No..Reclamación"="No..Reclamación"))
######Ahora se obtiene la suma asegurada cruzando con el seleccionador
Complemento1 = Complemento1%>%left_join(seleccionador, by = c("Póliza" = "Cobertura_D","Certificado" = "Consecutivo"))
Complemento1 = Complemento1%>%distinct()
#######Se revisan los productos
comp = sort(unique(Complemento1$Producto))
Complemento1 = Complemento1[Complemento1$Producto %in% comp[c(1)],]
Complemento1 = Complemento1%>%group_by(No..Reclamación,Año,Póliza,Certificado,Producto,Cobertura,Moneda,`Fecha Ocurrido`)%>%summarise(`Estimado + aumento - disminución...8`  =sum(`Estimado + aumento - disminución...8`),
                                                                                                                          `Estimado + aumento - disminución...9` = sum(`Estimado + aumento - disminución...9`),`Suma Asegurada` = max(`Suma_LMR`),
                                                                                                                          Garantía=sum(Garantía, na.rm = TRUE),Monto = sum(Monto))
##############################SECCIÓN 2
#################################POLIZAS NO TA
XC2 = XC[sapply(XC$Cobertura_D,function(x){grepl("^(?!TA)", x, perl = TRUE)}),]
seleccionador = XC2%>%select("Suma_LMR","Cobertura_D" )
seleccionador = seleccionador%>%separate_longer_delim(Cobertura_D, delim = "|")
seleccionador = seleccionador%>%distinct()
seleccionador <- seleccionador %>%
  group_by(Cobertura_D) %>%
  summarise(
    Suma_LMR = max(Suma_LMR, na.rm = TRUE),
    .groups = "drop"
  )
seleccionador2 = seleccionador%>%distinct()
##############################################
####Aquí no es necesario el certificado
Complemento = Complemento%>%left_join(seleccionador, by = c("Póliza" = "Cobertura_D"))
Complemento = Complemento%>%distinct()
comp = sort(unique(Complemento$Producto))
Complemento1 = Complemento[Complemento$Producto %in% comp[c(1,2,6)],]
Complemento1 = Complemento1%>%group_by(No..Reclamación,Año,Póliza,Producto,Cobertura,Moneda,`Fecha Ocurrido`)%>%summarise(`Estimado + aumento - disminución...8`  =sum(`Estimado + aumento - disminución...8`),
                                                                                      `Estimado + aumento - disminución...9` = sum(`Estimado + aumento - disminución...9`),`Suma Asegurada` = max(`Suma_LMR`),
                                                                                      Garantía=sum(Garantía, na.rm = TRUE),Monto = sum(Monto))
##########################################################
##########################################################
#Cuidado siniestro DA-EE-0211 monto -1000
SDEF_2026 = Siniestros_ramo_112_2026%>%group_by(No..Reclamación,Año,Póliza,Producto,Cobertura,Moneda,`Fecha Ocurrido`)%>%summarise(`Estimado + aumento - disminución...8`  =sum(`Estimado + aumento - disminución...8`),
                                                                                                                                   `Estimado + aumento - disminución...9` = sum(`Estimado + aumento - disminución...9`),`Suma Asegurada` = first(`Suma Asegurada`),
                                                                                                                                   Garantía=sum(Garantía, na.rm = TRUE),Monto = sum(Monto))
SDEF_2026$`Suma Asegurada` = as.numeric(SDEF_2026$`Suma Asegurada`)
##########################################################################
