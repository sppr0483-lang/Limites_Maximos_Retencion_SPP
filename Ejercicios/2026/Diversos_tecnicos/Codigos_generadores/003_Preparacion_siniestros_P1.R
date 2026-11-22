#@uthor Act. Alejandro ALberto García de León Jiménez
#######En este código se prepara la información del archivo Siniestros_ramo_112.xlsx insumo del cákculo del LMR
##########PARTE 1
###########Información de siniestros
##Base a trabajar
###Se trabaja con dos extractos, uno de la base histórica y otro de los siniestros ocurridos en el ejercicio hasta el corte.
Siniestros_ramo_112_2026 = data.frame(`No. Reclamación` = Historica$`No.de Siniestro`)
Complemento = data.frame(`No. Reclamación` = Siniestros$`No.de Siniestro`)
####Columnas adicionales
#################Outputs: Archivo Siniestros_ramo_112_2026 y Complemento
#####Primera variable: Año
Siniestros_ramo_112_2026$Año = year(Historica$`Fecha Ocurrido`)
Complemento$Año = year(Siniestros$`Fecha Ocurrido`)
#####Segunda variable: Póliza
Siniestros_ramo_112_2026$Póliza  =Historica$Póliza
Complemento$Póliza  = Siniestros$Póliza
#####Tercera variable: Producto
Siniestros_ramo_112_2026$Producto = Historica$Producto
Complemento$Producto = Siniestros$Producto
#####Cuarta variable: `Suma Asegurada`
Siniestros_ramo_112_2026$`Suma Asegurada` = Historica$`Suma Asegurada`
Complemento$`Suma Asegurada` = 0
#####Quinta variable Cobertura
Siniestros_ramo_112_2026$Cobertura = 0
Complemento$Cobertura = 0
#####Sexta variable Moneda
Siniestros_ramo_112_2026$Moneda = Historica$Moneda
Complemento$Moneda = Siniestros$Moneda
#####Séptima variable `Estimado + aumento - disminución...8` Variable sin incorporar moneda y tipo de cambio
######Solo se prepara con estimación inicial
Siniestros_ramo_112_2026 = Siniestros_ramo_112_2026%>%mutate(
  `Estimado + aumento - disminución...8` = ifelse(
    is.na(Historica$`Estimacion inicial`) | trimws(Historica$`Estimacion inicial`) == "",
    0,
    Historica$`Estimacion inicial`
  )
)
Complemento = Complemento %>%mutate(
  `Estimado + aumento - disminución...8` = ifelse(
    is.na(Siniestros$`Estimacion inicial`) | trimws(Siniestros$`Estimacion inicial`) == "",
    0,
    Siniestros$`Estimacion inicial`
  )
)
###############Revisión de nombres
#names(Historica)
#####Octava variable tipo de cambio
Siniestros_ramo_112_2026$tipo_de_cambio = ifelse(Historica$Moneda ==20,Historica$"Monto pagado\r\nM.N."/Historica$"Monto pagado\r\nMoneda original",1)
Complemento$tipo_de_cambio = ifelse(Siniestros$Moneda ==20,Siniestros$"Monto pagado\r\nM.N."/Siniestros$"Monto pagado\r\nMoneda original",1)
#View(Siniestros_ramo_112_2026[is.na(Siniestros_ramo_112_2026$tipo_cambio),])
Siniestros_ramo_112_2026$tipo_de_cambio = ifelse(is.na(Siniestros_ramo_112_2026$tipo_de_cambio),20, Siniestros_ramo_112_2026$tipo_de_cambio)
Complemento$tipo_de_cambio = ifelse(is.na(Complemento$tipo_de_cambio),20, Complemento$tipo_de_cambio)
########Se termina de preparar Séptima variable `Estimado + aumento - disminución...8` Variable sin incorporar moneda y tipo de cambio
Siniestros_ramo_112_2026$`Estimado + aumento - disminución...8` <- coalesce(Siniestros_ramo_112_2026$`Estimado + aumento - disminución...8`, 0) +
  coalesce(Historica$`Aumento de reserva`, 0)- coalesce(Historica$`Disminución de reserva`, 0)
Complemento$`Estimado + aumento - disminución...8`=coalesce(Complemento$`Estimado + aumento - disminución...8`, 0) +
  coalesce(Siniestros$`Aumento de reserva`, 0)- coalesce(Siniestros$`Disminución de reserva`, 0)
#####Octava variable `Estimado + aumento - disminución...9` Se incorpora el tipo de cambio cuando sea aplicable
Siniestros_ramo_112_2026$`Estimado + aumento - disminución...9` <- Siniestros_ramo_112_2026$`Estimado + aumento - disminución...8`*Siniestros_ramo_112_2026$tipo_de_cambio
Complemento$`Estimado + aumento - disminución...9` <-Complemento$`Estimado + aumento - disminución...8`*Complemento$tipo_de_cambio
#####Novena variable Garantía
###Variale de naturaleza informativa y de validación es el pago mas allá de lo provisionado.
Siniestros_ramo_112_2026$Garantía = Historica$"Monto pagado\r\nMoneda original"
Complemento$Garantía = Siniestros$"Monto pagado\r\nMoneda original"
#####Décima variable Monto
Siniestros_ramo_112_2026$Monto = Siniestros_ramo_112_2026$`Estimado + aumento - disminución...9` 
Complemento$Monto = Complemento$`Estimado + aumento - disminución...9`
#####Unodécima variable `Fecha Ocurrido`
Siniestros_ramo_112_2026$`Fecha Ocurrido`  =Historica$`Fecha Ocurrido`
Complemento$`Fecha Ocurrido`  =Siniestros$`Fecha Ocurrido`
