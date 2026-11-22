##@uthor Act. Alejandro Alerto García de León Jiménez
library(readxl)
####################################################################
######Los siniestros proporcionados al auditor actuarial del ejercicio con fecha de corte
Siniestros <- read_excel("D:/Limites maximos de retencion/OCURRIDO Y PAGADO SINIESTROS ENE A SEP 2026_ENTREGA.xlsx", 
                                                                 sheet = "DAÑOS", skip = 9)
###Se eliminan columnas no validas
Siniestros <- Siniestros[, colSums(!is.na(Siniestros)) > 0]
#####Se toman únicamente siniestros PAGADOS Y PENDIENTES
z = sort(unique(Siniestros$`Estatus siniestro`))
Siniestros= Siniestros%>%filter(!(`Estatus siniestro` %in% z[c(1,2,3,6)]))
#################################################
########################
################################################################
###Se utiliza base de datos histórica proporcionada en PREG 80 Julio 2026
Historica <- read_excel("D:/Limites maximos de retencion/ASE_HIST.xlsx", skip = 7)
######Preparar archivo siniestros
nombres = c("No. Reclamación", "Año", "Póliza", "Producto", "Suma Asegurada",
  "Cobertura", "Moneda", "Estimado + aumento - disminución",
  "Estimado + aumento - disminución", "Garantía", "Monto",
  "Fecha Ocurrido")
###################################
####Se eliminan columnas innecesarias
Historica <- Historica[, colSums(!is.na(Historica)) > 0]
