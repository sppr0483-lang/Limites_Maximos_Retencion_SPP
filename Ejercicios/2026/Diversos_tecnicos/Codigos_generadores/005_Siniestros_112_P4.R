#@uthor Act. Alejandro Alberto García de León Jiménez
#####Esta es la última parte para le generación del archivo Siniestros_ramo_112.xlsx
######Se concatenan pólizas TA y !TA en un solo archivo, se reordenan y eliminan duplicados
###Output: SDEF_2026
names(Complemento1)
#####Se descartan productos diferentes a DT
SDEF_2026 = rbind(SDEF_2026 ,Complemento1)
SDEF_2026 = SDEF_2026%>%distinct()
SDEF_2026 = SDEF_2026%>%filter(Monto!=0)
df = sort(unique(SDEF_2026$Producto))
SDEF_2026 = SDEF_2026%>%filter(Producto %in% df[c(1,2,7)])
names(SDEF_2026)
orden <- c(
  "No..Reclamación",
  "Año",
  "Póliza",
  "Producto",
  "Suma Asegurada",
  "Cobertura",
  "Moneda",
  "Estimado + aumento - disminución...8",
  "Estimado + aumento - disminución...9",
  "Garantía",
  "Monto",
  "Fecha Ocurrido"
)
#####Se agrupan por campos
SDEF_2026  = SDEF_2026%>%group_by(Póliza,No..Reclamación,Producto)%>%mutate(`Estimado + aumento - disminución...8` = sum(`Estimado + aumento - disminución...8`,na.rm = TRUE),Monto = sum(Monto, na.rm = TRUE),
                                                                            `Estimado + aumento - disminución...9`= sum(`Estimado + aumento - disminución...9`,na.rm = TRUE),`Suma Asegurada` = max(`Suma Asegurada`, na.rm = TRUE), Garantía = max(Garantía, na.rm = TRUE))%>%distinct()
##Se reordena
SDEF_2026 <- SDEF_2026[, orden]
##Se quitan pólizas ausentes
SDEF_2026 = SDEF_2026[!is.na(SDEF_2026$Póliza),]
library(openxlsx)
##########IMPRESIÓN INSUMO A UTILIZAR
write.xlsx(SDEF_2026,"D:/Limites maximos de retencion/2026/Siniestros_ramo_112.xlsx")
#################################################################################
