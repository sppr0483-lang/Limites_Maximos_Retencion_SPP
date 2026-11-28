###################Modelo lineal generalizado para frecuencia
###################Modelo lineal generalizado para severidad
####################Validación de Bondad de ajuste para ambos
####################Generación de escenarios
####################Prueba de límite máximo de retención 20 millones
####################En todos los escenarios la siniestralidad quedó por debajo del límite máximo propuesto
####################0% de las simulaciones excedieron LMR
####################Tablas de control
####################Impresión de resultados
####################
########################Importación  de datos
####Cambiar ruta
ruta = "D:/Limites maximos de retencion/Resultados_2026/Miscelaneos/data4.xlsx"
data1 = read_excel(ruta)
names(data1) = c("FACT1","FACT2","Año","NumClaims","Severidad","Duración Total","Duration","POL")
#data1 $Duration = data1$`Duración D*`
#data1$NumClaims  =data1$Siniestros*data1$Duration
# Conversión de variables
data1$Duration  <- as.numeric(data1$Duration)
data1$Severidad <- as.numeric(data1$Severidad)

data1$FACT1 <- as.factor(data1$FACT1)
data1$FACT2 <- as.factor(data1$FACT2)

# Niveles de referencia
data1$FACT1 <- relevel(data1$FACT1, ref = "1")
data1$FACT2 <- relevel(data1$FACT2, ref = "1")

levels(data1$FACT1)

#################################################
# MODELO DE FRECUENCIA
#################################################

mymodel <- glm(
  formula = (NumClaims)^3 ~ FACT1 + FACT2,
  family = "poisson",
  data = data1
  #offset = log(Duration)
)

summary(mymodel)

mat1 <- model.matrix(mymodel, data = data1)
betas1 <- coef(mymodel)

############################Validacion
mat1 <- model.matrix(mymodel, data = data1)
betas1 <- coef(mymodel)
set.seed(123)
res = residuals(mymodel,type = "response")+900
library(MASS)
fit <- fitdistr((res),  densfun = "poisson")
fit$estimate
poisson = rpois(13, fit$estimate[1])
ks = ks.test(poisson, res)
ks
#########################
#################################
####la prueba se realizo sobre epsilon +900
#####Sea [NSiniestros^3]-(\sum \beta_i*FACT_i + \beta_0) = \epsilon
####se simula epsilon' = epsilon+900 \sim Poisson por la Kolmgorov Smirnov
#### Se recupera \epsilon = \epsilon'-900 para cada escenario
####Se suma sum \beta_i*FACT_i + \beta_0 a  \epsilon lo 
##que da igual a [NSiniestros]^3
###[\sum \beta_i*FACT_i + \beta_0 + epsilon]^{\frac{1}{3}} = NSiniestros
############Escenario para la frecuencia
set.seed(123)
#escenario = rpois(2000000,fit$estimate[1]) 
tendencia = (mymodel$coefficients[1]*colMeans(mat1)[1]) + sum(mymodel$coefficients[2:7]*colMeans(mat1)[2:7])
escenario = rpois(2000000,fit$estimate[1]) 
escenario = escenario[!is.na(escenario)]
summary(escenario)
length(escenario)
escenario = escenario-900
Escenario =  (escenario + tendencia)
Escenario  = sign(Escenario) * abs(Escenario)^(1/3)
Escenario = Escenario[Escenario>=0]
summary(Escenario)
###############################################
pdf("D:/Limites maximos de retencion/Resultados_2026/FrecKS_Test.pdf", width = 8, height = 6)
plot.new()
texto <- capture.output(ks)
text(
  x = 0,
  y = 1,
  labels = paste(texto, collapse = "\n"),
  adj = c(0, 1),
  family = "mono"
)
dev.off()
###################################
#############Severidad
#################################################
# MODELO DE SEVERIDAD
#################################################
data2 <- data1[data1$NumClaims > 0, ]
data2$Severidad
mymodel2 <- glm(
  formula = log(Severidad) ~ FACT1 + FACT2,
  family = Gamma(link = "log"),
  weights = NumClaims,
  data = data2
)
mat2 <- model.matrix(mymodel2, data = data1)
summary(mymodel2)
res = residuals(mymodel2,type = "response")+.05
library(MASS)
fit <- fitdistr(exp(res), densfun = "gamma")
fit$estimate
set.seed(123)
gama = rgamma(13,shape= fit$estimate[1],rate = fit$estimate[2])
ks1 = ks.test(gama, exp(res))
ks1
###########
set.seed(123)
escenario2 = rgamma(length(Escenario),shape= fit$estimate[1],rate = fit$estimate[2])
tendencia2 = exp((mymodel2$coefficients[1]*colMeans(mat2)[1]) + sum(mymodel2$coefficients[2:7]*colMeans(mat2)[2:7]))
escenario2 = log(escenario2-.05)
Escenario2 = exp(tendencia2+escenario2)
summary(Escenario2)
Escenario_Def = Escenario*Escenario2
summary(Escenario_Def)
Escenario_Def = Escenario_Def[!is.na(Escenario_Def)]

se1  <- data.frame(Estadistico = names(summary(Escenario_Def)),
                   Valor = as.numeric(summary(Escenario_Def)))

frec1 <- data.frame(Estadistico = names(summary(Escenario)),
                    Valor = as.numeric(summary(Escenario)))

sev1 <- data.frame(Estadistico = names(summary(Escenario2)),
                   Valor = as.numeric(summary(Escenario2)))
RETENCION = 20000000
PruebaLMR = pmax(Escenario_Def-RETENCION,0)
summary(Escenario_Def)
quantile(PruebaLMR,.95,na.rm = TRUE)
summary(PruebaLMR)
Res1  <- data.frame(Estadistico = names(summary(PruebaLMR)),
                   Valor = as.numeric(summary(PruebaLMR)))
#summary(PruebaLMR)
#dfg = data.frame(E1 = Escenario)
library(openxlsx)

wb <- createWorkbook()

# Hoja 1
addWorksheet(wb, "Sheet1")
dfg = data.frame(Simulacion = Escenario)
writeData(wb, sheet = "Sheet1", x = dfg)

# Hoja 2
addWorksheet(wb, "Sheet2")
dfg = data.frame(Simulacion = Escenario2)
writeData(wb, sheet = "Sheet2", x = dfg)


addWorksheet(wb, "Sheet3")
dfg = data.frame(Simulacion = Escenario_Def)
writeData(wb, sheet = "Sheet3", x = dfg)
# Hoja 4
addWorksheet(wb, "Tablas_control1")
writeData(wb, sheet = "Tablas_control1", x = frec1)
# Hoja 5
addWorksheet(wb, "Tablas_control2")
writeData(wb, sheet = "Tablas_control2", x = sev1)
# Hoja 6
addWorksheet(wb, "Tablas_control3")
writeData(wb, sheet = "Tablas_control3", x = se1)
##################
addWorksheet(wb, "ResultadosDM")
writeData(wb, sheet = "Tablas_control3", x = PruebaLMR)
##################
addWorksheet(wb, "Tabla_ResultadosDM")
writeData(wb, sheet = "Tabla_ResultadosDM", x = Res1)

# Guardar archivo

saveWorkbook(wb, "D:/Limites maximos de retencion/Resultados_2026/Miscelaneos/OutcomeSimulDM.xlsx", overwrite = TRUE)
