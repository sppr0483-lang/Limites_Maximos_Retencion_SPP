##Author Act.Alejandro ALberto García de León Jiménez
##Vease Pag 140 PE, ejercicios 128, 129
set.seed(123)
Simular_nsiniestros = function(npol,lambda,pi){
  lam = (1-pi)*lambda
  der = rpois(npol, lam)
  return(der)
}
lambda=  0.3954788156177575
pi = 0.9757203689907209
uy = Simular_nsiniestros(length(Panel_LMR_2026$n_siniestros),lambda,pi)
ks.test(Panel_LMR_2026$n_siniestros,uy)
