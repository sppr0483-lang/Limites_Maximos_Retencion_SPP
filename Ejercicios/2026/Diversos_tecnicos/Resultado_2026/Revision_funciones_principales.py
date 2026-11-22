import numpy as np
import pandas as pd
from scipy.stats import poisson, lognorm
####Función que contiene el código principal para realizar la simulación para una póliza (por riesgo)
##Parámetros panel_sub contienen el número de siniestros
###lmbda_hat y p_zero parámetros de la poisson inflada en 0
###loc, scale argumentos de la severidad lognormal
###retención modificar ahora se deja preeliminarmente en 12M
def sim_annual_loss_zip_lognormal_detail(
    panel_sub,
    lambda_hat,
    p_zero,
    sigma,
    loc,
    scale,
    retention=12_000_000
):
    n = len(panel_sub)
    ###Coloca 1 si la simulación uniforme es mayor que el peso de 0; 0 en otro caso
    ##Se guarda en is_active
    # Zeros estructurales
    is_active = np.random.uniform(size=n) > p_zero

    # Frecuencia Poisson (ANUAL)
    #El ajuste es a una unidad temporal, se extrapola tantas unidades como
    #indica la exposición
    mu = lambda_hat * panel_sub["exposicion_anual"].values
    ##n ceros para frecuencia
    freq = np.zeros(n, dtype=int)
    ##En caso de no ser ceros inflados generar poisson parámetro mu*t
    freq[is_active] = poisson.rvs(mu[is_active])
    ###El número de reclamaciones será la suma
    n_claims = int(freq.sum())
    ###Caso trivial, no hay severdidad
    if n_claims == 0:
        return 0.0, np.array([]), np.array([]), 0
    ##Se simula severidad tantas veces como indica la frecuencia
    # Severidad
    sev = lognorm.rvs(
        s=sigma,
        loc=loc,
        scale=scale,
        size=n_claims
    )
    ##Reaseguro no proporcionalbajo retención anterior
    sev_retained = np.minimum(sev, retention)
    ####Pérdida total es la suma de esta severidad*frec retenida
    total_loss = sev_retained.sum()

    return total_loss, sev, sev_retained, n_claims
