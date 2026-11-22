###Este es el código empleado para aplicar las funciones anteriores revisadas.
###Se analiza a continuación
####losses almacena Para cada simulación
####total_loss = sev_retained.sum() la severidad total retenida en ese escenario
####sev_sim_all almacena cada severidad sin retención (completa)
####sev_ret_all almacena ahora la retenida
##sev_retained = np.minimum(sev, retention)
###freqs almacena el número de siniestros
##n_claims = int(freq.sum()) por escenario
###los demás son parámetros de los modelos parámetricos
losses, sev_sim_all, sev_ret_all, freqs = montecarlo_annual_losses_detail(
    sim_func=sim_annual_loss_zip_lognormal_detail,
    n_sim=2_000_000,
    panel_sub=panel_sub,
    lambda_hat=lambda_hat,
    p_zero=p_zero,
    sigma=sigma_hat,
    loc=loc_hat,
    scale=scale_hat
)
