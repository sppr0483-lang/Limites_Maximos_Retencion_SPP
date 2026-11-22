#####Análisis de función revisora de convergencia
####se regresa un data frame con número de simulaciones, media de las pérdidas
#####Varianza, desviación estándar, percentiles y media de las frecuencias
###esto a partir del vector de pérdidas y frecuencias por escenario
###los checkpoints indican los cortes para los cuales se tomaran estas medidas.
def convergence_metrics(losses, freqs, checkpoints):
    rows = []

    for n in checkpoints:
        L = losses[:n]
        F = freqs[:n]

        rows.append({
            "n_sim": n,
            "mean_loss": L.mean(),
            "var_loss": L.var(ddof=1),
           "sd_loss": L.std(ddof=1),
            "p90": np.quantile(L, 0.90),
            "p95": np.quantile(L, 0.95),
            "p99": np.quantile(L, 0.99),
            "freq_mean": F.mean()
        })

    return pd.DataFrame(rows)
##from, to, by
checkpoints = np.arange(1_000, 5_000_001, 1_000)
###Se regresan las medidas
df_conv = convergence_metrics(losses, freqs, checkpoints)
####Se revisa evolución de las simulaciones
###eje x número de ellas y eje y como van mejorando los percentiles y media a mayor simulaciones
###recuerdese que losses y freqs son agregadas
import matplotlib.pyplot as plt

plt.figure()
plt.plot(df_conv["n_sim"], df_conv["mean_loss"], label="Media")
plt.plot(df_conv["n_sim"], df_conv["p95"], label="P95")
plt.plot(df_conv["n_sim"], df_conv["p99"], label="P99")
plt.xlabel("Número de simulaciones")
plt.ylabel("Pérdida anual")
plt.legend()
plt.title("Convergencia de pérdida anual")
plt.show()
