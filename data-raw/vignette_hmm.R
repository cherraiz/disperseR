# Hidden Markov models fitted in the vignette
# Modelos ocultos de Markov que se ajustan en la vignette
#
# Fitting the two HMMs takes most of the vignette build time, which pushes the
# CRAN check over its 10-minute limit on Windows. The models are fitted here
# with exactly the code shown in the vignette, and the vignette loads the
# results from vignettes/hmm_fits.rds. Everything else in the vignette runs live.
#
# Ajustar los dos HMM ocupa la mayor parte del tiempo de la vignette, y eso hace
# que el check de CRAN en Windows supere el límite de 10 minutos. Los modelos se
# ajustan aquí con el mismo código que muestra la vignette, y la vignette carga
# los resultados de vignettes/hmm_fits.rds. El resto de la vignette se ejecuta.
#
# Run from the package root after any change in fitHMM.wrap() or sim_states
# Ejecutar desde la raíz del paquete tras cambiar fitHMM.wrap() o sim_states:
#   source("data-raw/vignette_hmm.R")

library(disperseR)

reg <- sim_states[, c("id_split", "time", "x", "y", "sex")]

# Three-state model merged into two (chunk "hmm")
# Modelo de tres estados fusionado en dos (chunk "hmm")
hmm <- fitHMM.wrap(reg, ids = "id_split", stationary = TRUE,
                   mean.step.1 = 700, sd.step.1 = 400,
                   mean.step.2 = 150, sd.step.2 = 100,
                   ang.con.1 = 0.7, ang.con.2 = 0.1)

# Two-state model started near resting and moving values (chunk "hmm-2states")
# Modelo de dos estados iniciado cerca de descanso y movimiento (chunk "hmm-2states")
hmm2 <- fitHMM.wrap(reg, ids = "id_split",
                    mean.step.1 = 300, sd.step.1 = 300,
                    mean.step.2 = 20, sd.step.2 = 20,
                    ang.con.1 = 0.5, ang.con.2 = 0.1)

# Keep only what the vignette uses: the data with states and the step estimates
# Guardar solo lo que usa la vignette: los datos con estados y las estimas de paso
slim <- function(fit) {
  list(data = as.data.frame(fit$data),
       models = list(global_model = list(mle = list(step = fit$models$global_model$mle$step))))
}

saveRDS(list(hmm = slim(hmm), hmm2 = slim(hmm2)),
        file = "vignettes/hmm_fits.rds", compress = "xz")
