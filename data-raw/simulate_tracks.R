# Simulated dispersal tracks used in examples, tests and the vignette
# Trayectorias de dispersión simuladas para los ejemplos, tests y la vignette
#
# Run from the package root / Ejecutar desde la raíz del paquete:
#   source("data-raw/simulate_tracks.R")
#
# Movement model (hourly) / Modelo de movimiento (horario):
#   - Diel rhythm: active 19:00-07:00, resting during the day (also while dispersing).
#     Ritmo diario: activo de 19:00 a 07:00, descansa de día (también al dispersarse).
#   - Directed movements: biased correlated random walk with a slowly drifting heading,
#     so that routes meander. Departures start at dusk.
#     Desplazamientos: paseo aleatorio correlacionado y sesgado hacia el destino, con una
#     deriva lenta del rumbo para que las rutas serpenteen. Las salidas empiezan al anochecer.
#   - Area-restricted phases: Ornstein-Uhlenbeck attraction to a centre.
#     Fases restringidas: atracción tipo Ornstein-Uhlenbeck hacia un centro.
#   - Stopovers: stays of 0.5-1.5 days inside some movements.
#     Paradas: estancias de 0,5-1,5 días dentro de algunos desplazamientos.
#
# Individuals / Individuos:
#   M01: area A made of two patches (A1, A2) with shuttles between them, move to B with a
#        stopover, loop from B, return to A (with a stopover).
#   F02: settles in D, 2.5-day stopover S, then B2 and C (stopover on the way to C).
#   M03: settles in E, long transit with a ~2-day monitoring gap, settles in G.
#   F04: home range H made of two patches (H1, H2), with two loops.
#   F05: released animal; exploration with stopovers, transient settlement in T, departs
#        again and the tracking ends while it is moving.

library(dplyr)

set.seed(20240301)

t0 <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC")

# Hourly simulator with a clock / Simulador horario con reloj ---------------------------

new_walker <- function(id, origin, start_hour = 12) {
  env <- new.env()
  env$id <- id
  env$pos <- origin
  env$hour <- start_hour          # hours since t0 / horas desde t0
  env$heading <- runif(1, -pi, pi)
  env$drift <- 0
  env$label <- "travel"
  env$step <- 0
  env$out <- list()
  env
}

is_active <- function(hour) {
  h <- hour %% 24
  h >= 19 | h < 7
}

record <- function(w) {
  w$out[[length(w$out) + 1]] <- list(w$hour, w$pos[1], w$pos[2], w$label, w$step)
  w$hour <- w$hour + 1
}

# Stay around a centre for a number of days / Permanecer alrededor de un centro
stay <- function(w, centre, days, pull = 0.12, sd_active = 130, sd_rest = 12) {
  n <- round(days * 24)
  for (i in seq_len(n)) {
    if (is_active(w$hour)) {
      w$pos <- w$pos + pull * (centre - w$pos) + rnorm(2, 0, sd_active)
    } else {
      w$pos <- w$pos + rnorm(2, 0, sd_rest)
    }
    record(w)
  }
}

# Keep staying until dusk / Seguir en el sitio hasta el anochecer
wait_dusk <- function(w, centre, ...) {
  while ((w$hour %% 24) != 19) stay(w, centre, 1 / 24, ...)
}

# Directed movement to a target / Desplazamiento dirigido hacia un objetivo
travel <- function(w, target, persistence = 0.45, speed_shape = 2.5, speed_scale = 260,
                   arrive = 500, max_hours = 2000) {
  for (i in seq_len(max_hours)) {
    to_target <- target - w$pos
    dist <- sqrt(sum(to_target^2))
    if (dist < arrive) break
    if (is_active(w$hour)) {
      w$drift <- 0.9 * w$drift + rnorm(1, 0, 0.07)
      bearing <- atan2(to_target[2], to_target[1])
      hx <- persistence * cos(w$heading) + (1 - persistence) * cos(bearing)
      hy <- persistence * sin(w$heading) + (1 - persistence) * sin(bearing)
      w$heading <- atan2(hy, hx) + w$drift + rnorm(1, 0, 0.3)
      step <- min(rgamma(1, shape = speed_shape, scale = speed_scale), dist)
      w$pos <- w$pos + step * c(cos(w$heading), sin(w$heading))
    } else {
      w$pos <- w$pos + rnorm(2, 0, 12)
    }
    record(w)
  }
}

# Run a plan of phases / Ejecutar un plan de fases
# The simulated behaviour of each location is kept in 'behaviour' (stay, stopover, travel)
# and the index of the plan item in 'step' / El comportamiento simulado se guarda en
# 'behaviour' y el número de elemento del plan en 'step'
run_plan <- function(w, plan) {
  for (k in seq_along(plan)) {
    p <- plan[[k]]
    w$step <- k
    switch(p$type,
           stay = {
             w$label <- "stay"
             stay(w, p$centre, p$days)
           },
           stop = {
             w$label <- "stopover"
             stay(w, w$pos, p$days, pull = 0.3, sd_active = 50)
           },
           travel = {
             wait_dusk(w, w$pos, pull = 0.3, sd_active = 50)
             w$label <- "travel"
             travel(w, p$to)
           },
           depart = wait_dusk(w, p$centre),
           shuttle = for (j in seq_along(p$hours)) {
             centre <- p$centres[[(j - 1) %% length(p$centres) + 1]]
             wait_dusk(w, w$pos, pull = 0.3, sd_active = 50)
             w$label <- "stay"
             travel(w, centre)
             stay(w, centre, p$hours[j] / 24)
           })
  }
  data.frame(ID = w$id,
             hour = vapply(w$out, `[[`, numeric(1), 1),
             x = vapply(w$out, `[[`, numeric(1), 2),
             y = vapply(w$out, `[[`, numeric(1), 3),
             behaviour = vapply(w$out, `[[`, character(1), 4),
             step = vapply(w$out, `[[`, numeric(1), 5))
}

# Patch centres (ETRS89 / UTM 30N, metres) / Centros de los parches (metros) ------------
A1 <- c(430000, 4300000); A2 <- c(433000, 4301800)
B  <- c(447000, 4313000); B2 <- c(452000, 4320000)
C  <- c(466000, 4306000); D  <- c(434000, 4326000)
S  <- c(443000, 4316000); E  <- c(410000, 4280000)
G  <- c(449000, 4266000)
H1 <- c(472000, 4326000); H2 <- c(475000, 4328500)
T1 <- c(395000, 4318000)

m01 <- run_plan(new_walker("M01", c(414000, 4291000)), list(
  list(type = "travel", to = A1),
  list(type = "stay", centre = A1, days = 2.5),
  list(type = "shuttle", centres = list(A2, A1), hours = c(48, 60, 40, 72, 48)),
  list(type = "depart", centre = A1),
  list(type = "travel", to = (A1 + B) / 2 + c(-1500, 1500)),
  list(type = "stop", days = 1),
  list(type = "travel", to = B),
  list(type = "stay", centre = B, days = 6),
  list(type = "depart", centre = B),
  list(type = "travel", to = B + c(7000, 3000)),
  list(type = "travel", to = B),
  list(type = "stay", centre = B, days = 4),
  list(type = "depart", centre = B),
  list(type = "travel", to = (A1 + B) / 2 + c(2000, -1500)),
  list(type = "stop", days = 0.75),
  list(type = "travel", to = A1),
  list(type = "stay", centre = A1, days = 5)
))

f02 <- run_plan(new_walker("F02", c(421000, 4333000)), list(
  list(type = "travel", to = D),
  list(type = "stay", centre = D, days = 7),
  list(type = "depart", centre = D),
  list(type = "travel", to = S),
  list(type = "stay", centre = S, days = 2.5),
  list(type = "depart", centre = S),
  list(type = "travel", to = B2),
  list(type = "stay", centre = B2, days = 8),
  list(type = "depart", centre = B2),
  list(type = "travel", to = (B2 + C) / 2 + c(1500, 2000)),
  list(type = "stop", days = 0.5),
  list(type = "travel", to = C),
  list(type = "stay", centre = C, days = 6)
))

m03 <- run_plan(new_walker("M03", c(398000, 4292000)), list(
  list(type = "travel", to = E),
  list(type = "stay", centre = E, days = 6),
  list(type = "depart", centre = E),
  list(type = "travel", to = G),
  list(type = "stay", centre = G, days = 8)
))

f04 <- run_plan(new_walker("F04", c(483000, 4342000)), list(
  list(type = "travel", to = H1),
  list(type = "stay", centre = H1, days = 2),
  list(type = "shuttle", centres = list(H2, H1), hours = c(60, 48, 72)),
  list(type = "depart", centre = H1),
  list(type = "travel", to = H1 + c(-6000, -5000)),
  list(type = "travel", to = H1),
  list(type = "shuttle", centres = list(H1, H2, H1), hours = c(36, 60, 72)),
  list(type = "depart", centre = H1),
  list(type = "travel", to = H2 + c(7000, 1000)),
  list(type = "travel", to = H2),
  list(type = "stay", centre = H2, days = 4)
))

f05 <- run_plan(new_walker("F05", c(405000, 4305000)), list(
  list(type = "travel", to = c(398000, 4296000)),
  list(type = "stop", days = 1),
  list(type = "travel", to = c(388000, 4304000)),
  list(type = "stop", days = 0.5),
  list(type = "travel", to = T1),
  list(type = "stay", centre = T1, days = 6),
  list(type = "depart", centre = T1),
  list(type = "travel", to = c(408000, 4330000)),
  list(type = "stop", days = 1),
  list(type = "travel", to = c(420000, 4338000))
))

# Fixes with timing jitter and ~10% missed fixes
# Localizaciones con desfase temporal y ~10% de fallos
sim_tracks <- dplyr::bind_rows(m01, f02, m03, f04, f05) %>%
  dplyr::mutate(timestamp = t0 + hour * 3600 + round(stats::runif(dplyr::n(), -300, 300))) %>%
  dplyr::filter(stats::runif(dplyr::n()) > 0.1)

# M03: ~2-day monitoring gap during the long transit
# M03: hueco de ~2 días sin datos durante el tránsito largo
gap_start <- min(m03$hour[m03$step == 4]) + 30
sim_tracks <- sim_tracks %>%
  dplyr::filter(!(ID == "M03" & hour >= gap_start & hour < gap_start + 48))

sim_tracks <- sim_tracks %>%
  dplyr::mutate(sex = ifelse(substr(ID, 1, 1) == "M", "male", "female"),
                x = round(x, 1), y = round(y, 1)) %>%
  dplyr::select(ID, timestamp, x, y, sex, behaviour) %>%
  as.data.frame()

if (!exists("run_models") || isTRUE(run_models)) {

  usethis::use_data(sim_tracks, overwrite = TRUE)

  # Regularised and HMM-classified version, built with the package functions
  # Versión regularizada y clasificada con HMM, generada con las funciones del paquete
  devtools::load_all(".")

  # Split at the monitoring gap so that ctmm does not interpolate across it
  # Dividir en el hueco para que ctmm no interpole a través de él
  split_trk <- divide.track.by.gap(sim_tracks, ids = "ID", time.col = "timestamp",
                                   gap.days = 1)

  reg <- resample.ctmm(as.data.frame(split_trk), tint = 3600, ids = "id_split",
                       timestamp = "timestamp", proj.in = 25830,
                       static.cols = "sex", verbose = FALSE)

  # Fit the HMM by segment, so that no step crosses the gap; then restore the individual ID.
  # A stationary state (daytime rest) is added and merged into Encamped.
  # Ajustar el HMM por segmento para que ningún paso cruce el hueco; después recuperar el ID.
  # Se añade un estado estacionario (descanso diurno) que se une a Encamped.
  hmm <- fitHMM.wrap(reg, ids = "id_split", stationary = TRUE,
                     mean.step.1 = 700, sd.step.1 = 400,
                     mean.step.2 = 150, sd.step.2 = 100,
                     ang.con.1 = 0.7, ang.con.2 = 0.1)
  hmm$data$id_split <- as.character(hmm$data$ID)
  hmm$data$ID <- sub("_[0-9]+$", "", hmm$data$id_split)

  sim_states <- as.data.frame(hmm$data)
  class(sim_states) <- "data.frame"
  sim_states$state <- as.integer(sim_states$state)
  sim_states$ID <- as.character(sim_states$ID)
  sim_states$angle <- as.numeric(sim_states$angle)
  rownames(sim_states) <- NULL
  attr(sim_states, "coords") <- NULL
  attr(sim_states, "gradient") <- NULL
  sim_states <- sim_states[, c("ID", "id_split", "time", "time_s", "x", "y", "step",
                               "angle", "state", "sex")]

  usethis::use_data(sim_states, overwrite = TRUE, compress = "xz")
}
