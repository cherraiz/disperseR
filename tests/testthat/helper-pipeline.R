# Shared pipeline on the simulated data / Flujo común sobre los datos simulados
run_pipeline <- function(data = sim_states, ...) {
  eps <- suppressMessages(get.epsilon(data))
  arp <- get.arp(data, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
  linked <- link.arp(arp, thres = 24 * 3600)
  list(eps = eps, arp = arp, linked = linked, mov = class.mov(linked, ...))
}

# Sequence of phases (run-length) for one individual / Secuencia de fases de un individuo
phase_sequence <- function(mov, id) {
  rle(mov$data$phase[mov$data$ID == id])$values
}
