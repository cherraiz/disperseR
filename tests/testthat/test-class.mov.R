test_that("class.mov recovers the simulated movement sequence", {
  mov <- run_pipeline()$mov
  expect_s3_class(mov, "disperseR")
  expect_equal(phase_sequence(mov, "M01"),
               c("Type 0", "ARP", "Type 2", "ARP", "Type 1", "ARP", "Type 3", "ARP"))
  expect_equal(phase_sequence(mov, "F02"),
               c("Type 0", "ARP", "Type 2", "ARP", "Type 2", "ARP", "Type 2", "ARP"))
  expect_equal(phase_sequence(mov, "M03"), c("Type 0", "ARP", "Type 2", "ARP"))
  expect_equal(phase_sequence(mov, "F04"),
               c("Type 0", "ARP", "Type 1", "ARP", "Type 1", "ARP"))
  # Tracking ends while F05 is moving / El seguimiento de F05 termina en movimiento
  expect_equal(phase_sequence(mov, "F05"), c("Type 0", "ARP", "Type 2 or Type 1"))
})

test_that("stopovers inside movements are not classified as ARPs", {
  mov <- run_pipeline()$mov
  # Simulated behaviour of each regular location, from the nearest raw fix
  # Comportamiento simulado de cada localización regular, de la localización bruta más cercana
  truth <- vapply(seq_len(nrow(mov$data)), function(i) {
    raw <- sim_tracks[sim_tracks$ID == mov$data$ID[i], ]
    raw$behaviour[which.min(abs(as.numeric(raw$timestamp) - mov$data$time_s[i]))]
  }, character(1))
  expect_gt(mean(mov$data$phase[truth == "stopover"] != "ARP"), 0.9)
})

test_that("establish and mcp options run and keep all rows", {
  linked <- run_pipeline()$linked
  est <- class.mov(linked, establish = TRUE, establish.days = 3)
  expect_equal(nrow(est$data), nrow(sim_states))
  # The 2.5-day stopover of F02 is not established / La parada de 2,5 días de F02 no se establece
  expect_equal(phase_sequence(est, "F02"),
               c("Type 0", "ARP", "Type 2", "ARP", "Type 2", "ARP"))
  expect_true(all(est$data$phase %in% c("ARP", paste("Type", 0:3), "Type 2 or Type 1")))
  mcp <- class.mov(linked, arp.links = "mcp")
  expect_equal(nrow(mcp$data), nrow(sim_states))
})

test_that("class.mov validates its input", {
  linked <- run_pipeline()$linked
  expect_error(class.mov(sim_states), "class 'disperseR'")
  expect_error(class.mov(linked, arp.links = "foo"))
  expect_error(class.mov(linked, establish = TRUE), "establish.days")
  expect_error(class.mov(class.mov(linked)), "already exists")
})

test_that("row order does not change the classification", {
  ref <- run_pipeline()$mov$data
  set.seed(1)
  shuffled <- run_pipeline(sim_states[sample(nrow(sim_states)), ])$mov$data
  key <- function(d) d[order(d$ID, d$time_s), c("ID", "time_s", "phase")]
  expect_equal(key(shuffled), key(ref), ignore_attr = TRUE)
})

# Minimal linked object built by hand / Objeto enlazado mínimo construido a mano
toy_linked <- function(arp, clusters) {
  n <- length(arp)
  obj <- list(data = data.frame(
    ID = "a",
    time_s = (seq_len(n) - 1) * 3600,
    arp = arp,
    linked_clusters = clusters,
    arp_link = FALSE
  ), models = NULL)
  class(obj) <- "disperseR"
  obj
}

test_that("single-point ARP blocks (duration 0) are handled", {
  # mov -> A (1 point) -> mov -> B (30 h) -> mov
  arp <- c(rep(FALSE, 3), TRUE, rep(FALSE, 3), rep(TRUE, 31), rep(FALSE, 2))
  cl  <- c(rep("mov_1", 3), "arp_1", rep("mov_3", 3), rep("arp_2", 31), rep("mov_5", 2))
  obj <- toy_linked(arp, cl)

  plain <- class.mov(obj)
  expect_equal(rle(plain$data$phase)$values,
               c("Type 0", "ARP", "Type 2", "ARP", "Type 2 or Type 1"))

  # The 1-point ARP never establishes; B (1.25 days) does with establish.days = 1
  est <- class.mov(obj, establish = TRUE, establish.days = 1)
  expect_false(anyNA(est$data$phase))
  expect_equal(est$data$phase[4], "Type 0")
  expect_true(all(est$data$phase[arp & cl == "arp_2"] == "ARP"))
})

test_that("establishment uses the exact ARP duration (as delta.days in get.arp)", {
  arp <- c(rep(FALSE, 2), rep(TRUE, 25), rep(FALSE, 2))  # ARP lasts exactly 1 day
  cl  <- c(rep("mov_1", 2), rep("arp_1", 25), rep("mov_3", 2))
  obj <- toy_linked(arp, cl)
  expect_true(all(class.mov(obj, establish = TRUE, establish.days = 1)$data$phase[arp] == "ARP"))
  expect_false(any(class.mov(obj, establish = TRUE, establish.days = 1.001)$data$phase[arp] == "ARP"))
})

test_that("establishment measures the whole stay of a linked ARP", {
  linked <- run_pipeline()$linked
  # Each visit to the two patches of M01 lasts < 3 days, the linked stay ~11 days
  # Cada visita a los dos parches de M01 dura < 3 días; la estancia enlazada ~11 días
  est <- class.mov(linked, establish = TRUE, establish.days = 3)
  expect_equal(phase_sequence(est, "M01"),
               c("Type 0", "ARP", "Type 2", "ARP", "Type 1", "ARP", "Type 3", "ARP"))
  expect_equal(phase_sequence(est, "F04"),
               c("Type 0", "ARP", "Type 1", "ARP", "Type 1", "ARP"))
})

test_that("without links, the stay is the single ARP block", {
  # A (1 day) -> link (2 h) -> B (1 day): linked stay of ~2.1 days
  arp <- c(FALSE, FALSE, rep(TRUE, 25), FALSE, FALSE, rep(TRUE, 25), FALSE)
  cl  <- c("mov_1", "mov_1", rep("arp_1", 25), "arp_1", "arp_1", rep("arp_1", 25), "mov_5")
  obj <- toy_linked(arp, cl)
  obj$data$arp_link <- c(rep(FALSE, 27), TRUE, TRUE, rep(FALSE, 26))
  expect_true(all(class.mov(obj, establish = TRUE, establish.days = 2)$data$phase[arp] == "ARP"))

  # Same blocks separated by an unlinked movement: each visit lasts 1 day
  obj$data$arp_link <- FALSE
  obj$data$linked_clusters[28:29] <- "mov_3"
  expect_false(any(class.mov(obj, establish = TRUE, establish.days = 2)$data$phase[arp] == "ARP"))
})
