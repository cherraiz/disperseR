test_that("get.arp returns a disperseR object with ARP assignments", {
  res <- run_pipeline()$arp
  expect_s3_class(res, "disperseR")
  expect_named(res, c("data", "models", "cluster_summary", "arp_clusters"))
  expect_type(res$data$arp, "logical")
  expect_true(all(c("cluster_id", "time_s") %in% names(res$data)))
  # Simulation design: M01 and F04 use two nearby patches; stopovers are not ARPs
  # Diseño de la simulación: M01 y F04 usan dos parches cercanos; las paradas no son ARP
  n_arp <- sapply(res$arp_clusters, nrow)
  expect_equal(as.vector(n_arp[c("M03", "F05")]), c(2, 1))
  expect_true(all(n_arp[c("M01", "F04")] >= 2))
  expect_true(all(res$arp_clusters$M01$dur_days >= 2))
})

test_that("numeric IDs and per-individual epsilon work", {
  num <- sim_states
  num$ID <- match(num$ID, c("M01", "F02", "M03", "F04", "F05")) + 100
  eps <- setNames(as.list(rep(230, 5)), 101:105)
  res <- get.arp(num, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
  expect_setequal(names(res$arp_clusters), as.character(101:105))
  expect_equal(nrow(res$arp_clusters[["101"]]),
               nrow(get.arp(sim_states, epsilon = 230, delta.days = 2,
                            pct.state1 = 0.2)$arp_clusters$M01))
})

test_that("missing filters trigger warnings and bad inputs errors", {
  expect_warning(get.arp(sim_states, epsilon = 220, pct.state1 = 0.3), "delta.days")
  expect_warning(get.arp(sim_states, epsilon = 220, delta.days = 2), "pct.state1")
  expect_error(get.arp(sim_states, epsilon = list(M01 = 200, F02 = 200), delta.days = 2,
                       pct.state1 = 0.3), "Missing epsilon")
  expect_error(get.arp(sim_states[, setdiff(names(sim_states), "x")], epsilon = 220,
                       delta.days = 2, pct.state1 = 0.2), "'x' and 'y'")
})
