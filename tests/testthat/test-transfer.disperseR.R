mov <- run_pipeline()$mov

test_that("all methods return one row per base row", {
  for (m in c("temporal", "spatial", "forward", "backward", "movement", "arp")) {
    out <- suppressMessages(transfer.disperseR(sim_tracks, mov, id.col = "ID",
                                               time.col = "timestamp", method = m))
    expect_equal(nrow(out), nrow(sim_tracks))
    expect_true(all(c("phase", "arp", "linked_clusters", "time_diff") %in% names(out)))
  }
})

test_that("movement/arp methods handle ARP transitions without warnings", {
  expect_no_warning(suppressMessages(
    transfer.disperseR(sim_tracks, mov, id.col = "ID", time.col = "timestamp",
                       method = "movement")))
  expect_no_warning(suppressMessages(
    transfer.disperseR(sim_tracks, mov, id.col = "ID", time.col = "timestamp",
                       method = "arp")))
})

test_that("max.diff sets distant matches to NA", {
  out <- suppressMessages(transfer.disperseR(sim_tracks, mov, id.col = "ID",
                                             time.col = "timestamp", max.diff = 1))
  expect_true(all(is.na(out$phase[out$time_diff > 1])))
})

test_that("invalid inputs are rejected", {
  expect_error(transfer.disperseR(sim_tracks, mov, id.col = "ID",
                                  time.col = "timestamp", method = "foo"), "Invalid method")
  expect_error(transfer.disperseR(sim_tracks, sim_states), "class 'disperseR'")
})
