test_that("tracks are split at gaps longer than gap.days", {
  df <- data.frame(
    ID = "a",
    time = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 1, 2, 10, 11) * 3600
  )
  out <- divide.track.by.gap(df, ids = "ID", time.col = "time", gap.days = 0.25)
  expect_equal(out$id_split, c("a_1", "a_1", "a_1", "a_2", "a_2"))
  expect_false(any(c("time_diff", "sequence", "gap_val") %in% names(out)))
})

test_that("gap.days can be given per individual", {
  df <- data.frame(
    ID = rep(c("a", "b"), each = 3),
    time = as.POSIXct("2024-01-01", tz = "UTC") + rep(c(0, 5, 6), 2) * 3600
  )
  out <- divide.track.by.gap(df, "ID", "time", gap.days = list(a = 0.1, b = 1))
  expect_equal(out$id_split, c("a_1", "a_2", "a_2", "b_1", "b_1", "b_1"))
})

test_that("invalid inputs are rejected", {
  expect_error(divide.track.by.gap(sim_tracks, "ID", "timestamp", gap.days = -1))
  expect_error(divide.track.by.gap(sim_tracks, "nope", "timestamp", gap.days = 1))
  expect_error(divide.track.by.gap(sim_tracks, "ID", "timestamp", gap.days = list(M01 = 1)),
               "Missing gap.days")
})
