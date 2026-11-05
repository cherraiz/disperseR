# Model fitting is slow: skipped on CRAN / El ajuste de modelos es lento: se omite en CRAN

test_that("fitHMM.wrap is exported and returns states 1 and 2", {
  expect_true("fitHMM.wrap" %in% getNamespaceExports("disperseR"))
  skip_on_cran()
  trk <- sim_tracks[sim_tracks$ID == "F02", c("ID", "x", "y")]
  utils::capture.output(hmm <- suppressMessages(fitHMM.wrap(
    trk, ids = "ID", mean.step.1 = 600, sd.step.1 = 400,
    mean.step.2 = 80, sd.step.2 = 50, ang.con.1 = 0.7, ang.con.2 = 0.1)))
  expect_s3_class(hmm, "disperseR")
  expect_setequal(unique(hmm$data$state), c(1, 2))
})

test_that("fitHMM.wrap refuses ids when an 'ID' column already exists", {
  d <- sim_states[, c("ID", "id_split", "time", "x", "y")]
  expect_error(fitHMM.wrap(d, ids = "id_split", mean.step.1 = 600, sd.step.1 = 400,
                           mean.step.2 = 80, sd.step.2 = 50,
                           ang.con.1 = 0.7, ang.con.2 = 0.1),
               "already has a column named 'ID'")
})

test_that("resample.ctmm returns a regular track and keeps static columns", {
  skip_on_cran()
  trk <- sim_tracks[sim_tracks$ID == "F02", ][1:100, ]
  reg <- suppressWarnings(resample.ctmm(trk, tint = 3600, proj.in = 25830,
                                        static.cols = "sex", verbose = FALSE))
  expect_true(all(c("ID", "x", "y", "time", "time_s", "sex") %in% names(reg)))
  expect_true(all(diff(reg$time_s) == 3600))
  expect_true(all(reg$sex == "female"))
})
