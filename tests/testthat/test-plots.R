test_that("plotting functions return ggplot objects", {
  res <- run_pipeline()
  expect_s3_class(suppressMessages(draw.step.densities(sim_states, threshold = res$eps)),
                  "ggplot")
  expect_s3_class(suppressMessages(draw.step.densities(sim_states, ind = "M01", ids = "ID")),
                  "ggplot")
  expect_s3_class(draw.linked.clusters(res$linked), "ggplot")
  expect_s3_class(draw.linked.clusters(res$linked, ids = "M01", grid_spacing = 5000),
                  "ggplot")
})
