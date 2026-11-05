mov <- run_pipeline()$mov

test_that("phase.summary returns one row per phase", {
  ph <- phase.summary(mov)
  expect_s3_class(ph, "data.frame")
  expect_named(ph, c("ID", "n_phase", "phase", "arp", "from", "to", "start", "end",
                     "duration_days", "n_locations", "distance_km", "displacement_km"))
  # Same sequence of phases as the data / Misma secuencia de fases que los datos
  for (id in unique(ph$ID)) {
    expect_equal(ph$phase[ph$ID == id], phase_sequence(mov, id))
  }
  expect_equal(sum(ph$n_locations), nrow(mov$data))
  expect_true(all(ph$distance_km >= ph$displacement_km - 1e-8))
  # Movements between ARPs know where they start and end
  m01 <- ph[ph$ID == "M01" & ph$phase == "Type 3", ]
  expect_equal(m01$to, ph$arp[ph$ID == "M01" & ph$phase == "ARP"][1])
  expect_true(all(is.na(ph$from[ph$phase == "Type 0"])))
  expect_error(phase.summary(sim_states), "Run class.mov")
})

test_that("draw.track and draw.timeline return ggplot objects", {
  expect_s3_class(draw.track(sim_tracks, time.col = "timestamp"), "ggplot")
  expect_s3_class(draw.track(mov, colour.col = "phase", km = TRUE), "ggplot")
  expect_s3_class(draw.track(mov, colour.col = "phase", ids = "M01", colour.path = TRUE),
                  "ggplot")
  expect_s3_class(draw.track(sim_states, colour.col = "step"), "ggplot")
  expect_s3_class(draw.timeline(mov), "ggplot")
  expect_s3_class(draw.timeline(sim_states, phase.col = "state"), "ggplot")
  expect_error(draw.track(sim_tracks, colour.col = "nope", time.col = "timestamp"), "not found")
  expect_error(draw.track(mov, ids = "nobody"), "None of the selected")
  expect_error(draw.timeline(sim_tracks), "not found")
})

test_that("print.disperseR summarises the object", {
  out <- utils::capture.output(print(mov))
  expect_match(out[1], "disperseR object")
  expect_true(any(grepl("Movement phases", out)))
  expect_invisible(print(mov))
})
