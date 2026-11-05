test_that("global mode returns a single positive threshold", {
  eps <- suppressMessages(get.epsilon(sim_states))
  expect_length(eps, 1)
  expect_gt(eps, 0)
})

test_that("mean and median methods match base R", {
  expect_equal(suppressMessages(get.epsilon(sim_states, method = "median")),
               stats::median(sim_states$step, na.rm = TRUE))
  expect_equal(suppressMessages(get.epsilon(sim_states, method = "mean")),
               mean(sim_states$step, na.rm = TRUE))
})

test_that("individual mode returns a list named by ID, also for numeric IDs", {
  eps <- suppressMessages(get.epsilon(sim_states, mode = "individual", ids = "ID"))
  expect_setequal(names(eps), c("M01", "F02", "M03", "F04", "F05"))

  num <- sim_states
  num$ID <- match(num$ID, c("M01", "F02", "M03", "F04", "F05")) + 100
  eps_num <- suppressMessages(get.epsilon(num, mode = "individual", ids = "ID",
                                          method = "median"))
  expect_setequal(names(eps_num), as.character(101:105))
  expect_length(eps_num, 5)
})

test_that("invalid arguments are rejected", {
  expect_error(get.epsilon(sim_states, mode = "foo"))
  expect_error(get.epsilon(sim_states, method = "foo"))
  expect_error(get.epsilon(sim_states, mode = "individual"), "requires specifying")
  three <- sim_states
  three$state[1:10] <- 3L
  expect_error(get.epsilon(three), "exactly 2 unique states")
})
