test_that("link.arp adds linked clusters and graph information", {
  res <- run_pipeline()$linked
  expect_s3_class(res, "disperseR")
  expect_true(all(c("linked_clusters", "arp_link") %in% names(res$data)))
  expect_named(res$cluster_graphs, c("F02", "F04", "F05", "M01", "M03"), ignore.order = TRUE)
  expect_s3_class(res$cluster_graphs$M01$g, "igraph")
  # The two patches of M01 and F04 are linked into one ARP; M03 and F05 have no links
  # Los dos parches de M01 y F04 quedan enlazados en un ARP; M03 y F05 no tienen enlaces
  arp_rows <- res$data[res$data$arp, ]
  n_linked <- tapply(arp_rows$linked_clusters, arp_rows$ID, function(x) length(unique(x)))
  expect_equal(as.vector(n_linked[c("M01", "F04", "F02", "M03", "F05")]), c(2, 1, 4, 2, 1))
  expect_true(any(res$data$arp_link[res$data$ID == "M01"]))
  expect_true(any(res$data$arp_link[res$data$ID == "F04"]))
  expect_false(any(res$data$arp_link[res$data$ID %in% c("M03", "F05")]))
  expect_false(any(c(".time_sec", "id_x_field", "mov_seq") %in% names(res$data)))
  expect_equal(nrow(res$data), nrow(sim_states))
})

test_that("link.arp checks its input", {
  expect_error(link.arp(sim_states), "class 'disperseR'")
})
