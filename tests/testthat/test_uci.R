context("uci")

# load data
data_dir <- system.file("extdata", package = "uci")
grid <- readRDS(file.path(data_dir, "grid_bho.rds"))

tester <- function(sf_object = grid,
                   var_name = 'jobs',
                   dist_type = 'euclidean',
                   bootstrap_border = FALSE,
                   showProgress = TRUE,
                   parallel = FALSE) {
  uci(sf_object,
      var_name,
      dist_type,
      bootstrap_border,
      showProgress,
      parallel)
}


# Expected behavior  -----------------------
test_that("expected behavior", {

  result <- tester()
  testthat::expect_is(result, "data.frame")

  result <- tester(dist_type = 'spatial_link')
  testthat::expect_is(result, "data.frame")
  
  result <- tester(parallel = TRUE)
  testthat::expect_is(result, "data.frame")
  
  result <- tester(bootstrap_border = TRUE, showProgress = TRUE)
  testthat::expect_is(result, "data.frame")
  
  result <- tester(bootstrap_border = TRUE, showProgress = FALSE)
  testthat::expect_is(result, "data.frame")
  
})



# ERRORS and messages  -----------------------
test_that("raises errors due to incorrect input", {

  testthat::expect_error(tester(sf_object='banana'))
  testthat::expect_error(tester(var_name = 'banana'))
  testthat::expect_error(tester(dist_type = 'banana'))
  testthat::expect_error(tester(dist_type = 123))
  testthat::expect_error(tester(bootstrap_border = 'banana'))
  testthat::expect_error(tester(showProgress = 'banana'))
  testthat::expect_error(tester(parallel = 'banana'))
  
})
