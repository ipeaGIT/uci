context("uci")

# load data
data_dir <- system.file("extdata", package = "uci")
grid <- readRDS(file.path(data_dir, "grid_bho.rds"))

tester <- function(sf_object = grid,
                   var_name = 'jobs',
                   boostrap_border = FALSE,
                   showProgress = TRUE) {
  uci(sf_object,
      var_name,
      boostrap_border,
      showProgress)
}


# Expected behavior  -----------------------
test_that("expected behavior", {

  result <- tester()
  testthat::expect_is(result, "data.frame")

  result <- tester(boostrap_border = TRUE, showProgress = TRUE)
  testthat::expect_is(result, "data.frame")
  
  result <- tester(boostrap_border = TRUE, showProgress = FALSE)
  testthat::expect_is(result, "data.frame")
  
})



# ERRORS and messages  -----------------------
test_that("raises errors due to incorrect input", {

  testthat::expect_error(tester(sf_object='banana'))
  testthat::expect_error(tester(var_name = 'banana'))
  testthat::expect_error(tester(boostrap_border = 'banana'))
  testthat::expect_error(tester(showProgress = 'banana'))

})
