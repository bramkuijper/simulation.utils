# test the gather_simulation functionality


test_that("summarize.sims() finds correct number of files in non-nested dirs", {

  n_param <- 10
  # make a bunch of simulated files to summarize
  make_bunch_of_files(nested_dirs=F
    ,n_files=20
    ,n_cols=10
    ,n_params=n_param)

  summary_data <- summarize.sims(
    simulations_path="."
    ,sep=";"
    ,parameter_start_pattern="par.a"
    ,parameter_end_pattern=NA
    ,data_start_pattern = "a"
    ,data_end_pattern = "^$"
    )

  expect_equal(nrow(summary_data), 20)
})

test_that("summarize.sims() finds correct number of columns in non-nested dirs", {

  n_param <- 10
  # make a bunch of simulated files to summarize
  make_bunch_of_files(nested_dirs=F
    ,n_files=20
    ,n_cols=10
    ,n_params=n_param)

  summary_data <- summarize.sims(
    simulations_path="."
    ,sep=";"
    ,parameter_start_pattern="par.a"
    ,parameter_end_pattern=NA
    ,data_start_pattern = "a"
    ,data_end_pattern = "^$"
    )

  expect_equal(ncol(summary_data), 20)
})

test_that("summarize.sims() finds correct final row", {

  n_param <- 10
  # make a bunch of simulated files to summarize
  make_bunch_of_files(nested_dirs=F
    ,n_files=20
    ,n_cols=10
    ,n_params=n_param)

  summary_data <- summarize.sims(
    simulations_path="."
    ,sep=";"
    ,parameter_start_pattern="par.a"
    ,parameter_end_pattern=NA
    ,data_start_pattern = "a"
    ,data_end_pattern = "^$"
    )

  # first column should be 100 time steps
  expect_equal(summary_data[1,"a"],100)
})

test_that("summarize.sims() finds correct first parameter", {

  n_param <- 10
  # make a bunch of simulated files to summarize
  make_bunch_of_files(nested_dirs=F
    ,n_files=20
    ,n_cols=10
    ,n_params=n_param)

  summary_data <- summarize.sims(
    simulations_path="."
    ,sep=";"
    ,parameter_start_pattern="par.a"
    ,parameter_end_pattern=NA
    ,data_start_pattern = "a"
    ,data_end_pattern = "^$"
    )

  # check whether first parameter is in there
  expect_equal("par.a" %in% names(summary_data),T)
})


test_that("summarize.sims() finds correct final parameter", {

  n_param <- 10
  # make a bunch of simulated files to summarize
  make_bunch_of_files(nested_dirs=F
    ,n_files=20
    ,n_cols=10
    ,n_params=n_param)

  summary_data <- summarize.sims(
    simulations_path="."
    ,sep=";"
    ,parameter_start_pattern="par.a"
    ,parameter_end_pattern=NA
    ,data_start_pattern = "a"
    ,data_end_pattern = "^$"
    )

  # check whether final parameter is in there
  expect_equal(paste0("par",letters[n_param]) %in% names(summary_data),T)
})
