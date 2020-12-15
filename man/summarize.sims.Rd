% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gather_simulation.r
\name{summarize.sims}
\alias{summarize.sims}
\title{if you have a bunch of simulation files in a directory
\code{example_dir}, the function
\code{summarize.sims(simulations_path=example_dir,...)}
goes through all files in that directory
The function returns a data.frame, each row of which contains the
parameters, filename and the last generation of the output of a single
simulation.}
\usage{
summarize.sims(
  simulations_path,
  simulation_file_pattern = "sim_.*",
  parameter_start_pattern = "^var",
  parameter_end_pattern = NA,
  data_start_pattern = "^generation",
  data_end_pattern = "^\\n",
  sep = ";",
  recursive = T
)
}
\description{
if you have a bunch of simulation files in a directory
\code{example_dir}, the function
\code{summarize.sims(simulations_path=example_dir,...)}
goes through all files in that directory
The function returns a data.frame, each row of which contains the
parameters, filename and the last generation of the output of a single
simulation.
}