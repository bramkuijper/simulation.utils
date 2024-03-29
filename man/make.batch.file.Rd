% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_parameters.r
\name{make.batch.file}
\alias{make.batch.file}
\title{Make batch file}
\usage{
make.batch.file(
  parameter_list,
  executable_path,
  n_replicates = 1,
  n_cores = 1,
  batch_file_prefix = "batch_file",
  output_file_prefix = "output"
)
}
\arguments{
\item{parameter_list}{a list or dataframe with all parameter combinations
the order with which a parameter was added to the list will be
maintained}

\item{executable_path}{the path to the simulation executable (i.e.,
the exe file)}

\item{n_replicates}{the number of replicates you'd like to run
of each simulation}

\item{n_cores}{the number of cores available. If n_cores>1, simulations
can be run in parallel}

\item{batch_file_prefix}{the prefix of the resulting batch file to which
all the simulation commands will be written to}

\item{output_file_prefix}{the prefix of the resulting output file to which
each simulation will write its output.}

\item{yaml}{if \code{yaml = TRUE}, simulations are fed \code{.yaml} files
(i.e., files written in the \code{yaml} format, see:
\href{https://yaml.org/spec/1.2/spec.html}{here}
).
Hence, this function will write each parameter combination is
to a single \code{.yaml}
file. See parameter \code{yaml_file_prefix} for the prefix of each file.
If \code{yaml = FALSE}, simulations are fed parameters through the
command line (through the \code{argv} and \code{argc} parameters in C).}

\item{yaml_file_prefix}{the prefix of each individual \code{.yaml} file that is
containing the parameters for a single simulation}
}
\value{
This function returns \code{NULL}. However,
a file with name \code{batch_file_prefix} will be
written to disk. If the option \code{yaml=TRUE}, this will be accompanied
by one or more \code{.yaml} files.
}
\description{
This function produces a batch file named \code{batch_file_prefix.sh}
with the following contents:
\preformatted{
/path/to/executable.exe 0.01 1 0 output_file_1.csv
/path/to/executable.exe 0.01 2 0 output_file_2.csv
/path/to/executable.exe 0.02 1 0 output_file_3.csv
}

However, when the option \code{yaml=TRUE} the batch file
named \code{batch_file_prefix.sh} will contain:
\preformatted{
/path/to/executable.exe -f parameter_file1.yaml
/path/to/executable.exe -f parameter_file2.yaml
/path/to/executable.exe -f parameter_file3.yaml
}
where parameter_file1.yaml then contains:
\preformatted{
param_name1: 0.01
param_name2: 1
param_name3: 1
output_file: output_file_1.csv
}

If this is all done, you can then run the batch file in any UNIX terminal
(e.g., Mac OS X terminal, MSYS2 on Windows) by giving the UNIX terminal the
command
\preformatted{
bash batch_file_prefix.sh
}
which causes all simulations to be run sequentially.
}
\examples{
#initalize a parameter object
parameter_object <- list()
parameter_object$z <- c(0.0,0.01)
parameter_object$b <- 0.1

# call the batch file function
make.batch.file(parameter_list=parameter_object
                     ,executable_path="./my_simulation.exe")
# which produces a file named 'batch_file.sh' in the current directory
# containing:
# ./my_simulation.exe 0.0 0.1 output_file_1
# ./my_simulation.exe 0.001 0.1 output_file_2
# Note that the value of parameter z is
# printed first and then the value of parameter b, followed by the
# name of the output file for this simulation


}
