# helper functions to establish a test suite
# with which to test functionality of
# gather_simulation.r


# generate a bunch of parameters for a
# putative output file that will be summarized
# later
generate_params <- function(delim=";",delim_two="")
{
  # generate a bunch of non-sensical
  # some parameter values
  values_parameters <- list(
    rbinom(n=5,size=23,prob=0.5)
    ,sample(x=c("male","female"),size=1)
    ,rnorm(n=3)

  )

  # get names for the parameters
  names_parameters <- letters[1:length(values_parameters)]

  # add 'par' in front
  names_parameters <- sapply(
    X = names_paremeters
    ,FUN=function(x){return(paste0("par.",x))}
    ,USE.NAMES = F)

  # some error checking
  stopifnot(length(names_parameters) == length(values))

  # return a parameter string
  param_str <- ""

  for (value_idx in length(values))
  {
    param_str <- paste0(param_str
                        ,delim
                        ,names_parameters[[value_idx]]
                        ,delim_two
                        ,"\n")
  }

} # end generate_params()

# generate a single data file
make_single_file <- function(
    file_name
    ,envt= parent.frame() # see https://testthat.r-lib.org/articles/test-fixtures.html#case-study-usethis
    ,numgen=100 # number of generations / time steps in the file
    ,ncols=6 # number of dummy data columns
    ,csv2=FALSE # whether we use write.csv2 and associated settings
    ,quote=FALSE # whether we quote character strings y/n
    )
{
  # set bounds on the number of columns
  if (ncols <= 1 )  {
    ncols <- ncols + 2 # minimum of 2
  } else if (ncols > length(letters))  {
    ncols <- length(letters) # max of 23
  }

  # make 1 integer col
  integer.col <- seq(1,numgen,1)

  # make the rest of the columns floating point
  floating.point.cols <- matrix(
    data = rnorm(n=ncols * numgen)
    ,nrow=numgen
    ,ncol= ncols - 1)

  # get the integer and floating point data
  # all together in a data.frame
  the.data <- as.data.frame(
    cbind(integer.col,
          floating.point.cols
          )
    )
  # give the data frame simple column names
  names(the.data) <- letters(ncol(the.data))

  # now generate the whole csv file
  # as a character string (because we still
  # need to append parameters to it)

  textconn = "tempfile"

  if (!csv2)
  {
    write.csv(x=the.data
              ,file=textConnection(textconn,"w")
              ,row.names=F
              ,quote=F)
  } else
  {
    write.csv2(x=the.data
              ,file=textConnection(textconn,"w")
               ,row.names=F
              ,quote=F)
  }

  # now generate the parameters
  parameter_string <- generate_params()

  # append parameters to the string reflecting the csv
  the_csv_string <- paste(textconn,"\n","\n",parameter_string)

  # write the whole string to a file
  writeLines(text = the_csv_string
             ,con = withr::local_file(.file = file_name, .local_envir = envt)
             )
} # end make_single_file()

# make a bunch of files of the normal type we use
make_bunch_of_files_normal_type <- function(
    n_files
    ,file_name_prefix = "sim_test_"
    ,envt=parent.frame()
    ,nested_dirs = F
)
{
  # if no weird nesting just make a whole bunch of different files
  if (!nested_dirs)
  {
    for (file_i in 1:n_files)
    {
      # create a file name
      date_format = format(x=Sys.Time(),"%Y_%m_%d_%H%M%S")
      file_name = paste0(file_name_prefix,date_format,"_",file_i)

      # create the file and move on
      make_single_file(file_name = file_name, envt = envt)
    }
  } else { # ok nested dir
    # we will make a few files in directory a1/
    # then a few files in directory a2/
    # and then a few in the top-level dir
    create_directory()


  }
} # make_bunch_of_files_normal_type
