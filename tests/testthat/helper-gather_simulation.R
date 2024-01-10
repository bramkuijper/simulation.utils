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

  values_parameters <- unlist(values_parameters)

  # get names for the parameters
  names_parameters <- letters[1:length(values_parameters)]

  # add 'par' in front
  names_parameters <- sapply(
    X = names_parameters
    ,FUN=function(x){return(paste0("par.",x))}
    ,USE.NAMES = F)

  # some error checking
  stopifnot(length(names_parameters) == length(values_parameters))

  # return a parameter string
  param_str <- ""

  for (value_idx in 1:length(values_parameters))
  {
    param_str <- paste0(param_str
                        ,names_parameters[[value_idx]]
                        ,delim
                        ,values_parameters[[value_idx]]
                        ,delim_two
                        ,"\n"
                        ,collapse="")
  }

  return(param_str)
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
    data = rnorm(n=(ncols - 1)* numgen)
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
  names(the.data) <- letters[1:ncol(the.data)]

  # now generate the whole csv file
  # as a character string (because we still
  # need to append parameters to it)
  close(textConnection(object = "tempfile"
                             ,open ="w"))

  textconn <- textConnection(object = "tempfile"
                             ,open ="w")

  if (!csv2)
  {
    write.csv(x=the.data
              ,file=textconn
              ,row.names=F
              ,quote=F)
  } else {
    write.csv2(x=the.data
              ,file=textconn
               ,row.names=F
              ,quote=F)
  }

  # append parameters to the string reflecting the csv
  the_csv_string <- paste0(paste0(tempfile,collapse="\n"),"\n","\n")


  close(textconn)

  # now generate the parameters
  parameter_string <- generate_params(delim = ifelse(csv2,";",","))

  # generate the whole file by combining csv file
  # and the parameters
  the_csv_string <- paste0(the_csv_string, paste0(parameter_string))

  # write the whole string to a file
  writeLines(text = the_csv_string
             ,con = withr::local_file(.file = file_name, .local_envir = envt)
             )

} # end make_single_file()

# helper function that returns number of slashes
# in a string (needed to sort directory depth)
num_slashes <- function(x)
{
  n_slash <- gregexpr(pattern="\\/", text=x)[[1]]

  # if it is not found first entry should have
  # been -1,
  if (n_slash[1] == -1)
  {
    return (0)
  }

  # it is found we should have a list with starting
  # indices, return that list
  return(length(n_slash))
}

# helper function to remove previously generated files during multiple tests
remove_files <- function(
    file_name_prefix
    ,dir_name_prefix=NULL) {

  # remove all test files in their
  all_files <- list.files(
                          path = "."
                          ,recursive = T
                          ,pattern = paste0(file_name_prefix,".*")
                          )

  file.remove(all_files)

  # now remove directories
  if (is.null(dir_name_prefix))
  {
    return
  }

  # get all dirs
  all_dirs <- list.files(
    path="."
    ,recursive=T
    ,include.dirs=T
    ,pattern = paste0(dir_name_prefix,".*")
  )

  # now sort dirs based on number of slashes, so that we
  # the most nested dirs first and delete them

  # sort list of dirs according to slash count
  all_dirs_sorted <- all_dirs(rev(order(num_slashes(all_dirs))))

  # now go through all dirs and remove the final one
  for (dir_i in all_dirs_sorted)
  {
    file.remove(all_dirs_sorted)
  }
} # end remove_files()

# make a bunch of files of the normal type we use
make_bunch_of_files_normal_type <- function(
    n_files=10
    ,file_name_prefix = "sim_test_"
    ,dir_name_prefix = "dir_sim_test_"
    ,envt=parent.frame()
    ,nested_dirs = F
)
{
  # if nested dirs, we need a minimum of three files to put
  # in the various directories
  if (nested_dirs) {
    stopifnot(n_files >= 3)
  }

  # first remove any previously generated test files
  remove_files(file_name_prefix, dir_name_prefix)

  # if no weird nesting just make a whole bunch of different files
  if (!nested_dirs)
  {
    for (file_i in 1:n_files)
    {
      # create a file name
      date_format = format(x=Sys.time(),"%Y_%m_%d_%H%M%S")
      file_name = paste0(file_name_prefix,date_format,"_",file_i)

      # create the file and move on
      make_single_file(file_name = file_name, envt = envt)
    }
  } else { # ok nested dir


    n1 <- n2 <- floor(n_files/3)

    # we will make a few files in directory a1/
    # then a few files in directory a1/a2/
    # then a few files in directory a3/
    # and then a few in the top-level dir
    dirs <- c("dtest_a1/test_a2/","dtest_a3",".")

    for (dir_i in dirs)
    {
      dir.create(nested_dir_name, recursive=T)

    # create a file name
    date_format = format(
      x=Sys.time()
      ,"%Y_%m_%d_%H%M%S")

    for (file_i in 1:n1)
    {
      file_name = paste0(nested_dir_name
                         ,file_name_prefix
                         ,date_format
                         ,"_",file_i)

      # create the file and move on
      make_single_file(file_name = file_name, envt = envt)
    }

    dir.create("dir_test_a3/")
  }
} # make_bunch_of_files_normal_type
