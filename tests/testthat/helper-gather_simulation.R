# helper functions to establish a test suite
# with which to test functionality of
# gather_simulation.r


# generate a bunch of parameters for a
# putative output file that will be summarized
# later
generate_params <- function(delim=";",delim_two="",n_params)
{
  stopifnot(n_params >= 3)

  n_params_int <- n_params_float <- floor(n_params/3)

  n_params_text <- n_params - n_params_int - n_params_float

  # generate a bunch of non-sensical
  # some parameter values
  values_parameters <- list(
    rbinom(n=n_params_int,size=23,prob=0.5)
    ,sample(x=c("male","female"),size=n_params_text, replace=T)
    ,rnorm(n=n_params_float)
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
    ,n_cols=6 # number of dummy data columns
    ,csv2=FALSE # whether we use write.csv2 and associated settings
    ,quote=FALSE # whether we quote character strings y/n
    ,delim=";" # the delimiter used to write the thing
    ,n_params=10
    ) {
  # set bounds on the number of columns
  if (n_cols <= 1 )  {
    n_cols <- n_cols + 2 # minimum of 2
  } else if (n_cols > length(letters))  {
    n_cols <- length(letters) # max of 23
  }

  # make 1 integer col
  integer.col <- seq(1,numgen,1)

  # make the rest of the columns floating point
  floating.point.cols <- matrix(
    data = rnorm(n=(n_cols - 1)* numgen)
    ,nrow=numgen
    ,ncol= n_cols - 1)

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

  write.table(x=the.data
            ,file=textconn
            ,row.names=F
            ,quote=quote
            ,sep=delim)

  # append parameters to the string reflecting the csv
  the_csv_string <- paste0(paste0(tempfile,collapse="\n"),"\n","\n")


  close(textconn)

  # now generate the parameters
  parameter_string <- generate_params(
    delim = delim
    ,n_params=n_params)

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
  if (length(x) == 0)
  {
    return(0)
  }

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
    ,dir_name_prefix=NULL)
{

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

  if (length(all_dirs) <= 0)
  {
    return
  }

  # now sort dirs based on number of slashes, so that we
  # the most nested dirs first and delete them

  # sort list of dirs according to slash count
  all_dirs_sorted <- all_dirs[rev(order(num_slashes(all_dirs)))]

  # now go through all dirs and remove the final one
  for (dir_i in all_dirs_sorted)
  {
    file.remove(all_dirs_sorted)
  }
} # end remove_files()

# make a bunch of files of the normal type we use
make_bunch_of_files <- function(
    n_files=10
    ,file_name_prefix = "sim_test_"
    ,dir_name_prefix = "dir_sim_test_"
    ,envt=parent.frame()
    ,nested_dirs = F
    ,delim=";"
    ,n_cols=10
    ,n_params=10
)
{
  # if nested dirs, we need a minimum of three files to put
  # in the various directories
  if (nested_dirs) {
    stopifnot(n_files >= 3)
  }

  # first remove any previously generated test files
  remove_files(
    file_name_prefix
    ,dir_name_prefix)

  # if no weird nesting just make a whole bunch of different files
  if (!nested_dirs)
  {
    for (file_i in 1:n_files)
    {
      # create a file name
      date_format = format(x=Sys.time(),"%Y_%m_%d_%H%M%S")
      file_name = paste0(file_name_prefix,date_format,"_",file_i)

      # create the file and move on
      make_single_file(file_name = file_name
                       ,envt = envt
                       ,delim=delim
                       ,n_cols=n_cols
                       ,n_params=n_params)
    }
  } else { # ok nested dir

    # number of files per folder
    n_files_per_folder <- c(NA,NA,NA)

    # folders 1,2 get 1/3 of the number
    n_files_per_folder[1] <- n_files_per_folder[2] <- floor(n_files/3)

    # folder 3 gets the remainder
    n_files_per_folder[3] <- n_files - n_files_per_folder[1] - n_files_per_folder[2]

    # we will make a few files in directory a1/
    # then a few files in directory a1/a2/
    # then a few files in directory a3/
    # and then a few in the top-level dir
    dirs <- c(
      paste0(dir_name_prefix,"_a1/",dir_name_prefix,"_a2/") # single nested dir
      ,paste0(dir_name_prefix,"_a3/"
      ,".")
              ) # single non-nested dir

    # make these directories
    for (dir_idx in 1:length(dirs))
    {
      if (dir_idx != ".")
      {
        dir.create(dirs[dir_idx], recursive=T)
      }

      # create a file name
      date_format = format(
        x=Sys.time()
        ,"%Y_%m_%d_%H%M%S")

      for (file_idx in 1:n[dir_idx])
      {
        file_name = paste0(dirs[dir_idx]
                           ,file_name_prefix
                           ,date_format
                           ,"_",file_idx
                           ,collapse="")

        # create the file and move on
        make_single_file(file_name = file_name
                         ,envt = envt
                         ,delim=delim
                         ,n_cols=n_cols
                         ,n_params=n_params)
      }
    } # end for dir_idx
  } # end else nested dir
} # make_bunch_of_files
