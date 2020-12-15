### R script to make your own batch file

row.2.yaml <- function(data.frame.row) {

    names_row <- names(data.frame.row)

    yaml_str = ""

    for (name_i in names_row){
        yaml_str = paste0(yaml_str
                         ,name_i
                         ,": "
                         ,data.frame.row[name_i]
                         ,"\n"
                        )
    }

    return(yaml_str)
}


# Auxiliary function to expand.grid() a parameter list if
# it is a list of values. However, if it is a data frame
# do nothing
expand_params <- function(parameter_list)
{
    # check whether parameter listing is of appropriate type
    if (class(parameter_list) == class(list())) {
        # combine all combinations of parameters
        all_parameters <- as.data.frame(expand.grid(parameter_list))
    } else if (class(parameter_list) ==class(data.frame())) {
        all_parameters <- parameter_list
    } else {
        stop("parameter_list should be a list or data.frame")
    }
    return(all_parameters)
}

#' Make yaml batch file
#'
#'
#' @param parameter_list a list or dataframe with all parameter combinations
#' the order with which a parameter was added to the list will be
#' maintained
#' @param executable_path the path to the simulation executable (i.e.,
#'  the exe file)
#' @param n_replicates the number of replicates you'd like to run
#' of each simulation
#' @param n_cores the number of cores available. If n_cores>1, simulations
#' can be run in parallel
#' @param batch_file_prefix the prefix of the resulting batch file to which
#' all the simulation commands will be written to
#' @param output_file_prefix the prefix of the resulting output file to which
#' each simulation will write its output.
#' @param yaml_file_prefix the prefix of each individual \code{.yaml} file that
#' iscontaining the parameters for a single simulation
#' @return This function returns \code{NULL}. However,
#' a file with name \code{batch_file_prefix} will be
#' written to disk. If the option \code{yaml=TRUE}, this will be accompanied
#' by one or more \code{.yaml} files.
#'
#' @description
#' This function produces a batch file named \code{batch_file_prefix.sh}
#' with the following contents:
#' \preformatted{
#' /path/to/executable.exe -f parameter_file1.yaml
#' /path/to/executable.exe -f parameter_file2.yaml
#' /path/to/executable.exe -f parameter_file3.yaml
#' }
#' where parameter_file1.yaml then contains:
#' \preformatted{
#' param_name1: 0.01
#' param_name2: 1
#' param_name3: 1
#' output_file: output_file_1.csv
#' }
#'
#' If this is all done, you can then run the batch file in any UNIX terminal
#' (e.g., Mac OS X terminal, MSYS2 on Windows) by giving the UNIX terminal the
#' command
#' \preformatted{
#' bash batch_file_prefix.sh
#' }
#' which causes all simulations to be run sequentially.
#'
#' @examples
#' #initalize a parameter object
#' parameter_object <- list()
#' parameter_object$z <- c(0.0,0.01)
#' parameter_object$b <- 0.1
#'
#' # call the batch file function
#' make_batch_file(parameter_list=parameter_object
#'                      ,executable_path="./my_simulation.exe")
#' # which produces a file named 'batch_file.sh' in the current directory
#' # containing:
#' # ./my_simulation.exe 0.0 0.1 output_file_1
#' # ./my_simulation.exe 0.001 0.1 output_file_2
#' # Note that the value of parameter z is
#' # printed first and then the value of parameter b, followed by the
#' # name of the output file for this simulation
#'
#'
#' @export
make.batch.file.yaml <- function(
        parameter_list
        ,executable_path
        ,n_replicates=1
        ,n_cores=1
        ,batch_file_prefix="batch_file"
        ,output_file_prefix="output"
        ,yaml_file_prefix="parameters"
        ,output_file_yaml_key="base_name"
        ) {

    all_parameters <- expand_params(parameter_list)

    # collect a count of all the parameters
    nrows <- nrow(all_parameters)

    # index number for output file of
    # each simulation
    file_idx <- 1

    list.batch.contents <- list()

    # loop through the replicates
    for (rep_idx in 1:n_replicates)
    {
        # loop through the rows of the combinations
        # dataframe
        for (row_idx in 1:nrows)
        {
            # generate the yaml file name for the
            # current simulation
            yaml_filename <- paste0(yaml_file_prefix,"_",file_idx,".yaml")

            # copy the corresponding row of the data frame
            # to a list object so that we can safely add a filename
            param_row_list <- all_parameters[row_idx,]

            # add filename to yaml file contents
            param_row_list[output_file_yaml_key] <- paste0(
                output_file_prefix
                ,"_"
                ,file_idx
                )

            # make yaml file contents
            yaml_file_contents <- row.2.yaml(
                data.frame.row = param_row_list)

            # write the jaml file
            writeLines(text=yaml_file_contents
                       ,con=yaml_filename)

            # now compile the command for the batch file
            # which is:
            #  - executable_path
            #  - f flag
            #  - filename
            params_concatenated <- paste(
                executable_path
                ,"-f"
                ,yaml_filename
                ,sep = " ")

            echo_msg <- paste0("echo ",file_idx)

            # append this line to the list with which we
            # will create a batch file
            list.batch.contents <- c(list.batch.contents
                                     ,list(echo_msg, params_concatenated))


            # update file index counter
            file_idx <- file_idx + 1


        } # for (row_idx in 1:nrows)
    } # for (rep_idx in 1:nrep)

    # write the batch file
    write(x=paste(list.batch.contents,collapse="\n")
            ,file=paste0(batch_file_prefix,".sh"))
} # end make.batch.file.yaml

#' Make batch file
#'
#'
#' @param parameter_list a list or dataframe with all parameter combinations
#' the order with which a parameter was added to the list will be
#' maintained
#' @param executable_path the path to the simulation executable (i.e.,
#'  the exe file)
#' @param n_replicates the number of replicates you'd like to run
#' of each simulation
#' @param n_cores the number of cores available. If n_cores>1, simulations
#' can be run in parallel
#' @param batch_file_prefix the prefix of the resulting batch file to which
#' all the simulation commands will be written to
#' @param output_file_prefix the prefix of the resulting output file to which
#' each simulation will write its output.
#' @param yaml if \code{yaml = TRUE}, simulations are fed \code{.yaml} files
#' (i.e., files written in the \code{yaml} format, see:
#' \href{https://yaml.org/spec/1.2/spec.html}{here}
#' ).
#' Hence, this function will write each parameter combination is
#' to a single \code{.yaml}
#' file. See parameter \code{yaml_file_prefix} for the prefix of each file.
#' If \code{yaml = FALSE}, simulations are fed parameters through the
#' command line (through the \code{argv} and \code{argc} parameters in C).
#' @param yaml_file_prefix the prefix of each individual \code{.yaml} file that is
#' containing the parameters for a single simulation
#' @return This function returns \code{NULL}. However,
#' a file with name \code{batch_file_prefix} will be
#' written to disk. If the option \code{yaml=TRUE}, this will be accompanied
#' by one or more \code{.yaml} files.
#'
#' @description
#' This function produces a batch file named \code{batch_file_prefix.sh}
#' with the following contents:
#' \preformatted{
#' /path/to/executable.exe 0.01 1 0 output_file_1.csv
#' /path/to/executable.exe 0.01 2 0 output_file_2.csv
#' /path/to/executable.exe 0.02 1 0 output_file_3.csv
#' }
#'
#' However, when the option \code{yaml=TRUE} the batch file
#' named \code{batch_file_prefix.sh} will contain:
#' \preformatted{
#' /path/to/executable.exe -f parameter_file1.yaml
#' /path/to/executable.exe -f parameter_file2.yaml
#' /path/to/executable.exe -f parameter_file3.yaml
#' }
#' where parameter_file1.yaml then contains:
#' \preformatted{
#' param_name1: 0.01
#' param_name2: 1
#' param_name3: 1
#' output_file: output_file_1.csv
#' }
#'
#' If this is all done, you can then run the batch file in any UNIX terminal
#' (e.g., Mac OS X terminal, MSYS2 on Windows) by giving the UNIX terminal the
#' command
#' \preformatted{
#' bash batch_file_prefix.sh
#' }
#' which causes all simulations to be run sequentially.
#'
#' @examples
#' #initalize a parameter object
#' parameter_object <- list()
#' parameter_object$z <- c(0.0,0.01)
#' parameter_object$b <- 0.1
#'
#' # call the batch file function
#' make_batch_file(parameter_list=parameter_object
#'                      ,executable_path="./my_simulation.exe")
#' # which produces a file named 'batch_file.sh' in the current directory
#' # containing:
#' # ./my_simulation.exe 0.0 0.1 output_file_1
#' # ./my_simulation.exe 0.001 0.1 output_file_2
#' # Note that the value of parameter z is
#' # printed first and then the value of parameter b, followed by the
#' # name of the output file for this simulation
#'
#'
#' @export
make.batch.file <- function(
        parameter_list
        ,executable_path
        ,n_replicates=1
        ,n_cores=1
        ,batch_file_prefix="batch_file"
        ,output_file_prefix="output"
        ) {

    # expand all parameters
    all_parameters <- expand_params(parameter_list)

    # collect a count of all the parameters
    nrows <- nrow(all_parameters)

    # index number for output file of
    # each simulation
    file_idx <- 1

    # variable to write the contents of the batch file to
    list.batch.contents <- list()

    # loop through the replicates
    for (rep_idx in 1:n_replicates)
    {
        # loop through the rows of the combinations
        # dataframe
        for (row_idx in 1:nrows)
        {
            # generate the output file name for the
            # current simulation
            filename <- paste(output_file_prefix,"_",file_idx,sep="")

            # update count of the file index counter
            file_idx <- file_idx + 1

            # first, concatenate all parameters
            # from the all_parameters data frame
            # put spaces between all columns
            params_concatenated <- paste(
                    all_parameters[row_idx,]
                    ,collapse=" ")

            # add an output file to the end
            # which we may or may not use
            params_concatenated <- paste(
                executable_path
                ,params_concatenated
                ,filename)

            # append this line to the list with which we
            # will create a batch file
            list.batch.contents <- c(list.batch.contents
                                     ,list(params_concatenated))
        } # for (row_idx in 1:nrows)
    } # for (rep_idx in 1:nrep)

    # write the batch file
    write(x=paste(list.batch.contents,collapse="\n")
            ,file=paste0(batch_file_prefix,".sh"))
} # end make_batch_file

# write a file summarizing the parameters that are varied
summarize.params <- function(...)
{
    # get all the arguments that are supplied to this function
    n.args <- ...length()

    # get all the names of the things that were
    # provided as arguments
    # apparently the first item is always ""
    # so we will skip that
    arg.names <- names(as.list(match.call()))
    arg.names <- arg.names[2:length(arg.names)]

    stopifnot(n.args == length(arg.names))

    output_summary <- ""

    for (i in 1:n.args)
    {
        output_summary <- paste0(output_summary
                ,arg.names[[i]]
                ,"="
                ,paste(...elt(i),collapse=" ")
                ,"\n"
                )
    }

    write(x=output_summary
            ,file=record_file_name)
}

#summarize.params(
#                exe=exe
#                ,max_time=max_time
#                ,p_good_init=p_good_init
#                ,kappa=kappa
#                ,bmax=bmax
#                ,c=c_resist
#                ,gamma_G=gamma_G
#                ,gamma_B=gamma_B
#                ,psi_G=psi_G
#                ,psi_B=psi_B
#                ,d=d
#                ,dB=dB
#                ,dG=dG
#                ,sigma=sigma
#                ,mu_x=mu_x
#                ,sdmu_x=sdmu_x
#                ,init_x=init_x
#                )
#
#
#
