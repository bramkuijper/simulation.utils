### R script to make your own batch file

#' Make batch file
#'
#' This function produces a batch file
#' in the following format:
#' /path/to/executable.exe 0.01 1 0 output_file_1.csv
#' /path/to/executable.exe 0.01 2 0 output_file_2.csv
#' /path/to/executable.exe 0.02 1 0 output_file_3.csv
#' etc
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
#' all the output will be written to
#'
#' @return nothing, but a file with the path batch_file_prefix will be
#' written to disk
#'
#' @examples:
#' #initalize parameter object
#' parameter_object <- list()
#' parameter_object$a <- c(0.0,0.01)
#' parameter_object$b <- 0.1
#'
#' make_batch_file(parameter_list=parameter_object
#'                      ,executable_path="./my_simulation.exe")
#' # which produces a file in the current directory
#' # called 'batch_file.sh'
#' # with the contents
#' # ./my_simulation.exe 0.0 0.1 output_file_1
#' # ./my_simulation.exe 0.001 0.1 output_file_1

#' @export
make.batch.file <- function(
        parameter_list
        ,executable_path
        ,n_replicates=1
        ,n_cores=1
        ,batch_file_prefix="batch_file") {

    # combine all combinations of parameters
    all_parameters <- as.data.frame(expand.grid(parameter_object))

    # collect a count of all the parameters
    nrows <- nrow(all_parameters)

    # index number for output file of
    # each simulation
    file_idx <- 1

    # variable to write the contents of the batch file to
    list.batch.contents <- list()

    # loop through the replicates
    for (rep_idx in 1:nrep)
    {
        # loop through the rows of the combinations
        # dataframe
        for (row_idx in 1:nrows)
        {
            # generate the output file name for the
            # current simulation
            filename <- paste(basename,"_",file_idx,sep="")

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
            params_concatenated <- paste(params_concatenated,filename)

            # append this line to the list with which we
            # will create a batch file
            batch_file_contents <- c(batch_file_contents
                                     ,list(params_concatenated))
        } # for (row_idx in 1:nrows)
    } # for (rep_idx in 1:nrep)

    # write the batch file
    write(x=paste(batch_file_contents,collapse="\n")
            ,file=batch_file_name)
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
