# gathers all simulations in certain folder and puts in big dataframe


# collects parameters from a single file and returns them as a dataframe
collect.params <- function(filename
                           ,line_from
                           ,line_to = NA
                           ,sep=";")
{
    raw.params <- read.table(filename
            ,header= F
            ,sep=sep
            ,skip=line_from
            ,nrow=line_to
            ,stringsAsFactors=F)

    p = as.data.frame(t(raw.params$V2), stringsAsFactors=F)
    colnames(p) <- raw.params$V1
}


# find out where the parameter listing starts and ends
patterns2lines <- function(
    filename
    ,pattern_from
    ,pattern_to
    ) {

    f <- readLines(filename)

    line_from <- NA
    line_to <- NA

    # make a sequence of the lines...
    seqq <- seq(1,length(f),1)

    found_parameters <- F

    # go through each line in the data file and find first line
    # where data is printed (i.e., a line which starts with a digit)
    for (line_i in seqq)
    {
        # if you are not yet in the parameter listing
        # and you find the start of the parameter listing
        if (!found_parameters && length(grep(pattern_from, f[[line_i]])) > 0)
        {
            found_parameters <- T
            line_from <- line_i

            # if there is no end pattern to the parameter listing
            # just quit
            if (is.na(pattern_to))
            {
                line_to <- length(f)
                break
            }
        } else if (found_parameters && length(grep(pattern_to,f[[line_i]] > 0)))
        {
            line_to <- line_i
        }

    }

    return(c(line_from, line_to))
} # end function find_out_param_line()



#' if you have a bunch of simulation files in a directory
#' \code{example_dir}, the function
#' \code{summarize.sims(simulations_path=example_dir,...)}
#' goes through all files in that directory
#' The function returns a data.frame, each row of which contains the
#' parameters, filename and the last generation of the output of a single
#' simulation.

summarize.sims <- function(simulations_path
                           ,parameter_start_pattern="^var"
                           ,parameter_end_pattern=NA
                           ,data_start_pattern="^generation"
                           ,data_end_pattern="^\n"
                           ,sep=";"
                           )
{
    # get a list of all the simulation files
    all.simulation.files <- list.files(
            path="."
            ,pattern="sim_.*")

    # minimum number of lines you want from each simulation
    # (counting from the end that is)
    # if you want the last line from each sim: 1
    # if you want the last 20 lines from each sim: 20
    # if you want everything

    # place holder variable for a big
    # data frame with all simulations
    big.dataframe.all.sims <- NULL

    for (file_i in all.simulation.files)
    {
        # filename might be a factor so let's change it to character
        file_i_chr <- as.character(file_i)

        param.lines <- patterns2lines(
               filename=file_i_chr
               ,pattern_from = parameter_start_pattern
               ,pattern_to = parameter_end_pattern
            )

        parameters <- collect.params(
                       filename=file_i_chr
                       ,param.lines[[1]]
                       ,param.lines[[2]]
                       ,sep=sep)

        data.lines <- patterns2lines(
               filename=file_i_chr
               ,pattern_from = data_start_pattern
               ,pattern_to = data_end_pattern)

        the.data <- read.table(
            file=file_i_chr
            ,header=T
            ,skip=data.lines[[1]]
            ,nrow=data.lines[[2]]
            ,sep=";")

        total.data <- cbind(parameters,the.data)

        big.dataframe.all.sims <- rbind(
            big.dataframe.all.sims
            ,total.data)

    } # end for

    return(big.dataframe.all.sims)
} # end summarize.sims()



