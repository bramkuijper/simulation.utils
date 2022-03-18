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
            ,nrow=line_to-line_from
            ,stringsAsFactors=F)

    if (is.null(nrow(raw.params)) || nrow(raw.params) < 1) {
        print(paste("Cannot find any parameters between lines"
                   ,line_from
                   ,"and"
                   ,line_to
                   ,"in file"
                   ,filename
                   ,". Skipping."
                   ))

        return(NULL)
    }

    p = as.data.frame(t(raw.params$V2), stringsAsFactors=F)
    colnames(p) <- raw.params$V1

    return(p)
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

    found_first <- F

    # go through each line in the data file and find first line
    # where data is printed (i.e., a line which starts with a digit)
    for (line_i in seqq)
    {
        #        print(paste("line numbert: ", line_i))
        #        print(paste0("line contents: '",f[[line_i]],"'"))
        #        #        print(paste("grep result endline: ",grep(pattern="^$",f[[line_i]])))
        #        print(paste("found_first true: ",found_first))
        #        print(paste("found_first pattern: ",pattern_from))
        #        print(paste("grep pattern: ",length(grep(pattern=pattern_from, x=f[[line_i]]))))

#        if (!is.na(pattern_to))
#        {
#            print(paste("grep pattern end: ",length(grep(pattern=pattern_to, x=f[[line_i]]))))
#        }
        
        # if you are not yet in the parameter listing
        # and you find the start of the parameter listing
        if (!found_first && length(grep(pattern=pattern_from, x=f[[line_i]])) > 0)
        {
            found_first <- T
            line_from <- line_i

            # if there is no end pattern to the parameter listing
            # just quit
            if (is.na(pattern_to))
            {
                line_to <- length(f)
                break
            }
        } else if (found_first &&
                   length(grep(pattern=pattern_to,f[[line_i]]) > 0))
        {
            line_to <- line_i
            break
        }
    }

    return(c(line_from, line_to))
} # end function find_out_param_line()


#' Summarizes a collection of simulation files, by producing a
#' \code{data.frame}
#' each row of which contains the parameters and the data from the last
#' generation of a single simulation file
#'
#' @param simulations_path the directory in which all the simulations are
#' collected
#' @param simulation_file_pattern a \href{
#' https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
#' }{regular expression} that matches the simulation files
#' @param parameter_start_pattern a regular expression that matches the start
#' of the parameters
#' @param parameter_end_pattern a regular expression that matches the end
#' of the parameters. If parameters are at the end of the file anyway
#' one can just write \code{NA}
#' @param data_start_pattern a regular expression that matches the start
#' of the data output for each timestep or generation
#' @param data_end_pattern a regular expression that matches the end
#' of the data output for each timestep or generation
#' @param sep data separators, e.g., \code{","} or \code{"\t"}
#' @param recursive if \code{recursive = TRUE}, the search for simulation files
#' recurses into subdirectories. If \code{recursive = FALSE}, only the current
#' directory will be searched without descending into subdirectories
#'
#' @return A \code{data.frame} containing the parameters and last line of
#' output of each simulation file and the corresponding file name
#'
#' @examples
#' # say the current directory "."
#' # contains the following files:
#' # ├── file_x.csv
#' # ├── parameters.txt
#' # ├── sim_cue_integration_23_06_2020_095246_1.csv
#' # ├── sim_cue_integration_23_06_2020_095246_10.csv
#' # ├── sim_cue_integration_23_06_2020_095246_10_dist.csv
#' # ├── sim_cue_integration_23_06_2020_095246_11.csv
#' # ├── sim_cue_integration_23_06_2020_095246_11_dist.csv
#' #
#' # we need to obtain the files
#' # ├── sim_cue_integration_23_06_2020_095246_1.csv
#' # ├── sim_cue_integration_23_06_2020_095246_10.csv
#' # ├── sim_cue_integration_23_06_2020_095246_11.csv
#' #
#' # hence we provide a simulation_file_pattern = "sim_.*\\d\\.csv"
#' # which is a regular expression mating sim_ followed by a series
#' # of any characters .* finalized by a digit \\d followed by a
#' # dot \\. and csv
#' #
#' data <- summarize.sims(simulations_path="."
#'                            ,simulation_file_pattern="sim_.*\\d\\.csv"
#'                            ,parameter_start_pattern="^sigmoid"
#'                            ,parameter_end_pattern="^sigmoid"
#'                            )
#' # collects the output from the files in the data.frame data
#' # sim_cue_integration_23_06_2020_095246_1.csv
#' # sim_cue_integration_23_06_2020_095246_10.csv
#' # sim_cue_integration_23_06_2020_095246_11.csv
#'
#' # data will look like
#' str(data)
#' # 'data.frame':	150 obs. of  4 variables:
#' # generation: num 50000 500000 500000 ...
#' # var1: num 1 2 3 4 ...
#' # x: num 0.33 0.35 0.35...
#' # file: sim_cue_integration_23_06_2020_095246_1.csv ...
#' @export
summarize.sims <- function(simulations_path
                           ,simulation_file_pattern="sim_.*"
                           ,parameter_start_pattern="^var"
                           ,parameter_end_pattern=NA
                           ,data_start_pattern="^generation"
                           ,data_end_pattern="^\n"
                           ,sep=";"
                           ,recursive=T
                           )
{
    # get a list of all the simulation files
    all.simulation.files <- list.files(
            path=simulations_path
            ,pattern=simulation_file_pattern
            ,full.names=T
            ,recursive=recursive
            )

    # place holder variable for a big
    # data frame with all simulations
    big.dataframe.all.sims <- NA

    if (length(all.simulation.files) < 1) {
        return(NA)
    }

    for (i in 1:length(all.simulation.files))
    {
        file_i <- all.simulation.files[[i]]
        # filename might be a factor so let's change it to character
        file_i_chr <- as.character(file_i)

        print(paste0("processing file "
                     ,i
                     ," out of "
                     ,length(all.simulation.files)
                     ,": "
                     ,file_i
                     )
             )

        param.lines <- patterns2lines(
               filename=file_i_chr
               ,pattern_from = parameter_start_pattern
               ,pattern_to = parameter_end_pattern)

        if (is.na(param.lines[[1]])) {
            print(paste("cannot find a match for the pattern "
                        ,"parameter_start_pattern='"
                        ,parameter_start_pattern
                        ,"' in the file ",file_i
                        ,". Skipping this file."
                        ,sep=""
                 ))

            next
        }

        parameters <- collect.params(
                       filename=file_i_chr
                       ,line_from = param.lines[[1]]
                       ,line_to = param.lines[[2]]
                       ,sep=sep)


        if (is.null(parameters))
        {
            next 
        }

        data.lines <- patterns2lines(
               filename=file_i_chr
               ,pattern_from = data_start_pattern
               ,pattern_to = data_end_pattern)

        if (is.na(data.lines[[1]]))
        {
            data.lines[[1]] = 1
        }

        pos.last.line <-
            ifelse(test = is.na(data.lines[[2]])
                   ,yes = -1  # no end to data, write -1
                   # end to data, subtract lines to skip
                   # and subtract 1 additional line (because
                   # header)
                   ,no = data.lines[[2]] - data.lines[[1]] - 2)

        # read the actual data
        the.data <- read.table(
            file=file_i_chr
            ,header=T
            ,skip=data.lines[[1]]-1 # R suddenly counts from 0
            ,blank.lines.skip = T
            ,strip.white=T
            ,nrows=pos.last.line
            ,sep=";")

        # get last line of the data
        last.line <- the.data[nrow(the.data),]

        # now tie parameters, last line of data and filename together
        total.data <- cbind(
            parameters
            ,last.line
            ,list(file=file_i))

        if (class(total.data) != class(data.frame())) {
            stop(paste("Data returned from file "
                       ,file_i_chr
                       ," is not in a proper data.frame format."
                       ,sep=""))
        }

        if (class(big.dataframe.all.sims) != class(data.frame())) {
            # if the data frame is not existing yet,
            # just add total data to big.dataframe.all.sims
            big.dataframe.all.sims <- total.data
        } else {
            # otherwise append to data.frame
            big.dataframe.all.sims <- rbind(
                big.dataframe.all.sims
                ,total.data)
        }

    } # end for

    return(big.dataframe.all.sims)
} # end summarize.sims()



