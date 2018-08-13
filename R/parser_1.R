#' [!] Parse a filename
#'
#' Parse a string with filename and return a dataframe with
#' extracted information.
#'
#' @param x A sting.
#'
#' @return A data frame.
#' @export
#' @details
#' Is \code{x} is a filename with a path, only the filename without path is parsed.
#'
#' @examples
#' string <- "aliejus;silicis;0.5mW;1;NA;_RamanShift_14-51-36-968.txt"
#' parser_1(string)
#'
#' string3 <- c(string,string,string)
#' parser_1(string3)
#'
parser_1 <- function(x){

    TEXT <- basename(x)
    pattern <- paste0(
        '(?<Object>.*);',
        '(?<Where>.*);',
        '(?<Laser_power>.*)mW;',
        '(?<Point>.*);',
        '(?<Comment>.*);',
        '_(?<Type_of_spectrum>.*)',
        '_(?<Time>.*)',
        '\\..*'
    )

    DF <- regexp2df(TEXT,pattern)
    DF$Laser_power <- as.numeric(as.character(DF$Laser_power))

    # op <- options(digits.secs = 3)

    DF$Time <- gsub("(.*)-(.*)-(.*)-(.*)",
                    "\\1:\\2:\\3.\\4",
                    DF$Time, fixed = TRUE)

    DF$FileCreatedOn <- file.info(x)$mtime

    # DF$Time <- gsub("-",".",DF$Time,fixed = TRUE)  %>% strptime("%H.%M.%OS")

    # DF$Time <- modifyList(unclass(DF$Time),
    #                       unclass(x.info(x)$mtime %>% as.Posix)[c("mday","mon","year",
    #                                                       "wday","yday","isdst","zone","gmtoff")])
    # DF$timestamp <- DF$Time %>% as.POSIXlt()  %>% unclass
    # options(op)

    return(DF)
}
