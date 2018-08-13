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
#' stringA <- "Duomenu_pvz/Fluorescencija/2010-03-16/ID_001/2.Scope"
#' parser_TD2015(stringA)
#'
#' stringB <- "Duomenu_pvz/Fluorescencija/2014-08-13/ID_451/56.txt"
#' parser_TD2015(stringB)
#'
#' stringC <- "Duomenu_pvz/Fluorescencija/2009-07-13/ID_051/66.Master.Scope"
#' parser_TD2015(stringC)
#'
#' x <- c(stringA,stringB,stringC)
#' parser_TD2015(x)
#'
parser_TD2015 <- function(x){

    sep <- .Platform$file.sep
    pattern <- paste0('.*', sep,
                      '(?<Date_>\\d+-\\d+-\\d+)', sep,
                      '(ID_)?(?<ID>.*?)', sep,
                      '(?<Point>.*?)',
                      '(?:.Master)?',
                      '(?:\\.[Ss]cope|\\.txt)'
    )

    DF <- regexp2df(x, pattern)
    DF$Date_ <- as.Date(DF$Date_)
    DF$FileCreatedOn <- file.info(x)$mtime

    return(DF)
}
