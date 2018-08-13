#' @name nID_nSp
#' @aliases nID_nSp
#' @aliases nID_nObs
#' @title [+!] Calculate number and percentage of unique IDs and observations
#'
#'
#' @description Calculate number and percentage of unique IDs and observations.\cr
#'
#' number(IDs) >= number(observations)
#'
#' # DEFAULT BEHAVIOUR OF FUNCTION CHANGED
#'
#' @param data A data frame with variables, that names are denoted by \code{ID} and \code{gr}
#' @param ID Vector with speciment IDs (Either a vector or a variable name in \code{data}).
#' @param gr Vector with speciment groups (Either a vector or a variable name in \code{data}).
#'
#' @param ID_text A title for unique IDs, that apear in the table.
#' @param observation_text A title for observations, that apear in the table.
#'
#' @param decimals A number of maximum decimal places in percentages.
#'  This parameter is passed to function \code{\link[base]{round}}. Default is 0.
#'
#' @return Table (data frame) with count and percentages.
#' @export
#'
#' @examples
#' data(DataSet1)
#'
#' nID_nSp(DataSet1, "ID", "gr")
#' nID_nSp(ID = DataSet1$ID, gr = DataSet1$gr)
#' pander::pander(nID_nSp(DataSet1, "ID", "gr"))
#'
#' nID_nObs(DataSet1, "ID", "gr")
#' pander::pander(nID_nObs(DataSet1, "ID", "gr"))
#'
#' # For hyperSpec object
#' nID_nSp(Spectra2$.., ID = "class", "gr")
#'
#' @author Vilmantas Gegzna
nID_nSp <- function(data = NULL, ID, gr,
                    ID_text          = "medical specimens",
                    observation_text = "spectra",
                    decimals = 1){

    # Parse input and prepare data ===========================================
    # CALL <- match.call()
    # if (!is.null(CALL$data)) { # if `data` is provided:
    #     ID <- getVarValues(ID, data, CALL)
    #     gr <- getVarValues(gr, data, CALL)
    # }
# DEFAULT BEHAVIOUR OF FUNCTION CHANGED

    if (!is.null(data)) {
        ID <- eval_glue("data$`{ID}`")
        gr <- eval_glue("data$`{gr}`")
    }

    # -----------------------------------------------------------------------
    if (length(ID) != length(gr)) stop("Length of `ID` and `gr` must agree.")
    # -----------------------------------------------------------------------
    data <- data.frame(ID = ID, gr = gr)

    # Calculations ===========================================
    percents <- function(TABLE) {
        tbl <- prop.table(TABLE) * 100
        tbl <- round(tbl, decimals)
        paste0(tbl, "%")
    }

    nID <- table(unique(data[,c("ID", "gr")])[ ,"gr"]) # Number of unique medical samples per grpup
    nSp <- table(data[ ,"gr"]) # Number of spectra per group


    tbl <- rbind(nID, percents(nID),  nSp, percents(nSp))

    rownames(tbl) <- c(paste("Number of", ID_text),
                       paste("Percentage of", ID_text),
                       paste("Number of", observation_text),
                       paste("Percentage of", observation_text)
                       )
    return(tbl)
}

#  ------------------------------------------------------------------------

#' @rdname nID_nSp
#' @export
nID_nObs <- function(data = NULL, ID, gr, ID_text = "unique IDs",
                     observation_text = "observations") {
    nID_nSp(data = data, ID = ID, gr = gr,
            ID_text = ID_text,
            observation_text = observation_text)
}
