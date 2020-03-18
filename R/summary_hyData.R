#' [!] Summary statistics of non-spectroscopic data in `hyperSpec` objecet
#'
#' An alternative way to summarize non spectroscopic data (in \code{@@data} slot)
#' in a \code{hyperSpec} object.
#'
#' @details If class of summarized object is:\cr
#'  - "hyperSpec", lablels of variables (slot \code{@@label}) are used as
#'   headers of columns.\cr
#'
#' - not "hyperSpec", the function behaves in a similar way as function
#'  \code{summary}. The differences are indicated in section "Examples".
#'
#' @template sp-hy
#' @inheritParams base::summary
#' @param ... further parameters to be passed to function \code{summary}.
#'
#' @return A table with summary statistics.
#' @export
#'
#' @seealso \code{\link[base]{summary}}
#' @examples
#'
#' library(spHelper)
#' library(pander)
#'
#'
#' # Summary of "hyperSpec" object
#'  summary_hyData(Spectra2)
#'
#'
#' # Summary of "hyperSpec" object + `pander` (useful if `knitr` is used)
#'  summary_hyData(Spectra2) %>%  pander
#'
#'
#' # ======= `summary_hyData(sp)` vs `summary(sp$..)' vs `summary(sp)' ========
#' sp <- Spectra2
#' labels(sp) <- list(gr = "--- Group ---", class = "--- Class ---")
#'
#' summary_hyData(sp)  # Column names are appropriate values of `labels(sp)`
#' #>   --- Group --- --- Class ---
#' #>    A:52          K :50
#' #>    B:55          l :30
#' #>    C:43          N :19
#' #>    S1:51
#'
#' summary(sp$..)      # Column names are appropriate values of `colnames(sp)`
#' #>    gr     class
#' #>    A:52   K :50
#' #>    B:55   l :30
#' #>    C:43   N :19
#' #>    S1:51
#'
#' summary(sp)         # Default summary of whole `hyperSpec` object
#' #>   hyperSpec object
#' #>   150 spectra
#' #>   3 data columns
#' #>   501 data points / spectrum
#' #>   wavelength:  [integer] 300 301 ... 800
#' #>   data:  (150 rows x 3 columns)
#' #>   1. gr: --- Group --- [factor] B B ... A
#' #>   2. class: --- Class --- [factor] N l ... S1
#' #>   3. spc:  [matrix501] 159.8996 139.9296 ... 12.11558
#'
#' # ======= Summary of factor variables in a data frame ======================
#' # (if printed using function `pander` there are differences in column
#' # `Species` ):
#'
#' iris[,4:5]  %>%  summary         %>%  pander
#' iris[,4:5]  %>%  summary_hyData  %>%  pander
#'
#'
summary_hyData <- function(object,...){
    UseMethod("summary_hyData")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setGeneric("summary_hyData")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname summary_hyData
#' @export
summary_hyData.default <- function(object,...) {
    # Make summary
    summary_ <- summary(object,...)

    # Clean unnecessary NA's
    # (convenient if `pander::pander` is used to print the results)
    summary_[is.na(summary_)] <- ' '

    # Return results
    return(summary_)
}


#' @rdname summary_hyData
#' @param unique_ID (FALSE | character) either variable name
#' that contain ID numbers (to select only the first rows with
#' unique ID) or \code{FALSE} (default) to use all rows.
#' @export
summary_hyData.hyperSpec <- function(sp, ... , unique_ID = FALSE) {
    chk.hy(sp)

    # Select only unique ID
    if (is.character(unique_ID)) {
        if (!unique_ID %in% names(sp@data))
            stop(glue::glue("The dataset does not contain the indicated variable: unique_ID = '{unique_ID}'"))

       ID <- eval_glue("sp$`{unique_ID}`")
       sp <- sp[!duplicated(ID)]
    }


    # Update labels if some are missing
    labels(sp) <- labels(sp)

    # Mark not used, irrelevalt labels to get rid of
    used_label <-  names(sp@label) %in% names(sp@data)

    # Prepare column names
    Labels <- sp@label[used_label]

    # Remove labels of "special" names:
    Labels[c("spc", ".wavelength")] <- NULL

    # Convert vector-type labels (e.g. c("1","2","3")) to single string (e.g. "1,2,3")
    # And unlist the object "Labels"
    Labels %<>%
        lapply(function(x) {
            if (length(x) > 1)
            {
                paste(x, collapse = ', ')
            } else
                as.character(x)
        })  %>%
        unlist

    # Extract non-spectroscpoic data only
    Non_sp_info <- sp$..

    # Rename columns
    colnames(Non_sp_info) <- Labels

    # Make summary
    summary_ <- summary_hyData(Non_sp_info,...)

    # Return results
    return(summary_)
}
