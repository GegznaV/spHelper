# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= `number_ticks` [Internal function] =======================
#  [Internal function]
#  Function to to calculate position of ticks
number_ticks <- function(n, min.n = 2, ...) {
    function(limits) pretty(limits, n, min.n = min.n,...)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Prepare spectroscopic data ===============================

#' Calculate a summary statistic and return long-format data frame
#'
#' Calculate a summary statistic of spectroscopic data in
#' \code{hyperSpec} object and return the result as a
#' long-format data frame.
#'
#' @template sp-hy
#' @param FUN Function of summray statistics.
#' @param by (string) name of grouping variable.
#' @param ... Arguments to be passed to \code{FUN}.
#' @param var_names Names of summary statisticsm generated with \code{FUN}.
#' @param name_if_by_is_NULL (string) Name of whole dataset in case \code{by}
#'        is NULL. Default is "All Data"
#'
#' @return A long-format data frame.
#' @export
#'
#' @examples
#' spStat_ldf(Spectra2, FUN = mean)
#'
#' spStat_ldf(sp, "gr", FUN = quantile, probs = c(0,1),
#'            var_names = c("ymin","ymax"))
#'
spStat_ldf <- function(sp, by, FUN,  ..., var_names = NULL,
                       name_if_by_is_NULL = "All Data") {

    Groups <- (getVarValues(by, sp) %if_null_or_len0%
        rep(name_if_by_is_NULL, nrow(sp)))%>% as.factor

    n_gr  <- Groups %>% droplevels() %>% nlevels()

    # Find min and max
    aggr_sp <- hyperSpec::aggregate(sp,
                                    Groups,
                                    FUN = FUN,
                                    ...,
                                    na.rm = TRUE) %>% 
        hyDrop_NA
    n_rows  <- nrow(aggr_sp)

    # Number of new names
    n_needed   <- n_rows / n_gr
    n_provided <- length(var_names)

    # Identify each calculated summary statistic (.type)
    var_basename <- match.call()$FUN  %>% as.character
    var_number   <- (n_provided+1):n_needed
    varNames_final <- c(var_names, # <- var names, that are function inputs
                        # Var names that are generated:
                        if (length(var_number) == 1){
                            var_basename
                        } else { # if length is more than 1, suffix is added.
                          paste0(var_basename, "_", var_number)
                        }
    )[1:n_needed]

    aggr_sp$.type <- rep_len(varNames_final, n_rows)

    # Convert to data frame and split by values of `.type`
    sp_ <- split(ldf(aggr_sp), aggr_sp$.type)

    # Create variables with calculated summary statistics
    new_vars <- lapply(sp_, function(x) x$spc) %>% as.data.frame
    spDF <- sp_[[1]]
    spDF[,c("spc",".type")]  <- NULL
    spDF <- cbind(spDF, new_vars)

    return(spDF)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Check `palette` ==========================================
#' @export
#' @rdname qplot_spRange
# Groups vector with groouping variable
# palette - palette object.
#
# @examples
#
# check_palette(palette, Groups)

check_palette <- function(palette, Groups){
    nPal <- length(palette)
    nColNeeded <-  Groups %>% as.factor %>% droplevels  %>% nlevels
    if (nPal < nColNeeded) {
        if (nPal > 0){
            warning(sprintf(paste("There are %d colors in provided palette",
                                  "and %d are needed, thus the DEFAULT colors",
                                  "will be used."),nPal,nColNeeded))
        }
        palette <- NULL

    }

    return(palette)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= aes_sp ==========================================

#' Default aesthetics for plotting a hyperSpec object with ggplot2
#'
#' Convenience functions that automatically map default
#' \pkg{ggplot2} aesthetics and enamble easy plotting of
#' \code{hyperSpec} object with \pkg{ggplot2}. \cr
#' Default values of aesthetics are \cr \code{x = ".wavelength",
#' \cr y = "spc", \cr group = ".rownames"}.\cr
#' These functions can be used with in combination with
#' \code{\link[hyperSpec]{as.long.df}} and its wrapper \code{\link{ldf}}
#' that is more convenient for plotting.
#'
#' @details
#'  Parameters entered to \code{aes_sp}, \code{aes_sp_} or
#'  \code{aes_sp_string} update and modify the list of mapped aesthetics.
#' If default aesthetic (x, y, or group) has to be changed or removed,
#' it shoud be done explicitly, e.g., \code{aes_sp(x = NULL)}.
#' See examples below.  \cr
#'
#'  \code{aes_sp} accepts unquoted variable names. It is alternative
#'                to \code{\link[ggplot2]{aes}()}\cr
#'
#'  \code{aes_sp_string} requires explicitly quoted variable names
#'                      with \code{""}. It is alternative to
#'                       \code{\link[ggplot2]{aes_string}()}\cr
#'
#' \code{aes_sp_} is alias to \code{aes_sp_string}.
#'
#' @param ... List of name value pairs giving aesthetics to map to variables.
#'            See \code{\link[ggplot2]{aes}()}.
#'
#' @export
#'
#' @examples
#'
#' aes_sp()
#'    ## * group -> .rownames
#'    ## * x     -> .wavelength
#'    ## * y     -> spc
#'
#' # To add aesthetics:
#'
#' aes_sp(color = length)
#'    ## * group  -> .rownames
#'    ## * x      -> .wavelength
#'    ## * y      -> spc
#'    ## * colour -> length
#'
#'
#' # To remove defaul values of aesthetics, do it expicitly:
#'
#' aes_sp(x = NULL)
#'    ## * group -> .rownames
#'    ## * x     -> .wavelength
#'
#'
#' # To use `aes_sp` in combination with function `ldf`:
#'
#' ggplot(ldf(Spectra2[1:10]), aes_sp()) + geom_line()
#'
#' ggplot() + geom_line(aes_sp(), ldf(Spectra2[1:10]))
#'
aes_sp <- function(...){

    aes <- structure(as.list(match.call()[-1]), class = "uneval")
    new_aes <- ggplot2:::rename_aes(aes)

    modifyList(aes_string(x = ".wavelength",
                          y = "spc",
                      group = ".rownames"),
               new_aes
   )
}

# ============= aes_sp_string ==========================================
#' @export
#' @rdname aes_sp
aes_sp_string <- function(...){
    modifyList(aes_string(x = ".wavelength",
                          y = "spc",
                      group = ".rownames"),
               aes_string(...)
    )
}

# ============= aes_sp_ ==========================================
#' @export
#' @rdname aes_sp
aes_sp_ <- function(...){aes_sp_string(...)}

