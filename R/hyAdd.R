#' [!] Add a variable to \code{hyperSpec} object
#'
#' Add (or overwrite, if already exists) a variable which name is passed as
#'  \code{var.name} with values in \code{unique.values}
#' that correspond to levels of factor variable \code{by}.
#'
#' @template sp-hy
#' @param by A factor variale which levels will correspond to values in
#'       \code{unique.values}.
#' @param var.name The name of variable to be added.
#' @param unique.values Unique values used to create the variavle.
#'
#' @return \code{HyperSpec} object with added/replaced variable.
#'              Lables of the variable indicate unique values used.
#' @export
#'
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd <- function(sp, var.name, by, unique.values) {
    var.name <- as.character(match.call()$var.name)
    by <- getVarValues(by,sp)

    val_ind   <- unclass(as.factor(by))
    nIncluded <- nlevels(val_ind)

    nUniques <- length(unique.values)

    # If not enough elements:
    if (nUniques < nIncluded) {
        stop(sprintf("At least %d elements in `unique.values` are needed.",nIncluded))
    }

    included_values <- unique.values[1:nIncluded]

    # If not all elements are used:
    if (nUniques > nIncluded) {
        warning(sprintf(paste("Only first %d of %d elements in",
                        "`unique.values` are used:\n%s"),
                        nIncluded, nUniques,
                        paste(included_values, collapse = ", ")))
    }

    # Add column "var.name"
    eval(parse(text = paste0('sp$',var.name,' <- included_values[val_ind]')))

    # Labels is vector with color names
    labels(sp, var.name) <- included_values
    return(sp)
}

