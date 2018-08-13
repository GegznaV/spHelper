#' Read OceanOptics spectra
#'
#' @param path (string)
#' @param pattern (string) regular extression to filter files to be selected as spectra.
#' @param read_fun (string | function)
#' @param ... parameters to be passed to `read_fun`
#' @inheritParams base::dir
#'
#' @return hyperSpec object
#' @export

read_OOspectra <- function(path,
                         pattern  = ".*", # pattern to filter files to be selected as spectra
                         read_fun = c("auto","ascii","ts","base32","a FUNCTION")[1],
                         recursive = FALSE,
                         ... # arguments to be passed to `read_fun`
)
{

    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Make a list of filenames
    dir_fun  <- function(paths) dir(path = paths,
                                    pattern = pattern,
                                    recursive = recursive,
                                    full.names = TRUE)

    files <- Reduce(c, lapply(path, dir_fun))
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Read spectra
    if (is.character(read_fun)) {
        read_fun <- switch(tolower(read_fun),
                        # ----------------------------------
                           "auto"  = read.OceanView0,
                           "ts"    = read.OceanView.ts0,
                           "ascii" = read.OceanView.ascii0,
                        # ----------------------------------
                           "base32"= read.OOIBase32,
                        # ----------------------------------
                           stop(paste("Unsupported value of `read_fun`:", read_fun))
        )
    } else if (!is.function(read_fun)){
        stop("`read_fun` must be either a character vector or a function")
    }

    sp <- collapse(lapply(files, read_fun, ...))
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(sp)
}
