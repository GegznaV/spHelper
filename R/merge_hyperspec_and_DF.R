##' [!!!] Merge a hyperSpec object and a data frame
##'
##' Merge a hyperSpec object and a data frame.
##'
##'
##' @aliases merge,hyperSpec,hyperSpec-method merge
##' @inheritParams base::merge.data.frame
##' @param x a hyperSpec object
##' @param y a data.frame object
##' @param ... handed to \code{\link[base]{merge.data.frame}}
##'
##' @author V. Gegzna
##' @return A hyperSpec object, with updated slot \code{@@data}.
##'
##' @export
##'
##' @rdname merge
##' @docType methods
##'
##' @aliases merge
##' @seealso \code{\link[base]{merge}}, \code{\link[hyperSpec]{merge}}.
##'
##' \code{\link{collapse}} combines hyperSpec objects that do not share the wavelength axis.
##' \code{\link{rbind}}, and \code{\link{cbind}} for combining hyperSpec objects that.
##'
##' @keywords manip
##'
##' @examples
##'
##' merge(chondro [1:10,, 600], chondro[5:15,, 600]$.., by = c("x", "y"))$.
##' tmp <- merge (chondro [1:10,, 610], chondro [5:15,, 610]$..,
##'               by = c("x", "y"), all = TRUE)
##' tmp$.
##' wl (tmp)
##'
##' ## remove duplicated wavelengths:
##' approxfun <- function (y, wl, new.wl){
##'   approx (wl, y, new.wl, method = "constant",
##'           ties = function (x) mean (x, na.rm = TRUE)
##'           )$y
##' }
##'
##' merged <- merge (chondro [1:7,, 610 ~ 620], chondro [5:10,, 615 ~ 625], all = TRUE)
##' merged$.
##' merged <- apply (merged, 1, approxfun,
##'                  wl = wl (merged), new.wl = unique (wl (merged)),
##'                  new.wavelength = "new.wl")
##' merged$.
##'
##'
##'
setMethod ("merge", signature = signature (x = "hyperSpec", y = "data.frame"),
   function (x, y, ...){
       validObject (x)

       tmp <- .merge2(x, y, ...)

       if (nrow (tmp) == 0 && nrow (x) > 0 && nrow (y) > 0)
           warning ("Function `merge` results in 0 spectra.")
       return(tmp)
   }
)

.merge2 <- function(x,y,...){
    merged_DF <- merge(x@data, y, ...)

    new("hyperSpec",
        data        = merged_DF,
        wavelength  = x@wavelength,
        labels      = x@label)
}

