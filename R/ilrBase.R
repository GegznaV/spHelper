#' [!] `ilrBase` method for `hyperSpec` object
#'
#'
#' @note
#' there is a bug in \pkg{compositions}, thus parameter
#' \code{D} might be irresponsive if used through package
#' \pkg{spHelper}. IIf \code{D} is necessary, consider using
#' \code{compositions::ilrBase} directly.
#'
#' @inheritParams compositions::ilrBase
#' @export
#'
ilrBase <- function(x=NULL, z=NULL, D = NULL,
                    method = "basic"){
    UseMethod("ilrBase")
}


#' @rdname ilrBase
#' @export
#'
ilrBase.default <- function(x = NULL, z = NULL,D = NULL, method = "basic") {
    if (!is.null(D)) {
        stop("If parameter D is needed, use function compositions::ilrBase(), not spHelper::ilrBase().")
        }
    compositions::ilrBase(x = x, z = z, method = method)
}
#' @rdname ilrBase
#' @export
ilrBase.hyperSpec <- function(x = NULL, z = NULL, D = NULL, method = "basic"){
    compositions::ilrBase(x = x@data$spc,
                          z = z,
                          method = method)
}
