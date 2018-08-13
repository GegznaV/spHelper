# Cusstomizable function `performance0` ----------------------------------

# copied from original function
#
#' [.]  Cusstomizable function `performance0`: function to create performance objects
#'
#' A more flexible version of \code{\link[ROCR]{performance}}.
#'
#' @inheritParams ROCR::performance
#'
#' @return An S4 object of class performance.
#'
#' @export
#'
#' @seealso \code{\link[ROCR]{performance}}
#' @examples
#'
#'  ## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
#' library(ROCR)
#' data(ROCR.simple)
#' pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
#' perf <- performance0(pred,"tpr","fpr")
#' plot(perf)
#'
#'
performance0 <- function(prediction.obj, measure,
                           x.measure = "cutoff", ...)
{
    envir.list         <- define.environments0(...)
    long.unit.names    <- envir.list$long.unit.names
    function.names     <- envir.list$function.names
    obligatory.x.axis  <- envir.list$obligatory.x.axis
    optional.arguments <- envir.list$optional.arguments
    default.values     <- envir.list$default.values

    if (class(prediction.obj) != "prediction"){
        stop(paste("Wrong argument types: First argument must be of type",
                   "'prediction'"))
    }
    if (!exists(measure,  where = long.unit.names, inherits = FALSE)){
        stop(paste("Measure", measure, "not found"))
    }
    if (!exists(x.measure,  where = long.unit.names, inherits = FALSE)){
        stop(paste("Measure", measure, "not found"))
    }
    if (exists(x.measure, where = obligatory.x.axis, inherits = FALSE)) {
        message <- paste("The performance measure", x.measure,
                         "can only be used as 'measure', because it has",
                         "the following obligatory 'x.measure':\n",
                         get(x.measure, envir = obligatory.x.axis))
        stop(message)
    }
    if (exists(measure, where = obligatory.x.axis, inherits = FALSE)) {
        x.measure <- get(measure, envir = obligatory.x.axis)
    }
    if (x.measure == "cutoff" || exists(measure, where = obligatory.x.axis,
                                        inherits = FALSE)) {
        optional.args <- list(...)
        argnames <- c()
        if (exists(measure, where = optional.arguments, inherits = FALSE)) {
            argnames <- get(measure, envir = optional.arguments)
            default.arglist <- list()
            for (i in 1:length(argnames)) {
                default.arglist <- c(default.arglist,
                                     get(paste(measure,":", argnames[i], sep = ""),
                                         envir = default.values,
                                         inherits = FALSE))
            }
            names(default.arglist) <- argnames
            for (i in 1:length(argnames)) {
                templist <- list(optional.args, default.arglist[[i]])
                names(templist) <- c("arglist", argnames[i])
                optional.args <- do.call(".farg", templist)
            }
        }
        optional.args <- .select.args(optional.args, argnames)
        function.name <- get(measure, envir = function.names)
        x.values <- list()
        y.values <- list()
        for (i in 1:length(prediction.obj@predictions)) {
            argumentlist <- .sarg(optional.args,
                                  predictions = prediction.obj@predictions[[i]],

                                  labels = prediction.obj@labels[[i]],
                                  cutoffs = prediction.obj@cutoffs[[i]],

                                  fp = prediction.obj@fp[[i]],
                                  tp = prediction.obj@tp[[i]],

                                  fn = prediction.obj@fn[[i]],
                                  tn = prediction.obj@tn[[i]],

                                  n.pos = prediction.obj@n.pos[[i]],
                                  n.neg = prediction.obj@n.neg[[i]],

                                  n.pos.pred = prediction.obj@n.pos.pred[[i]],
                                  n.neg.pred = prediction.obj@n.neg.pred[[i]]
            )
            ans <- do.call(function.name, argumentlist)
            if (!is.null(ans[[1]]))
                x.values <- c(x.values, list(ans[[1]]))
            y.values <- c(y.values, list(ans[[2]]))
        }
        if (!(length(x.values) == 0 || length(x.values) == length(y.values))) {
            stop("Consistency error.")
        }
        return(new("performance",
                   x.name = get(x.measure, envir = long.unit.names),
                   y.name = get(measure, envir = long.unit.names),
                   alpha.name = "none",
                   x.values = x.values,
                   y.values = y.values,
                   alpha.values = list()))
    }
    else {
        perf.obj.1 <- performance0(prediction.obj,
                                   measure = x.measure,
                                    ...)
        perf.obj.2 <- performance0(prediction.obj,
                                   measure = measure,
                                    ...)
        return(.combine.performance.objects(perf.obj.1, perf.obj.2))
    }
}

#  ------------------------------------------------------------------------


# Define environments -----------------------------------------------------
# @inheritParams ROCR::prediction
# @export
# @examples
#
define.environments0 <- function(funnames = NULL, # name of measure
                                 longnames = NULL, # name of actual thing
                                 exprs = NULL, # list of 2 character vectors to be expressed
                                 optargs, # list
                                 default.vals,
                                 xaxis
)
{
    # get original environments
    envir.list <- ROCR::.define.environments()
    long.unit.names    = envir.list$long.unit.names
    function.names     = envir.list$function.names
    obligatory.x.axis  = envir.list$obligatory.x.axis
    optional.arguments = envir.list$optional.arguments
    default.values     = envir.list$default.values

    .performance.dice <- function (predictions, labels, cutoffs, fp,
                                   tp, fn, tn, n.pos,
                                   n.neg, n.pos.pred, n.neg.pred) {
        list(cutoffs, 2 * tp / (2*tp + fp + fn))

    }

    assign("dice",
           .performance.dice,
           envir=function.names)

    assign("dice",
           "Sorensen-Dice coefficient",
           envir=long.unit.names)

    #######################################
    # Allow for general adding
    #######################################
    if (!is.null(funnames)){
        stopifnot(
            length(funnames) == length(longnames) &&
                length(funnames) == length(exprs)
        )
        if (!missing(optargs)){
            stopifnot(length(optargs) == length(funnames))
        }
        if (!missing(optargs)){
            stopifnot(length(default.vals) == length(funnames))
        }
        if (!missing(xaxis)){
            stopifnot(length(xaxis) == length(funnames))
        }

        for (iname in seq_along(funnames)){
            ie1 = exprs[[iname]][[1]]
            ie2 = exprs[[iname]][[2]]
            funcname = paste0("func <- function (predictions, labels,
                              cutoffs, fp,
                              tp, fn, tn, n.pos,
                              n.neg, n.pos.pred, n.neg.pred) {
                              list(", ie1, ", ", ie2, ") }")
            eval(parse(text=funcname))

            assign(funnames[iname], func,
                   envir=function.names)
            assign(funnames[iname], longnames[iname],
                   envir=long.unit.names)

            #############
            # optional arguments
            #############
            if (!missing(optargs)){
                oargs = optargs[[iname]]
                for (ioarg in seq_along(oargs)){
                    assign(oargs[[ioarg]][[1]], oargs[[ioarg]][[2]],
                           envir=optional.arguments)
                }
            }

            #############
            # Default values
            #############
            if (!missing(default.vals)){
                oargs = default.vals[[iname]]
                for (ioarg in seq_along(oargs)){
                    assign(oargs[[ioarg]][[1]], oargs[[ioarg]][[2]],
                           envir=default.values)
                }
            }

            if (!missing(default.vals)){
                oargs = default.vals[[iname]]
                for (ioarg in seq_along(oargs)){
                    assign(oargs[[ioarg]][[1]], oargs[[ioarg]][[2]],
                           envir=obligatory.x.axis)
                }
            }

        }
    } # is is.null

    list(
        long.unit.names = long.unit.names,
        function.names = function.names,
        obligatory.x.axis = obligatory.x.axis,
        optional.arguments = optional.arguments,
        default.values = default.values
    )
}

#  .compute.unnormalized.roc.curve --------------------------------------------
# in original function decreasing = TRUE

.compute.unnormalized.roc.curve0 <-
    function (predictions, labels, decreasing = NULL)
    {
        neg.label <- levels(labels)[1]
        pos.label <- levels(labels)[2]

        if (is.null(decreasing)){
            trim_mean <- function(LABEL){
                mean(predictions[labels == LABEL], trim = .10)
            }
            mean__pos.label <- trim_mean(pos.label)
            mean__neg.label <- trim_mean(neg.label)

            decreasing <- mean__pos.label > mean__neg.label
        }

        pred.order <- order(predictions, decreasing = decreasing)
        predictions.sorted <- predictions[pred.order]
        tp <- cumsum(labels[pred.order] == pos.label)
        fp <- cumsum(labels[pred.order] == neg.label)
        dups <- rev(duplicated(rev(predictions.sorted)))
        tp <- c(0, tp[!dups])
        fp <- c(0, fp[!dups])

        if (decreasing == TRUE){
            cutoffs <- c(Inf, predictions.sorted[!dups])
        } else {
            cutoffs <- c(-Inf, predictions.sorted[!dups])
        }

        return(list(cutoffs = cutoffs, fp = fp, tp = tp))
    }


# Prediction ------------------------------------------------------


#' [!] Function to create prediction objects
#'
#' This function is similar to \code{\link[ROCR]{prediction}}  (package
#'  \pkg{ROCR}, which is described:
#' Every classifier evaluation using ROCR starts with creating a \code{prediction}
#'  object. This function is used to transform the input data (which can be
#'  in vector, matrix, data frame, or list form) into a standardized format.\cr
#'
#'  The main \bold{difference} in \code{prediction0} is that it calculates
#'  performance in a more flexible way. I.e. value of negative group do not
#'  have to be smaller than in positive group (see description of parameter
#'  \code{neg.smaller})
#'
#'
#' @inheritParams ROCR::prediction
#' @param neg.smaller Either logical or \code{NULL} (default):\cr
#' \code{TRUE} value of of negative group is always smaller (default in
#' function \code{\link[ROCR]{prediction}});\cr
#' \code{FALSE} value of of negative group is always larger;\cr
#' \code{NULL} it is automatically decided either value of negative group is
#'  smaller or larger by calculating and comparing 10% trimmed means of both
#'  groups.
#'
#' @details
#'
#' predictions' and 'labels' can simply be vectors of the same length. However,
#'  in the case of cross-validation data, different cross-validation runs can
#'  be provided as the *columns* of a matrix or data frame, or as the entries
#'  of a list. In the case of a matrix or data frame, all cross-validation
#'  runs must have the same length, whereas in the case of a list, the lengths
#'   can vary across the cross-validation runs. Internally, as described in
#'   section 'Value', all of these input formats are converted to list
#'   representation.
#'
#' Since scoring classifiers give relative tendencies towards a negative
#' (low scores) or positive (high scores) class, it has to be declared which
#' class label denotes the negative, and which the positive class. Ideally,
#' abels should be supplied as ordered factor(s), the lower level
#' corresponding to the negative class, the upper level to the positive
#' lass. If the labels are factors (unordered), numeric, logical or
#' characters, ordering of the labels is inferred from R's built-in
#' < relation (e.g. 0 < 1, -1 < 1, 'a' < 'b', FALSE < TRUE). Use
#' label.ordering to override this default ordering. Please note that the
#'  ordering can be locale-dependent e.g. for character labels '-1' and '1'.
#'
#' Currently, ROCR supports only binary classification (extensions toward
#' multiclass classification are scheduled for the next release, however).
#' If there are more than two distinct label symbols, execution stops with
#' an error message. If all predictions use the same two symbols that are
#' used for the labels, categorical predictions are assumed. If there are
#' more than two predicted values, but all numeric, continuous predictions
#' are assumed (i.e. a scoring classifier). Otherwise, if more than two
#' symbols occur in the predictions, and not all of them are numeric,
#' execution stops with an error message.
#'
#' @return An S4 object of class \code{prediction}.
#'
#' @export
#'
#' @seealso Detailed descriptions and references: \code{\link[ROCR]{prediction}}.
#'
#' @examples
#'
#' # create a simple prediction object
#' library(spHelper)
#' data(ROCR.simple,package = "ROCR")
#' x  <- ROCR.simple$predictions
#' gr <- ROCR.simple$labels
#' pred <- prediction(x, gr)
#'
#' pred0 <- prediction0(x, gr)
#'

prediction0 <- function(predictions, labels, label.ordering = NULL,
                         neg.smaller = NULL)
{
    if (is.data.frame(predictions)) {
        names(predictions) <- c()
        predictions <- as.list(predictions)
    }
    else if (is.matrix(predictions)) {
        predictions <- as.list(data.frame(predictions))
        names(predictions) <- c()
    }
    else if (is.vector(predictions) && !is.list(predictions)) {
        predictions <- list(predictions)
    }
    else if (!is.list(predictions)) {
        stop("Format of predictions is invalid.")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.data.frame(labels)) {
        names(labels) <- c()
        labels <- as.list(labels)
    }
    else if (is.matrix(labels)) {
        labels <- as.list(data.frame(labels))
        names(labels) <- c()
    }
    else if ((is.vector(labels) || is.ordered(labels) || is.factor(labels)) &&
             !is.list(labels)) {
        labels <- list(labels)
    }
    else if (!is.list(labels)) {
        stop("Format of labels is invalid.")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(predictions) != length(labels))
        stop(paste("Number of cross-validation runs must be equal",
                   "for predictions and labels."))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!all(sapply(predictions, length) == sapply(labels, length)))
        stop(paste("Number of predictions in each run must be equal",
                   "to the number of labels for each run."))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (i in 1:length(predictions)) {
        finite.bool <- is.finite(predictions[[i]])
        predictions[[i]] <- predictions[[i]][finite.bool]
        labels[[i]] <- labels[[i]][finite.bool]
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    label.format = ""
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (all(sapply(labels, is.factor)) && !any(sapply(labels,
                                                      is.ordered))) {
        label.format <- "factor"
    }
    else if (all(sapply(labels, is.ordered))) {
        label.format <- "ordered"
    }
    else if (all(sapply(labels, is.character)) || all(sapply(labels,
                                                             is.numeric)) || all(sapply(labels, is.logical))) {
        label.format <- "normal"
    }
    else {
        stop(paste("Inconsistent label data type across different",
                   "cross-validation runs."))
    }
    if (!all(sapply(labels, levels) == levels(labels[[1]]))) {
        stop(paste("Inconsistent factor levels across different",
                   "cross-validation runs."))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    levels <- c()
    if (label.format == "ordered") {
        if (!is.null(label.ordering)) {
            stop(paste("'labels' is already ordered. No additional",
                       "'label.ordering' must be supplied."))
        }
        else {
            levels <- levels(labels[[1]])
        }
    }
    else {
        if (is.null(label.ordering)) {
            if (label.format == "factor")
                levels <- sort(levels(labels[[1]]))
            else levels <- sort(unique(unlist(labels)))
        }
        else {
            if (!setequal(unique(unlist(labels)), label.ordering)) {
                stop("Label ordering does not match class labels.")
            }
            levels <- label.ordering
        }
        for (i in 1:length(labels)) {
            if (is.factor(labels))
                labels[[i]] <- ordered(as.character(labels[[i]]),
                                       levels = levels)
            else labels[[i]] <- ordered(labels[[i]], levels = levels)
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(levels) != 2) {
        message <- paste0("Number of classes is not equal to 2.\n",
                          "ROCR currently supports only evaluation of ",
                          "binary classification tasks.")
        stop(message)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.numeric(unlist(predictions))) {
        stop("Currently, only continuous predictions are supported by ROCR.")
    }
    cutoffs <- list()
    fp <- list()
    tp <- list()
    fn <- list()
    tn <- list()
    n.pos <- list()
    n.neg <- list()
    n.pos.pred <- list()
    n.neg.pred <- list()

    for (i in 1:length(predictions)) {
        n.pos <- c(n.pos, sum(labels[[i]] == levels[2]))
        n.neg <- c(n.neg, sum(labels[[i]] == levels[1]))
        ans <- .compute.unnormalized.roc.curve0(predictions[[i]],
                                                labels[[i]], neg.smaller)
        cutoffs <- c(cutoffs, list(ans$cutoffs))
        fp <- c(fp, list(ans$fp))
        tp <- c(tp, list(ans$tp))
        fn <- c(fn, list(n.pos[[i]] - tp[[i]]))
        tn <- c(tn, list(n.neg[[i]] - fp[[i]]))
        n.pos.pred <- c(n.pos.pred, list(tp[[i]] + fp[[i]]))
        n.neg.pred <- c(n.neg.pred, list(tn[[i]] + fn[[i]]))
    }

    return(new("prediction", predictions = predictions, labels = labels,
               cutoffs = cutoffs, fp = fp, tp = tp, fn = fn, tn = tn,
               n.pos = n.pos, n.neg = n.neg, n.pos.pred = n.pos.pred,
               n.neg.pred = n.neg.pred))
}
