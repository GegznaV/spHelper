# getMethod("mean","hyperSpec") -------------------------------------------
Method Definition:
    
    function (x, ...) 
    {
        .local <- function (x, na.rm = TRUE, ...) 
        {
            m <- structure(colMeans(x@data$spc),
                           dim = c(1, length(x@wavelength)), 
                           dimnames = list("mean", NULL))
            decomposition(x, m)
        }
        .local(x, ...)
    }


# getMethod("log","hyperSpec") --------------------------------------------
Method Definition:
    
    function(x, ...) 
    {
        .local <- function(x, base = exp(1), ...) 
        {
            validObject(x)
            x[[]] <- log(x[[]], base = base)
            x
        }
        .local(x, ...)
    }


# getMethod("Math","hyperSpec") -----------------------------------------------
Method Definition:
    
    function (x) 
    {
        validObject(x)
        if (grepl("^cum", .Generic) || grepl("gamma$", .Generic)) 
            warning(paste("Do you really want to use", .Generic, 
                          "on a hyperSpec object?"))
        x[[]] <- callGeneric(x[[]])
        x
    }
<environment: namespace:hyperSpec>
    
    Signatures:
    x          
target  "hyperSpec"
defined "hyperSpec"



#  getMethod("aggregate","hyperSpec") -----------------------------------------

Method Definition:
    
    function (x, ...) 
    {
        .local <- function (x, by = stop("by is needed"), FUN = stop("FUN is needed."), 
                            ..., out.rows = NULL, append.rows = NULL, by.isindex = FALSE) 
        {
            validObject(x)
            if (!is.list(by) || !by.isindex) 
                by <- split(seq(x, index = TRUE), by, drop = TRUE)
            if (is.null(out.rows)) {
                tmp <- .apply(data = x@data[by[[1]], , drop = FALSE], 
                              MARGIN = 2, FUN = FUN, ...)
                out.rows <- nrow(tmp) * length(by)
            }
            data <- x@data[rep(1, out.rows), , drop = FALSE]
            data <- cbind(data, .aggregate = NA)
            col.aggregate <- ncol(data)
            r <- 1
            for (i in seq(along = by)) {
                tmp <- .apply(data = x@data[by[[i]], , drop = FALSE], 
                              MARGIN = 2, FUN = FUN, ...)
                prows <- nrow(tmp) - 1
                if (r + prows > out.rows) {
                    if (is.null(append.rows)) 
                        append.rows <- max(100, ceiling(1 - (i/length(by)) * 
                                                            out.rows))
                    out.rows <- max(append.rows + out.rows, r + prows)
                    data <- rbind(data, data[rep(1, out.rows - nrow(data)), 
                                             , drop = FALSE])
                    warning("At", i, "of", length(by),
                            "levels: Output data.frame too small. Consider using an", 
                            "appropriate value for out.rows to speed up calculations.")
                }
                if (prows >= 0) {
                    data[r:(r + prows), -col.aggregate] <- tmp
                    data[r:(r + prows), col.aggregate] <- i
                    r <- r + prows + 1
                }
            }
            x@data <- data[seq_len(r - 1), , drop = FALSE]
            x@data[, col.aggregate] <- factor(x@data[, col.aggregate], 
                                              levels = seq_along(by))
            if (!is.null(names(by)) && !any(is.na(names(by)))) 
                levels(x@data[, col.aggregate]) <- names(by)
            x
        }
        .local(x, ...)
    }
# <environment: namespace:hyperSpec>
#     
#     Signatures:
#     x          
# target  "hyperSpec"
# defined "hyperSpec"



# getMethod("plot", c("hyperSpec", "character")) --------------------------
function (x, y, ...) 
{
    dots <- list(...)
    if (missing(y)) {
        stop("second argument to plot is missing. Should be a character indicating the type of plot.")
        y = "spc"
    }
    switch(tolower(y),
           spc = plotspc(x, ...),
           
           spcmeansd = {
               dots <- modifyList(list(object = mean_pm_sd(x), fill = c(1,NA, 1)), dots)
               do.call(plotspc, dots)
           },
           
           spcprctile = {
               dots <- modifyList(list(object = quantile(x, probs = c(0.16, 
                                                                      0.5, 0.84)),
                                       fill = c(1, NA, 1)), dots)
               do.call(plotspc, dots)
           },
           spcprctl5 = {
               dots <- modifyList(list(object = quantile(x, probs = c(0.05, 
                                                                      0.16, 0.5, 0.84, 0.95)), fill = c(1, 2, 3, 2, 1), 
                                       fill.col = c("#00000040")), dots)
               do.call(plotspc, dots)
           },
           map = plotmap(x, ...),
           voronoi = plotvoronoi(x, ...), 
           mat = plotmat(x, ...),
           c = plotc(x, ...),
           ts = plotc(x, spc ~ t, ...),
           depth = plotc(x, spc ~ z, ...),
           stop(paste("y = ", y, "unknown.", collapse = " ")))
}
# <environment: namespace:hyperSpec>
#     
#     Signatures:
#     x           y          
# target  "hyperSpec" "character"
# defined "hyperSpec" "character"


# plotspc -----------------------------------------------------------------

plotspc
function (object, wl.range = NULL, wl.index = FALSE, wl.reverse = FALSE, 
          spc.nmax = 50, func = NULL, func.args = list(), stacked = NULL, 
          stacked.args = list(), add = FALSE, bty = "l", plot.args = list(), 
          col = "black", lines.args = list(), xoffset = 0, yoffset = 0, 
          nxticks = 10, axis.args = list(), break.args = list(), title.args = list(), 
          fill = NULL, fill.col = NULL, border = NA, polygon.args = list(), 
          zeroline = list(lty = 2, col = col)) 
{
    force(zeroline)
    chk.hy(object)
    validObject(object)
    if (nrow(object) == 0) 
        stop("No spectra.")
    if (is.null(wl.range)) {
        wl.range <- seq_along(object@wavelength)
        wl.index <- TRUE
    }
    if (!is.list(wl.range)) 
        wl.range <- list(wl.range)
    if (!wl.index) 
        wl.range <- lapply(wl.range, function(w) {
            tmp <- unique(wl2i(object, w))
            tmp[!is.na(tmp)]
        })
    if (length(xoffset) == length(wl.range) - 1) 
        xoffset = c(0, xoffset)
    else if (length(xoffset) == 1) 
        xoffset = rep(xoffset, times = length(wl.range))
    if (!is.numeric(xoffset) || (length(xoffset) != length(wl.range))) 
        stop("xoffset must be a numeric  vector of the same length (or one less) as the list with", 
             "wavenumber ranges.")
    xoffset <- cumsum(xoffset)
    u.wl.range <- unlist(wl.range)
    wavelengths <- relist(object@wavelength[u.wl.range], wl.range)
    x <- wavelengths
    for (i in seq_along(x)) x[[i]] <- x[[i]] - xoffset[i]
    ispc <- relist(seq_along(u.wl.range), wl.range)
    rm(wl.range)
    spc <- object[[, , u.wl.range, drop = FALSE, wl.index = TRUE]]
    rm(u.wl.range)
    if (!is.null(func)) {
        if (!is.function(func)) 
            stop("func needs to be a function.")
        apply.args <- c(list(X = spc, MARGIN = 2, FUN = func), 
                        func.args)
        spc <- matrix(do.call(apply, apply.args), ncol = ncol(spc))
        if (nrow(spc) == 0) 
            stop("No spectra after", func, "was applied.")
    }
    if (nrow(spc) > spc.nmax) {
        warning(paste("Number of spectra exceeds spc.nmax. Only the first", 
                      spc.nmax, "are plotted."))
        spc <- spc[seq_len(spc.nmax), , drop = FALSE]
    }
    if (!is.null(stacked)) {
        stacked.args <- modifyList(stacked.args, list(x = object, 
                                                      stacked = stacked, .spc = spc))
        if (!is.null(lines.args$type) && lines.args$type == "h") 
            stacked.args <- modifyList(stacked.args, list(min.zero = TRUE))
        stacked <- do.call(stacked.offsets, stacked.args)
        if (all(yoffset == 0)) 
            yoffset <- stacked$offsets[stacked$groups]
        else if (length(yoffset) == length(unique(stacked$groups))) 
            yoffset <- yoffset[stacked$groups]
    }
    if (length(yoffset) != nrow(spc)) {
        if (length(yoffset) == 1) 
            yoffset <- rep(yoffset, nrow(spc))
        else if (length(yoffset) > nrow(spc)) 
            yoffset <- yoffset[seq_len(nrow(spc))]
        else stop("yoffset must be single number or one number for each spectrum (or stacking group).")
    }
    spc <- sweep(spc, 1, yoffset, "+")
    if (!add) {
        plot.args <- modifyList(list(xlim = range(unlist(x), 
                                                  na.rm = TRUE), ylim = range(spc, na.rm = TRUE)), 
                                plot.args)
        if (wl.reverse) 
            plot.args$xlim <- rev(plot.args$xlim)
        plot.args <- modifyList(plot.args, list(x = unlist(x), 
                                                y = spc[1, , drop = FALSE], type = "n", bty = "n", 
                                                xaxt = "n", yaxt = "n", xlab = NA, ylab = NA))
        do.call(plot, plot.args)
        if (diff(plot.args$xlim) < 0) 
            plot.args$xlim <- rev(plot.args$xlim)
        axis.args <- modifyList(list(x = list(), y = list()), 
                                axis.args)
        if (bty %in% c("o", "l", "c", "u", "]", "x")) {
            cuts <- .cut.ticks(sapply(wavelengths, min), sapply(wavelengths, 
                                                                max), xoffset, nxticks)
            axis.args$x <- modifyList(axis.args[!names(axis.args) %in% 
                                                    c("x", "y")], axis.args$x)
            if (is.null(axis.args$x$labels) & !is.null(axis.args$x$at)) 
                axis.args$x$labels <- axis.args$x$at
            axis.args$x <- modifyList(list(side = 1, at = cuts$at, 
                                           labels = cuts$labels), axis.args$x)
            axis(side = 1, at = max(abs(plot.args$xlim)) * c(-1.1, 
                                                             1.1))
            do.call(axis, axis.args$x)
            break.args <- modifyList(list(style = "zigzag"), 
                                     break.args)
            break.args$axis <- NULL
            break.args$breakpos <- NULL
            if (length(cuts$cut) > 0) {
                if (!requireNamespace("plotrix")) {
                    cat("hyperSpec will use its own replacement for plotrix' axis.break\n\n")
                    break.fun <- .axis.break
                }
                else {
                    break.fun <- plotrix::axis.break
                }
                for (i in cuts$cut) do.call(break.fun, c(list(axis = 1, 
                                                              breakpos = i), break.args))
            }
        }
        if (bty %in% c("o", "l", "c", "u", "y")) {
            axis.args$y <- modifyList(axis.args[!names(axis.args) %in% 
                                                    c("x", "y", "main", "sub")], axis.args$y)
            if (!is.null(stacked)) {
                if (!is.null(stacked.args$min.zero) && stacked.args$min.zero) 
                    group.mins <- stacked$offsets
                else group.mins <- apply(spc[!duplicated(stacked$groups), 
                                             , drop = FALSE], 1, min, na.rm = TRUE)
                axis.args$y <- modifyList(list(at = stacked$offsets, 
                                               labels = stacked$levels[!duplicated(stacked$levels)]), 
                                          axis.args$y)
            }
            axis.args$y <- modifyList(list(side = 2), axis.args$y)
            axis(side = 2, at = max(abs(plot.args$ylim)) * c(-1.1, 
                                                             1.1))
            do.call(axis, axis.args$y)
        }
        tmp <- title.args[!names(title.args) %in% c("x", "y", 
                                                    "ylab", "main", "sub")]
        tmp <- modifyList(tmp, as.list(title.args$x))
        tmp <- modifyList(list(xlab = I(object@label$.wavelength), 
                               line = 2.5), tmp)
        do.call(title, tmp)
        tmp <- title.args[!names(title.args) %in% c("x", "y", 
                                                    "xlab", "main", "sub")]
        tmp <- modifyList(tmp, as.list(title.args$y))
        tmp <- modifyList(list(ylab = I(object@label$spc)), tmp)
        do.call(title, tmp)
        tmp <- title.args[!names(title.args) %in% c("x", "y", 
                                                    "xlab", "ylab")]
        tmp <- modifyList(tmp, as.list(title.args[c("main", "sub")]))
        tmp <- modifyList(list(ylab = I(object@label$spc)), tmp)
        do.call(title, tmp)
    }
    col <- rep(col, each = ceiling(nrow(spc)/length(col)), length.out = nrow(spc))
    if (!is.logical(zeroline) && !is.na(zeroline)) {
        zeroline <- modifyList(list(h = unique(yoffset)), as.list(zeroline))
        do.call(abline, zeroline)
    }
    for (i in seq_along(x)) {
        if (!is.null(fill)) {
            if (is.character(fill) && length(fill) == 1) 
                fill <- unlist(object[[, fill]])
            else if (isTRUE(fill)) {
                fill <- seq_len(nrow(spc)/2)
                if (nrow(spc)%%2 == 1) 
                    fill <- c(fill, NA, rev(fill))
                else fill <- c(fill, rev(fill))
            }
            else if (is.factor(fill)) 
                fill <- as.numeric(fill)
            else if (!is.numeric(fill)) 
                stop("fill must be either TRUE, the name of the extra data column to use for grouping,", 
                     "a factor or a numeric.")
            groups = unique(fill)
            groups = groups[!is.na(groups)]
            polygon.args <- modifyList(list(x = NULL, y = NULL), 
                                       polygon.args)
            if (is.null(fill.col)) {
                fill.col <- character(length(groups))
                for (j in seq_along(groups)) {
                    tmp <- which(fill == groups[j])
                    fill.col[j] <- rgb(t(col2rgb(col[tmp[1]])/255)/3 + 
                                           2/3)
                }
            }
            else {
                fill.col <- rep(fill.col, length.out = length(groups))
            }
            border <- rep(border, length.out = length(groups))
            polygon.args$x <- c(x[[i]], rev(x[[i]]))
            for (j in seq_along(groups)) {
                tmp <- which(fill == groups[j])
                polygon.args$y <- c(spc[head(tmp, 1), ispc[[i]]], 
                                    rev(spc[tail(tmp, 1), ispc[[i]]]))
                polygon.args$col = fill.col[groups[j]]
                polygon.args$border <- border[groups[j]]
                do.call(polygon, polygon.args)
            }
        }
        lines.args <- modifyList(list(x = NULL, y = NULL, type = "l"), 
                                 lines.args)
        if (lines.args$type == "h" && is.list(stacked)) {
            for (j in seq_len(nrow(spc))) {
                keep <- !is.na(spc[j, ispc[[i]]])
                lines.args$x <- rep(x[[i]][keep], each = 3)
                lines.args$y <- as.numeric(matrix(c(rep(yoffset[j], 
                                                        sum(keep)), spc[j, ispc[[i]]][keep], rep(NA, 
                                                                                                 sum(keep))), byrow = TRUE, nrow = 3))
                lines.args$type = "l"
                lines.args$col <- col[j]
                do.call(lines, lines.args)
            }
        }
        else {
            for (j in seq_len(nrow(spc))) {
                keep <- !is.na(spc[j, ispc[[i]]])
                lines.args$x <- x[[i]][keep]
                lines.args$y <- spc[j, ispc[[i]]][keep]
                lines.args$col <- col[j]
                do.call(lines, lines.args)
            }
        }
    }
    invisible(list(x = rep(unlist(x), each = nrow(spc)), y = spc, 
                   wavelengths = rep(unlist(wavelengths), each = nrow(spc))))
}
# <environment: namespace:hyperSpec>
