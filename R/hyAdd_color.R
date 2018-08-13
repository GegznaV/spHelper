#' @export
#' @rdname hyAdd_color
#'
hyRm_palette <- function(sp = NULL){
    sp$.color <- NULL
    return(sp)
}

#' @export
#' @rdname hyAdd_color
#'
hyAdd_palette <- function(sp = NULL, by,
                                      palette = c(RColorBrewer::brewer.pal(9,"Set1")),
                                      label = NULL, ...
){
    # Define colors ------------------------------------------------------
    # c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
    #   "#A65628","#F781BF","#999999")
    # palette <- c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
    #                 "#A65628","#F781BF","#999999")
    # RColorBrewer::brewer.pal(8,"Dark2")
    # # trellis.par.get("superpose.symbol")$col

    # ColorNumbers[is.na(ColorNumbers)] <- nlevels(sp$gr) + 1;
    # --------------------------------------------------------------------

    # by <- getVarValues(by, sp) %>% as.factor()

    by_ <- eval_glue("sp$`{by}`") %>% as.factor()

    # Reset column .colors
    sp$.color <- NA

    if (is.null(sp)) sp <- data.frame()

    ColorNumbers <- unclass(by_);
    # nColors <- max(ColorNumbers, na.rm = T) ### <<<<---- klaida čia
    nColors <- nlevels(by_)


    if (length(palette) < nColors) {
        stop(sprintf("At least %d colors must be in the palette.", nColors))
    }

    UniqueColors <- palette[1:nColors]
    color_vector <- UniqueColors[ColorNumbers]

    palette = data.frame(colors = UniqueColors,
                         labels = levels(by_),
                         stringsAsFactors = FALSE)
    L <- length(by)
    new_attr <- list(var_name  = if (L==1) by else match.call()$by,
                     var_label = if (L==1) labels(sp,by) else match.call()$by)

    attributes(palette) <- modifyList(attributes(palette), new_attr)


    attributes(color_vector) <- list(palette = palette)
    # this line needs review (how to determine real variable name): match.call()$by
    # , variableLabel = labels(sp, by)
    # )

    # Add column for colors
    sp$.color <- color_vector

    # Add label for variable ".color"
    if (inherits(sp, "hyperSpec")){
        if (is.null(label)) {
            txt <- match.call()$by %>% as.character
            if (length(txt) > 0) {
                label <- 'Colors for "'  %++%  labels(sp, txt)  %++% '"'
            } else {
                label <- "Colors"
            }
        }
        labels(sp, ".color") <- label
    }

    # Return the result
    return(sp)
}


#' [+!] Add a variable with color names to \code{hyperSpec} object
#'
#' Add (or overwrite, if already exists) a column \code{.color} with color
#' names that correspond to levels of factor variable \code{by}.
#' Function \code{hyRm_palette} removes this palette.
#'
#' @template sp-hy
#' @param by A factor variale which levels will correspond to colors in
#'       \code{.color}.
#' @param palette A color palette (vector with colors for each level in
#'       \code{by}). If this argument is not provided, default palette is used.
#'
#' @param label Label for column \code{.color}. If \code{label = NULL}, default
#'        label is added.
#'
#' @return \code{HyperSpec} object with added/replaced column \code{.color}.
#'              Lables of variable \code{.color} indicate unique colors used
#'              (illustration in section "Examples").
#' @export
#'
#' @examples
#'
#' data(Spectra2)
#' Spectra1 <- hyAdd_color(Spectra2, "class")
#'
#' colnames(Spectra2)
#'    #> [1] "gr"    "class" "spc"
#'
#' colnames(Spectra1)
#'    #> [1] "gr"     "class"  "spc"    ".color"
#'
#' # Default labels
#' labels(Spectra1,".color")
#'    #>  [1] "Colors for \"class\""
#'
#' hyGet_palette(Spectra1)
#'    #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' # ATTENTION -------------------------------------------------
#'
#' # Preserve labels:
#' Spectra1[1,".color"] <- "red"
#' hyGet_palette(Spectra1)
#'
#' labels(Spectra1,".color")
#'
#' # Overwrites labels:
#' Spectra1$.color[1] <- "red"
#'
#' hyGet_palette(Spectra1)
#'
#' labels(Spectra1,".color")
#'     #>  ".color"
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

hyAdd_color <- function(sp = NULL, by,
                        palette = c(RColorBrewer::brewer.pal(9,"Set1")),
                        label = NULL, ...
                        ){
    # Define colors ------------------------------------------------------
    # c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
    #   "#A65628","#F781BF","#999999")
    # palette <- c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
    #                 "#A65628","#F781BF","#999999")
    # RColorBrewer::brewer.pal(8,"Dark2")
    # # trellis.par.get("superpose.symbol")$col

    # ColorNumbers[is.na(ColorNumbers)] <- nlevels(sp$gr) + 1;
    # --------------------------------------------------------------------

    # by <- getVarValues(by, sp) %>% as.factor()

    by_ <- eval_glue("sp$`{by}`") %>% as.factor()

    # Reset column .colors
    sp$.color <- NA

    if (is.null(sp)) sp <- data.frame()

    ColorNumbers <- unclass(by_);
    # nColors <- max(ColorNumbers, na.rm = T) ### <<<<---- klaida čia
    nColors <- nlevels(by_)


    if (length(palette) < nColors) {
        stop(sprintf("At least %d colors must be in the palette.", nColors))
    }

    UniqueColors <- palette[1:nColors]
    color_vector <- UniqueColors[ColorNumbers]

    palette = data.frame(colors = UniqueColors,
                         labels = levels(by_),
                                  stringsAsFactors = FALSE)
    L <- length(by)
    new_attr <- list(var_name  = if (L==1) by else match.call()$by,
                     var_label = if (L==1) labels(sp,by) else match.call()$by)

    attributes(palette) <- modifyList(attributes(palette), new_attr)


    attributes(color_vector) <- list(palette = palette)
                                      # this line needs review (how to determine real variable name): match.call()$by
                                    # , variableLabel = labels(sp, by)
                                    # )

    # Add column for colors
    sp$.color <- color_vector

    # Add label for variable ".color"
    if (inherits(sp, "hyperSpec")){
        if (is.null(label)) {
            txt <- match.call()$by %>% as.character
            if (length(txt) > 0) {
                label <- 'Colors for "'  %++%  labels(sp, txt)  %++% '"'
            } else {
                label <- "Colors"
            }
        }
        labels(sp, ".color") <- label
    }

    # Return the result
    return(sp)
}

    # labels(sp, ".color") <- UniqueColors
