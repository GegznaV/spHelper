
#' [+] Get color palette used to create variable'.color'
#'
#' @param sp A \code{hyperSpec} object, that contais variable \code{.color}
#'  added with function \code{\link{hyAdd_color}}
#'
#' @return Color palette used to create variable \code{.color}.
#' @note If class of \code{sp} is not \code{hyperSpec}, function returns
#'       \code{NULL}.
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
#' # Default label
#' labels(Spectra1,".color")
#'    #>  [1] "Colors for \"class\""
#'
#' # Names of attributes of variable ".color":
#' attributes(Spectra1$.color)  %>% names
#'   #> [1] "palette"      "variableName"
#'
#' # Values of attributes:
#' attributes(Spectra1$.color)
#'   #> $palette
#'   #> color label
#'   #> 1 #E41A1C     K
#'   #> 2 #377EB8     l
#'   #> 3 #4DAF4A     N
#'   #> 4 #984EA3    S1
#'   #>
#'   #> $variableName
#'   #> [1] "class"
#'
#' # Extract color palette:
#' hyGet_palette(Spectra1)
#'    #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#'    #>      colors   labels
#'    #>  1 #E41A1C     K
#'    #>  2 #377EB8     l
#'    #>  3 #4DAF4A     N
#'    #>  4 #984EA3    S1
#'
#' # Extract palette (just colors) as vector:
#' hyGet_palette0(Spectra1)
#'    #>  [1] #E41A1C #377EB8 #4DAF4A #984EA3
#'    #>  Levels: #377EB8 #4DAF4A #984EA3 #E41A1C
#'
#' # ATTENTION -------------------------------------------------
#'
#' # Preserve labels/ color palette (operator ` ]<-`):
#' Spectra1[1,".color"] <- "red"
#' hyGet_palette(Spectra1)
#'     #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' # Overwrite labels/ color palette (operator `$<-`):
#' Spectra2a$.color[1] <- "red"
#' hyGet_palette(Spectra1)
#'     #>  Warning message:
#'     #>  In hyGet_palette(Spectra2a) : Values of pallete do not exist.
#'     #>  Most probably they are overwriten by operation `$.color<-`.
#'
#' labels(Spectra1,".color")
#'     #>  ".color"
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
hyGet_palette <- function(sp){
    pal <- hyGet_palette0(sp)  %>% spPlot::palette2vec()
    if (!is.null(pal)) as.character(pal) else pal
}


#' @rdname hyGet_palette
#' @export
hyGet_palette0 <- function(sp){
    if (class(sp) == "hyperSpec") {

        # Palette <- labels(sp,".color")
        Palette <- attributes(sp$.color)$palette

        # Check for manually added colors
        Unique_colors <- unique(sp$.color)
        whichColors   <- Unique_colors %in% Palette[[1]]

        if (is.null(Palette)){
            if (is.null(Unique_colors)) {
                # Palette <- NULL
                return(NULL)
            } else {
                Palette <- data.frame(colors = Unique_colors,
                                      labels = rep(NA,length(Unique_colors)),
                                      stringsAsFactors = FALSE)

            }
        } else { # if palette is NOT missing
            if (!all(whichColors)) {
                warning(sprintf("Vector `.color` contains more unique colors "  %++%
                                    "than listed in the palette. \n" %++%
                                    "Newly added colors are: %s.",
                                paste(Unique_colors[!whichColors],
                                      collapse = ", ")))
            }
        }

        if (length(Palette[[1]]) == 1) warning("Only 1 unique color was found!")
        return(Palette)


        # # This `if` can be removed:
        # if (any(Palette == ".color")) {
        #     warning(paste0("Values of pallete do not exist.\n",
        #                    "Most probably they are overwriten by operation `$.color<-`."))
        #     invisible(NULL)
        #
        # } else {
        #     if (length(Palette[[1]]) == 1) warning("Only 1 unique color was found!")
        #     return(Palette)
        # }
    } else {
        return(NULL)
    }
}
