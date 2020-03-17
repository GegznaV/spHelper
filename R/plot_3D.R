# ----------------------------------------------------------------------------------------------
#' [.] Plot 3D scatterplot
#'
#' Convenience functions to make 3D scatterplot with \pkg{plotly}.
#'
#' @param data Matrix with 3 columns (size Nx3).
#' @param by (Nx1 vector) grouping variable group with names  for each row in \code{data}.
#' @param text (Nx1 vector) charter vector with text hor each row in \code{data}.
#' @param title The main title of the plot.
#' @param add.space (numeric)  fraction of space to be added (value between 0 and 1). Default is 0.2, i.e. 10\%.
#'
#' @param scene Description in \url{https://plot.ly/r/reference/#scatter3d-scene}
#' specification of parameters in \url{https://plot.ly/r/reference/#layout-scene}.
#'
#' @return \code{plotly} object.
#' @export
#'
#' @examples
#'
#' ## NOT YET INCLUDED
#'
plot_3D <- function(data, by = NULL,
                    text = NULL,
                    title = "3D scatterplot",
                    add.space = .2,
                    scene = list(camera = list(eye = list(x = 1.1, y = -1.1, z = 1))))
{
    if (is.null(by))     by <- rep_len("Data", nrow(data))
    if (is.null(text)) text <- paste("Row",1: nrow(data))

    df <- data.frame(x = data[,1],
                     y = data[,2],
                     z = data[,3],
                     group = by,
                     text = text)

    make_3D(df,title,scene,add.space)
}
# ----------------------------------------------------------------------------------------------
#' @rdname plot_3D
#' @export
#' @param fit An object drim which Nx3 matrix will be extracted. The object is
#'  created with one of the following functions \cr
#' \code{\link[base]{cmdscale}}, \code{\link[MASS]{ISOmap}},
#'             \code{\link[MASS]{sammon}} or similar for \code{mds2mat}();\cr
#' \code{\link[SMACOF]{mds}} in \pkg{SMACOF} for \code{SMACOF2mat}();\cr
#' \code{\link[Rtsne]{Rtsne}} in \pkg{Rtsne} for \code{tSNE2mat}().\cr
#'

mds2mat    <- function(fit){fit$points}

#' @export
#' @rdname plot_3D
SMACOF2mat <- function(fit){fit$conf}

#' @export
#' @rdname plot_3D
tSNE2mat   <- function(fit){fit$Y}
# ----------------------------------------------------------------------------------------------



# FUNCTION for 3D plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(plotly)

#' @param df A data frame with variables named $x, $y, $z, $x,
#'          $text, $group.
#'
#' @export
#' @rdname plot_3D
make_3D <- function(df, title = NULL, scene = list(), add.space = .2){

    # Calculate ranges for each variable + add percentage of space
    # indicated in `add.space`
    range_FUN <- function(x, add.space = .1) {
        r  <- range(x);
        r0 <- abs((r[2] - r[1]))*add.space;
        r2 <- r + c(-r0, +r0)
        return(r2)
    }
    ranges <- lapply(df[,c("x","y","z")], range_FUN,
                     add.space =  add.space)
    scene0 <- list(xaxis = list(range = ranges$x),
                   yaxis = list(range = ranges$y),
                   zaxis = list(range = ranges$z))
    # Update variable "scene"
    scene <- modifyList(scene0, scene)

    # Make plotly graph
    plot_ly(df,
            x = x, y = y, z = z,
            text     = text,
            color    = group,
            type     = "scatter3d",
            mode     = "markers",
            marker   = list(opacity = 0.6, size = 4),
            hoverinfo= "text"
    ) %>%
        layout(title = title, scene = scene)
}
