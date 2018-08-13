#' @title [!.] Plot a cross-tabulation (classification table)
#'
#' @description Plot a crosstabulation (classification table)
#'
#' @param tabl A classification table, crosstabulation: either an object of
#'             a class "table" or a square matrix.
#' @template labels
#' @template subtitle
#'
#' @param sort Default is \code{FALSE} matrix is plotted as is.
#' Other options: \cr
#'        Either \code{"maxOnDiag"} or \code{TRUE} - function
#'        \code{\link{sort_descOnDiag}} is applied (rows and columns are sorted
#'        in so that maximum values of frequencies were on diagonal, if it's
#'        possible);\cr
#'         \code{"rowSums"} - sort rows by row sums;\cr
#'         \code{"colSums"} - sort columns by column sums;\cr
#'         \code{"colMax"}  - sort rows in respect only to maximum value in
#'         each row;\cr
#'         \code{"rowMax"}  - sort columns in respect only to maximum value in
#'         each column.
#'
#' @param bg.color (THIS PARAMETER DO NOT WORK if \code{as.percentage} is not
#' \code{FALSE}.)
#'  The main background color (if \code{shade = TRUE}, this color
#'  is used for cells with high values).
#' @param low.color A background for low values.
#' @param max.color A background for maximum values.
#' @param text.size The size of text inside cells.
#' @param decimals The number of decimal positions in rounding. Default is 2
#'        (i.e., precission is 0.01).
#'
#' @param show.max The rule to highlighted top values. Possible entries:\cr
#'    \code{"max"} highlight maximum of whole matrix;\cr
#'    \code{"colMax"} (default) highlight maxima of every column ;\cr
#'    \code{"rowMax"} highlight maxima of every row;\cr
#'    \code{FALSE} maxima are \bold{not} highlighted.
#'
#' @param shades Logical. If \code{TRUE}, color of a cell varies depending
#'       not on its value (except the cells with top values if \code{show.max}
#'       is \code{FALSE}).
#' @param guide A type of guide to display: either \code{"legend"},
#'        \code{"colorbar"} or \code{FALSE} (if no guide is needed).
#'
#' @param zero.color Color of text inside cells, that have value equal to zero.
#' @param  Color of text inside cells, that have value not equal to zero.
#' @param  as.percentage Show result as percentage:\cr
#'  \code{TRUE} - show as fraction of total sum;\cr
#'  \code{FALSE} (default) - show as frequency/counts;\cr
#'  \code{"col"} column-wise persentage (sum of values of each column is 100\%);\cr
#'  \code{"row"} row-wise persentage (sum of values of each row is 100\%).
#'
#'
#' @template ggplot
#' @examples
#'
#'  # BUG for this data: when 'sort = TRUE',
#'  #              IPPN LSIL HSIL
#'  #  H.Cluster 1    0    0  111
#'  #  H.Cluster 2    0    0   45
#'  #  H.Cluster 3    0    0   49
#'  #  H.Cluster 4    0    0    5
#'
#' library(spHelper)
#'
#'
#' # Generate data: Random guess  ============================
#'  N <- 1000 # number of observations
#'
#' Prediction <- sample(1:4, N, replace = TRUE)
#' Reference  <- sample(c("A", "B","C","D","E"),N, replace = TRUE)
#'
#' tabl <- table(Prediction,Reference)
#' qplot_crosstab(tabl)
#' qplot_crosstab_sort(tabl)   # different order of columns and rows
#' qplot_crosstab0(tabl)       # no colors
#' qplot_crosstab0s(tabl)      # no colors, different order of columns and rows
#'
#'
#' qplot_crosstab(Prediction, Reference, as.percentage = FALSE, sort = TRUE)
#' qplot_crosstab(Prediction, Reference, as.percentage = "row", sort = TRUE)
#' qplot_crosstab(Prediction, Reference, as.percentage = "col", sort = TRUE)
#' qplot_crosstab(Prediction, Reference, as.percentage = TRUE, sort = TRUE)
#'
#' @export
#' @seealso To plot classification table (confusion matrix) use
#'  \code{\link{qplot_confusion}}.
#'
#' @family \pkg{spHelper} plots
#' @author Vilmantas Gegzna

qplot_crosstab <- function(obj,...){
    UseMethod("qplot_crosstab")
}

#  ------------------------------------------------------------------------

#' @rdname qplot_crosstab
#' @method qplot_crosstab table
#' @export
qplot_crosstab.table <- function(tabl,
                                 Title  = "Cross-tabulation",
                                 xLabel = NULL,
                                 yLabel = NULL,
                                 subTitle = NULL,
                                 text.size = 5,
                                 sort = FALSE,
                                 bg.color  = "skyblue1",# conrollable nonly if as.percentage is FALSE
                                 max.color = "gold3",
                                 low.color = "grey80",
                                 decimals = 1,
                                 show.max = c("max","colMax", "rowMax",   FALSE),
                                 shades = TRUE,
                                 guide = c("legend","colorbar",FALSE),
                                 zero.color = "grey60",
                                 text.color = "black",
                                 as.percentage = c(FALSE,TRUE,"row", "col"), ...) {


   #  ------------------------------------------------------------------------
    force(sort)

    # Transpose to show x and y labels correctly
    tabl  %<>% t

    # Make proportions ********************************************************
    switch(as.percentage[1] %>% as.character  %>% toupper,
           "TRUE" = {
               tabl <- prop.table(tabl)*100;
               subTitle <-  ifFALSE(subTitle, NULL, "Percentage");
               LEGENDS_title <- "Percentage";
               bg.color  = "aquamarine2";
               REFlines <- NULL;
           },
           "ROW"  = {
               tabl <- prop.table(tabl, 1)*100;
               subTitle <-  ifFALSE(subTitle, NULL, "Row-wise percentage");
               LEGENDS_title <- "Percentage";
               bg.color  = "green3";
               yintercepts <- 1:(nrow(tabl)-1) + 0.5
               REFlines <- geom_hline(yintercept = yintercepts,
                                      lty = 1, lwd = .8);
           },
           "COL"  = {
               tabl <- prop.table(tabl, 2)*100;
               subTitle <-  ifFALSE(subTitle, NULL, "Column-wise percentage");
               LEGENDS_title <- "Percentage";
               bg.color  = "darkolivegreen2";
               xintercepts <- 1:(ncol(tabl)-1) + 0.5
               REFlines <- geom_vline(xintercept = xintercepts, lty = 1,
                                      lwd = .8);
           },
           # Otherwise
           {
               subTitle <-  ifFALSE(subTitle, NULL, subTitle)
               LEGENDS_title <- "Frequency/Counts";
               bg.color  = bg.color;
               REFlines <- NULL;
          }
    )

    # Round *****************************************************************
    tabl_a   <- round(tabl,decimals)

    # Sort: put maxima on diagonal, if possible *****************************
    tabl_a <- switch(as.character(sort),
                     "TRUE"        = sort_descOnDiag(tabl_a),
                     "maxOnDiag"   = sort_descOnDiag(tabl_a),
                     "colMax"      = sort_colMax(tabl_a),
                     "rowMax"      = sort_rowMax(tabl_a),
                     "colSums"     = sort_colSums(tabl_a),
                     "rowSums"     = sort_rowSums(tabl_a),
                     tabl_a) #default value

    # Make a long format data frame *****************************************
    tabl_m <- reshape2::melt(tabl_a)


    # Convert first two variables to factors ********************************
    # And sort levels to plot data correctly ********************************
    tabl_m[[1]] <- factor(tabl_m[[1]], levels = rev(rownames(tabl_a)))
    tabl_m[[2]] <- factor(tabl_m[[2]], levels =     colnames(tabl_a))

    # # Determine cell text COLORS *******************************************
    ColValue <- rep(TRUE, length(tabl_a))
    ColValue <- as.factor(tabl_m$value != 0)
    tabl_m$ColValue <- ColValue

    # # Determine cell FILL colors *******************************************
    FillValue   <- rep(.8,length(tabl_a))

    # Shades
    if (shades == TRUE) {
        FillValue[] <- scales::rescale(tabl_m$value, to = c(0, .8))
        gBreaks0 = c(.8, .4, 0)
        gLabels0 = c("High",
                     "Intermediate",
                     "Low")
    } else {
        FillValue[] <- .8
        gBreaks0 = NULL
        gLabels0 = NULL

    }

    # Max value
    gBreaksMax = NULL
    gLabelsMax = NULL

    switch(show.max[1],

           colMax = {
               indF <- which.max.perCol(tabl_a)
               FillValue[indF] <- 1
               gBreaksMax = c(.95)
               gLabelsMax = c("Maximum (per column)")
           },

           rowMax = {
               indF <- which.max.perRow(tabl_a)
               FillValue[indF] <- 1
               gBreaksMax = c(.95)
               gLabelsMax = c("Maximum (per row)")
           },

           max    = {
               indF <- which.max.all(tabl_a)
               FillValue[indF] <- 1
               gBreaksMax = c(.95)
               gLabelsMax = c("Maximum")
           }
    )

    # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    tabl_m$FillValue <- FillValue
    gBreaks <- c(gBreaksMax, gBreaks0)
    gLabels <- c(gLabelsMax, gLabels0)

    if (any(is.null(gBreaks),is.null(gLabels))) guide <- FALSE
    # ***********************************************************************
    #  Plot   ***************************************************************
    nameX <- names(tabl_m)[2]
    nameY <- names(tabl_m)[1]

    p <- ggplot(tabl_m, aes_string(x = nameX, y = nameY))

    p <- p + theme_bw()

    p <- p +
        geom_tile(aes(fill = FillValue), colour = "grey50") +
        geom_text(aes(label = value, colour = ColValue), size = text.size) +
        scale_color_manual(values = c("FALSE" = zero.color, "TRUE" = text.color),
                           guide = FALSE ) +

        labs(title = subt(Title, subTitle) ,
             x = {if (is.null(xLabel)) nameX else xLabel},
             y = {if (is.null(yLabel)) nameY else yLabel}
        ) +

        # p <- p +
        scale_fill_gradientn(colours = c(low.color, bg.color, max.color, max.color),
                             values  = c(0,.8,.93,1),
                             limits = c(0,1),
                             na.value = "grey60",
                             guide = guide[1],
                             name = LEGENDS_title,
                             breaks = gBreaks,
                             labels = gLabels ) +
        REFlines

    return(p)
}


#  ============================================================================
#' @rdname qplot_crosstab
#' @method qplot_crosstab numeric
#' @export
qplot_crosstab.numeric <- function(varX, varY,
                                      Title  = "Cross-tabulation",
                                      # xLabel = NULL,
                                      # yLabel = NULL,
                                      xLabel = match.call()$varX,
                                      yLabel = match.call()$varY,
                                      ...
){

    tabl <- table(varX, varY)
    qplot_crosstab(tabl,
                   Title = Title,
                   xLabel = xLabel,
                   yLabel = yLabel,
                   ...)
}

#' @rdname qplot_crosstab
#' @method qplot_crosstab hyperSpec
#' @export
qplot_crosstab.hyperSpec <- function(obj, varX, varY,
                                      Title  = "Cross-tabulation",
                                      xLabel = NULL,
                                      yLabel = NULL,
                                      ...
){
    force(obj)

    # 2 lines might have bug:
    if (is.null(xLabel)) xLabel = hyperSpec::labels(obj,varX[1])
    if (is.null(yLabel)) yLabel = hyperSpec::labels(obj,varY[1])

    obj <- obj$.
    varX <- getVarValues(varX, obj)
    varY <- getVarValues(varY, obj)

    tabl <- table(varX, varY)
    qplot_crosstab(tabl,
                   Title = Title,
                   xLabel = xLabel,
                   yLabel = yLabel,
                   ...)
}

#' @rdname qplot_crosstab
#' @method qplot_crosstab data.frame
#' @export
qplot_crosstab.data.frame <- function(obj, varX, varY,
                                      Title  = "Cross-tabulation",
                                      xLabel = match.call()$varX,
                                      yLabel = match.call()$varY,
                                      ...
                                      ){

    varX <- getVarValues(varX, obj)
    varY <- getVarValues(varY, obj)

    tabl <- table(varX, varY)
    qplot_crosstab(tabl,
                   Title = Title,
                   xLabel = xLabel,
                   yLabel = yLabel,
                   ...)
}

#' @rdname qplot_crosstab
#' @method qplot_crosstab matrix
#' @export
qplot_crosstab.matrix <- function(obj, Title  = NULL, ...){
    # Transpose, at it will be again transposed in .table method
    tabl <- as.table(obj) %>% t
    qplot_crosstab(tabl, ...)
}


#  ------------------------------------------------------------------------


#' @rdname qplot_crosstab
#' @export
qplot_crosstab_sort <- function(...,
                                sort = "maxOnDiag",
                                show.max = TRUE,
                                shades   = TRUE)
{
    qplot_crosstab(...,
                   sort = sort,
                   show.max = show.max,
                   shades   = shades)
}


#  ------------------------------------------------------------------------


#' @rdname qplot_crosstab
#' @template same
#'
#' @export

qplot_crosstab0 <- function(tabl,
                            ...,
                            sort = FALSE,
                            bg.color  = "wheat2",
                            decimals = 2,
                            show.max = FALSE,
                            shades   = FALSE,
                            guide    = FALSE) {
    qplot_crosstab(tabl,
     ...,
     bg.color  = bg.color,
     decimals  = decimals,
     show.max  = show.max,
     shades    = shades,
     guide     = guide)
}

#  ------------------------------------------------------------------------


#' @rdname qplot_crosstab
#' @export
qplot_crosstab0s <- function(tabl,
                             ...,
                             Title  = "Cross-tabulation",
                            xLabel = NULL,
                            yLabel = NULL,
                            subTitle = NULL,
                            text.size = 5,
                            sort = "maxOnDiag",
                            bg.color  = "wheat2",
                            decimals = 2,
                            show.max = FALSE,
                            shades   = FALSE,
                            guide    = FALSE) {
    qplot_crosstab(tabl,
                   ...,
                   Title  = Title,
                   xLabel = xLabel,
                   yLabel = yLabel,
                   subTitle = subTitle,
                   text.size = text.size,
                   sort  = sort,
                   bg.color  = bg.color,
                   decimals = decimals,
                   show.max = show.max,
                   shades   = shades,
                   guide    = guide)
}

#  ------------------------------------------------------------------------
