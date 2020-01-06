# ***** Plot a confusion matrix ***** --------------------------------------
#
#' @name qplot_confusion
#'
#' @title [!+] Plot a confusion matrix (a.k.a. classification table)
#'
#' @description
#' Plot a confusion matrix (classification table) with
#' additional statistics (sensitivity (Se) a.k.a. true positive
#' rate, positive predictive value (PPV) and Cohens' Kappa (k)). \cr
#'
#' Colors in the main matrix: \bold{diagonal} cells vary from grey to
#' \bold{green} and \bold{offdiagonal} elements vary from grey to
#' \bold{red}. The color intensity in certain cell is determined by
#' parameter \code{shades}.\cr
#'
#' Colors of cells with statistics Se, PPV and k vary depening on
#' values of these statistics from \bold{red} (\emph{low} values) to
#' \bold{grey} (\emph{middle} values), to \bold{green} (\emph{high} values).\cr
#'
#' \bold{Exception:} when \code{shades="const"} and \code{shades="none"},
#' color intensities are constant.
#'
#' @param ... Appropriate parameters (described below).
#' @param conf,mat A confusion matrix (classification table): either an object
#'        of a class "table" or a square matrix.
#' @param Prediction A factor variable with \bold{predicted} groups.
#' @param Reference  A factor variable with \bold{reference} groups.
#' @template labels
#' @template subtitle
#' @param shades A function how intensities of cell colors in the main confsion
#'        matrix depend on cell values:
#'      \describe{
#'          \item{"prop"}{An intensity of a cell color is proportianal
#'                to a value in the cell. Distribution of color intensities is
#'                 balanced according to number of classes (but not balanced
#'                 according to number of observations per class.)}
#'          \item{"max"}{Cell with the absolute maximum value is represented by the
#'          most intensive color. Other cells are proportionally less intensive.
#'          }
#'          \item{"const"}{Constant red and green colors.}
#'          \item{"none"}{All cells are grey.}
#'      }
#' @param guide Logical. If \code{TRUE}, a legend is plotted.
#' @param text.size The size of a text inside cells.
#' @param decimals The number of decimal positions in rounding. Default is 2
#'        (i.e., precission is 0.01).
#' @param TPR.name The name of the row with true positive rate (TPR).
#'        Default is "<TPR>". It is the same as "<Sensitivity>".
#' @param PPV.name The name of the column with positive predictive value (PPV).
#'        Default is "<PPV>".
#'
#' @param sort A way to sort columns and rows of output matrix. Options to sort
#'  values in descending order:\cr
#'  \code{FALSE} (default) do not sort;\cr
#'  \code{"diagonal"} - sort values on diagonal;\cr
#'  \code{"PPV"} - sort positive predictive values (PPV);\cr
#'  \code{"TPR"} - sort true positive rate (TPR) values.
#'
#' @param metric The metric of overall accuracy. Curently supported values are:\cr
#'  \code{"kappa"} (default) to use Cohen's kappa, in table denoted with symbol \bold{"k"};\cr
#'  \code{"weighted.kappa"}  to use weighted kappa, (symbol \bold{"w"});\cr
#'  \code{"meanTPR"} Mean of true positive rates of each group (symbol \bold{"t"});\cr
#'  \code{"meanPPV"} Mean of positive predictive values of each group (symbol \bold{"v"}).
#'
#'
#' @return A plot of confusion matrix and additional statistics (`ggplot` object).
#' @examples
#'
#' # Generate data: Random guess  ============================
#'  N <- 1000 # number of observations
#'
#' Prediction <- sample(c("A","B","C","D"), N, replace = TRUE)
#' Reference  <- sample(c("A", "B","C","D"),N, replace = TRUE)
#'
#' # This function:
#' qplot_confusion(Prediction, Reference)
#'
#' # does the same as:
#' conf <- table(Prediction,Reference)
#' qplot_confusion(conf)
#'
#' # At least 50% of the cases agree =========================
#' ind <- sample(1:N,round(0.50*N))
#' Reference[ind] <- Prediction[ind]
#' conf2 <- table(Prediction,Reference)
#'
#' qplot_confusion(conf2)
#'
#' # Most of the cases agree =================================
#' ind <- sample(1:N,round(N*.80))
#' Reference[ind] <- Prediction[ind]
#' conf3 <- table(Prediction,Reference)
#'
#' qplot_confusion(conf3)
#'
#' # Proportions =============================================
#'
#' qplot_confusion(conf3)
#'
#' # Shades: proportional =====================================
#'
#' qplot_confusion(conf,shades = "prop",  subTitle = "shades: 'prop', correct by chance")
#' qplot_confusion(conf,shades = "max",   subTitle = "shades: 'max', correct by chance")
#'
#' qplot_confusion(conf2,shades = "prop", subTitle = "shades: 'prop', correct >50%")
#' qplot_confusion(conf2,shades = "max",  subTitle = "shades: 'max', correct >50%")
#'
#' qplot_confusion(conf3,shades = "prop", subTitle = "shades: 'prop', correct >80%")
#' qplot_confusion(conf3,shades = "max",  subTitle = "shades: 'max', correct >80%")
#'
#' # Shades: constant and none ================================
#'
#' qplot_confusion(conf3,shades = "const",subTitle = "shades: constant")
#' qplot_confusion(conf3,shades = "none", subTitle = "shades: none")
#'
#'
#' @export
#' @family \pkg{spHelper} plots
#' @author Vilmantas Gegzna
qplot_confusion <- function(...) {
    UseMethod("qplot_confusion")
}

#  ------------------------------------------------------------------------
#' @rdname qplot_confusion
#' @method qplot_confusion default
#' @export
qplot_confusion.default <- function(Prediction, Reference,
                                    Title     = "Classification table",
                                    xLabel    = NULL,
                                    yLabel    = NULL,
                                    subTitle  = NULL,
                                    shades    = c("prop", "max", "const", "none"),
                                    guide     = FALSE,
                                    text.size = 5,
                                    decimals  = 2,
                                    ...) {

    if (length(Prediction) != length(Reference)) {
        stop("Lengths of vectors `Prediction` and `Reference` must be equal.")
    }
    conf <- table(Prediction, Reference)
    qplot_confusion(conf,
                    Title, xLabel, yLabel, subTitle, shades, guide, text.size,
                    decimals,...)
}
#  ------------------------------------------------------------------------
#' @rdname qplot_confusion
#' @method qplot_confusion matrix
#' @export
qplot_confusion.matrix <- function(mat, ...){
    dims <- dim(mat)
    if (dims[1] != dims[2]) stop("Matrix 'mat' must be square.")
    conf <- as.table(mat)
    qplot_confusion(conf, ...)
}
#  ------------------------------------------------------------------------
#' @rdname qplot_confusion
#' @method qplot_confusion ResampleResult
#' @export

qplot_confusion.ResampleResult <- function(obj, ...) {
    # for MLR
    qplot_confusion(obj$pred, ...)
}

#  ------------------------------------------------------------------------
#' @rdname qplot_confusion
#' @method qplot_confusion PredictionClassif
#' @export

qplot_confusion.PredictionClassif <- function(obj, ...) {
    # for MLR
    with(
        unclass(obj$data),
        qplot_confusion(response, truth, ...)
    )
}


#  ------------------------------------------------------------------------
#' @rdname qplot_confusion
#' @method qplot_confusion table
#' @export
qplot_confusion.table <- function(conf,
                                  Title     = "Classification table",
                                  xLabel    = NULL,
                                  yLabel    = NULL,
                                  subTitle  = NULL,
                                  shades    = c("prop", "max", "const", "none"),
                                  guide     = FALSE,
                                  text.size = 5,
                                  decimals  = 2,
                                  TPR.name  = "<TPR>",
                                  PPV.name  = "<PPV>",
                                  sort      = c(FALSE, "diagonal", "PPV", "TPR"),
                                  metric    = c("kappa", "weighted.kappa", "meanTPR", "meanPPV")
)
{
    # Accuracy measures ============================================================

    # Calculate accuracy measures TPR and PPV
    TPR <- diag(prop.table(conf,2)) # Sensitivity, TRUE positive rate
    PPV <- diag(prop.table(conf,1)) # "Positive Predictive Value"

    # Sort columns and rows
    #
    `%if<0%` <- function(a, b) {if (length(a) > 0) a else b}

    options <- c("DIAGONAL", "TPR", "PPV")
    sort <- options[pmatch(toupper(sort[1]), options, nomatch = FALSE)] %if<0% "FALSE"
    ind <- switch(
        sort,
        DIAGONAL = conf %>% diag %>% order(decreasing = TRUE),
        TPR      = TPR  %>% order(decreasing = TRUE),
        PPV      = PPV  %>% order(decreasing = TRUE),
        # Otherwise:
        1:(conf %>% diag %>% length)
    )

    # Sort appropriate values
    conf <- conf[ind, ind]
    TPR  <- TPR[ind]       # Sensitivity, TRUE positive rate
    PPV  <- PPV[ind]       # "Positive Predictive Value"

    # Select and calculate overall accuracy measure
    switch(
        tolower(metric[1]),
        kappa = {
            K   <- psych::cohen.kappa(conf)[["kappa"]] # Cohen's Kappa
            ACC_symbol <- 'k'
        },

        weighted.kappa = {
            K   <- psych::cohen.kappa(conf)[["weighted.kappa"]] # Weighted Kappa
            ACC_symbol <- 'w'
        },

        meantpr = {
            K   <- mean(TPR)
            ACC_symbol <- 't'
        },

        meanppv = {
            K   <- mean(PPV)
            ACC_symbol <- 'v'
        },
        stop(sprintf("Accuracy metric '%s' is not supported.", metric[1]))
    )

    # Add accuracy measures to the main matrix/table
    PPV <- c(PPV, K)
    conf_a <- rbind(conf, TPR) %>% cbind(., PPV)

    # Rename TPR and PPV
    dimNam <- dimnames(conf_a)
    dimLen <- sapply(dimNam, length)

    dimnames(conf_a)[[1]][dimLen[1]] <- TPR.name
    dimnames(conf_a)[[2]][dimLen[2]] <- PPV.name
    # ------------------------------------------------------------
    # Transformations ============================================================

    conf_a   <- round(conf_a,decimals)
    # Preserve names of dimensions
    conf_a <- as.table(conf_a)
    names(dimnames(conf_a)) <- names(dimnames(conf))

    # Make a long format data frame *****************************************
    conf_m <- reshape2::melt(conf_a)

    # Sort levels to plot data correctly ************************************
    conf_m[[1]] <- factor(conf_m[[1]], levels = rev(rownames(conf_a)))
    conf_m[[2]] <- factor(conf_m[[2]], levels =     colnames(conf_a))

    # Determine COLORS ============================================================

    N  <- length(conf_a) # number of elemants in the extended matrix
    nr <- nrow(conf)
    nc <- ncol(conf)
    n  <- max(nr, nc);# number of rows/columns in the main matrix

    ind.Se   <- base::setdiff(spMisc::which.in(row, conf_a, nrow(conf_a)), N)
    ind.PV   <- base::setdiff(spMisc::which.in(col, conf_a, ncol(conf_a)), N)
    ind.SePV <- sort(c(ind.Se, ind.PV))
    ind.main <- base::setdiff(1:N, c(ind.SePV, N)) # indices of the main matrix elements
    ind.diag <- base::setdiff(spMisc::which.in.diag(conf_a), N) # ind. of the diag. el. in main matrix

    FillValue <- rep(NA, N)

    accShades <- function(){
        #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        # Rescale Se and PV
        FillValue[ind.SePV] <- scales::rescale(conf_a[ind.SePV],
                                               c(-1, 1),
                                               c(1/n, 1))
        # Rescale Kappa  - - - - - - - - - - - - - - - - - - - - - - -
        FillValue[N] <- scales::rescale(conf_a[N], c(-1, 1), c(0, 1))
        # Correct too small and too high values- - - - - - - - - - - -
        FillValue[FillValue < -1] <- -1
        FillValue[FillValue >  1] <-  1
        #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        return(FillValue)
    }

    switch(shades[1],
           prop =  {# best for square matrix
               FillValue[ind.main] <- -conf_m$value[ind.main] /
                   sum(conf) * n * (n - 1);
               FillValue[ind.diag] <- -FillValue[ind.diag] / (n - 1)
               FillValue <- accShades()
           },

           max = {
               FillValue[ind.main] <- -conf_m$value[ind.main] / max(conf);
               FillValue[ind.diag] <- -FillValue[ind.diag]
               FillValue <- accShades()
           },

           const = {
               FillValue[ind.main]   <- -.60
               FillValue[ind.diag]   <-  .70
               # FillValue[N]          <- 0.10

           },
           # Just constant grey color (no red nor green colors)
           FillValue[] <- 0
    )

    conf_m$FillValue <- FillValue
    # *************************************************************
    # K2 <- substitute(kappa == K, list(K = round(conf_a[N]),2))
    # conf_m$value[N] <- K2

    conf_m$value[ind.SePV] <- sprintf("%.2f", conf_a[ind.SePV])
    conf_m$value[N] <- sprintf("%s=%.2f", ACC_symbol, conf_a[N])

    # # - for TPR and PPV

    #
    # TPR.formated <- sprintf("%.1f",conf_m$value[TPR.ind]*100)
    # PPV.formated <- sprintf("%.1f",conf_m$value[PPV.ind]*100)
    #
    # conf_m$value[TPR.ind] <- TPR.formated
    # conf_m$value[PPV.ind] <- PPV.formated
    #
    # # - for k: add 'ACC_symbol'
    # conf_m$value[N] <- sprintf("%s=%s",ACC_symbol,conf_a[N])

    # Plot ============================================================
    # *************************************************************
    nameX <- names(conf_m)[2]
    nameY <- names(conf_m)[1]

    p <- ggplot(conf_m, aes_string(x = nameX, y = nameY))
    # p <- p + theme_bw()

    p <- p +
        geom_tile(aes(fill = FillValue), colour = "grey50") +
        geom_text(aes(label = value), size = text.size) +
        geom_hline(size = 1.2, color = "grey30", yintercept = 1.5    ) +
        geom_vline(size = 1.2, color = "grey30", xintercept = nc + .5) +

        scale_fill_gradient2(high = "#209D20", # "#008000",
                             mid  = "#eeeeee", #mid = "#f2f6c3",
                             midpoint = 0,
                             low  = "#dd4040",#"tomato2",
                             na.value = "grey60",

                             guide = {if (guide) "colourbar" else FALSE} ,
                             name = "Accuracy",
                             limits = c(-1, 1),
                             breaks = c(1, -1),
                             labels = c("High", "Low")
        ) +

        labs(title = Title,
             subtitle = subTitle,
             x = {if (is.null(xLabel)) nameX else xLabel},
             y = {if (is.null(yLabel)) nameY else yLabel}
        )

    p <- p + scale_x_discrete(position = "top")

    return(p)
}



