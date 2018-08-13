#' [!] Plot crosstab for 2 variables in hyperSpec object
#'
#' @template sp-hy
#' @param x_var, y_var names of variables inside \code{sp}, that contain
#'                   information to be crosstabulated.
#' @param xLabel, yLabel labels for variables \code{x_var, y_var} respectively.
#' @param ID_var name of variable inside \code{sp}, that contain ID of sample.
#' @param rotate.x.tick.labels Determine if x axis tick labels should be rotated
#'      in 45 degrees angle (to avoid overlapping). Possible valued \code{TRUE},
#'      \code{FALSE} or \code{NULL} (default, rotate if maximum length of x tick
#'       label is more that 3 symbols).
#' @inheritParams base::table
#' @return 2 ploted crosstabs: first for number of medical samples,
#'  second for number of spectra.
#' @export
#'
#' @examples
#' library(spHelper)
#'
#' gg_crosstab2(Spectra2,"gr","class", ID_var = NULL)
#'
#'
#' Spectra2$gr3<- c("a123", "b123")
#' gg_crosstab2(Spectra2,"gr3","class", ID_var = NULL)
#'
gg_crosstab2 <- function(sp, x_var, y_var,
                         xLabel = labels(sp, x_var),
                         yLabel = labels(sp, y_var),
                         ID_var = "ID",
                         rotate.x.tick.labels = NULL,
                         useNA = "ifany") {
    #-----------------------------------------------------------------------
    force(xLabel)
    force(yLabel)
    #-----------------------------------------------------------------------
    qcrosstab <- function(x, y, subTitle, bg.color, rotate.x.tick.lab)
    {
        p <- qplot_crosstab0(table(x,y, useNA = useNA),
                        xLabel = xLabel,
                        yLabel = yLabel,
                        subTitle = subTitle,
                        bg.color = bg.color)

         if (rotate.x.tick.lab == TRUE){
            p <- p +  theme(axis.text.x  = element_text(angle = 45,
                                              hjust = 1,
                                              vjust = 1)
            )
         }
        return(p)
    }
    #-----------------------------------------------------------------------
    # Check existance and validity of "ID_var"
    if (is.null(ID_var)) {
        UniqueIDData <- sp
    } else {
        if (!(ID_var %in% colnames(sp))) {
            stop(sprintf("Variable ID_var = '%s' indicated icorrectly.", ID_var))
        } else {
            ID <- eval_glue("sp$`{ID_var}`")
            UniqueIDData <- sp[!duplicated(ID),,]; rm(ID)
        }
    }
    #-----------------------------------------------------------------------
    # Check validity of "x_var" and "y_var"
    if (!all(c(x_var, y_var)  %in%  colnames(sp)))
    {
        stop(sprintf("Either variable x_var = '%s' or y_var = '%s' indicated icorrectly. " %.+.%
                     "Must be %s."
                     , x_var, y_var, paste(colnames(sp), collapse = ", ")))
    }
    #=======================================================================    # For non unique IDs
    x <- eval_glue("UniqueIDData$`{x_var}`")
    y <- eval_glue("UniqueIDData$`{y_var}`")

    # Determine, if x tick label rotation is needed
    MAX.num.of.symbols.allowed <- 3
    MAX.num.of.symbols         <- (x %>%
                                       as.factor() %>%
                                       levels() %>%
                                       sapply(nchar) %>%
                                       max())

    rot <- if (is.null(rotate.x.tick.labels))
        {
            if (MAX.num.of.symbols <= MAX.num.of.symbols.allowed)
            {
                FALSE # short label, do not rotate
            } else {
                TRUE # long label, rotate
            }
        } else {
        as.logical(rotate.x.tick.labels)
        }
    #-----------------------------------------------------------------------
    plot1 <- qcrosstab(x, y, "Medical specimens", "#F7F6A8", rot)
    #-----------------------------------------------------------------------
    rm(x,y,UniqueIDData)
    #=======================================================================
    # For non unique IDs
    x <- eval_glue("sp$`{x_var}`")
    y <- eval_glue("sp$`{y_var}`")

    plot2 <- qcrosstab(x, y, "Spectra", "wheat3", rot)

    #-----------------------------------------------------------------------
    # Plot two ggplot2 graphs side by side
    grid.newpage()
    grid.draw(cbind(ggplotGrob(plot1), ggplotGrob(plot2),
                    size = "last"))
    #-----------------------------------------------------------------------
}
