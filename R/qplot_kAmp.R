# ***** Amplitudes of components ***** -----------------------------------------

# Added parameter `palette`

#' @title [!+] Plot amplitudes (a.k.a scores) of spectroscopic components
#'
#' @description Plot amplitudes (a.k.a scores) of spectroscopic components
#'  grouped by categorical variable \code{by}. Amplitudes are provided as
#' \code{\link[=hyperSpec-class]{hyperSpec}} object.\cr
#'
#' @note If more information on matrix decomposition/factorisation is
#' needed it can be found
#' \href{https://en.wikipedia.org/wiki/Matrix_decomposition}{here} or
#' \href{http://www.r-bloggers.com/matrix-factorization/}{here}).
#'
#' @param scores An object of class \code{\link[=hyperSpec-class]{hyperSpec}}
#' with factor scores, principal component scores, component amplitudes, etc.
#'
#' @template labels
#' @template subtitle
#' @param by A name of grouping variable. If \code{NULL}, all data is ploted.
#'      Default is \code{by = "gr"} for \code{qplot_kAmp} and
#'       \code{by = NULL} for \code{qplot_scores}.
#'
#' @param palette A color palette to be used in plotting. (...)
#'
#' @param add.violin Logical. If \code{TRUE} adds so called violin (i.e.,
#'        symmetrical probarility density) plot.
#'         Default is \code{TRUE} for \code{qplot_kAmp}.
#' @param add.jitter Logical. If \code{TRUE} adds jitter plot.
#'          Default is \code{FALSE}.
#' @param add.boxplot Logical. If \code{TRUE} adds boxplot.
#'         Default is \code{TRUE}.
#' @param violin.alpha Transperency of violin plot.
#' @param jitter.alpha Transperency of jitter plot.
#' @param jitter.size  Point size in jitter plot.
#'
#' @details Plots are drawn in this order: violin plot, jitter plot, boxplot.
#' @template ggplot
#' @examples
#'
#' data(Scores2)
#' qplot_kAmp(Scores2)
#' qplot_scores(Scores2)
#'
#' data(Scores3)
#' qplot_kAmp(Scores3, by = "class")
#'
#' sp3 <- hyAdd_color(Scores3, "class")
#' qplot_kAmp(sp3, by = "class")
#'
#' p <- qplot_scores(Scores2, add.jitter = TRUE)
#' p
#'
#' p + theme_bw()
#'
#' @export
#' @family \pkg{spHelper} plots
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna

qplot_kAmp <- function(scores,
                      Title = "Component amplitudes",
                      subTitle = NULL,
                      xLabel = labels(scores, ".wavelength"),
                      yLabel = labels(scores, "spc"),
                      by = "gr",
                      palette = hyGet_palette(scores),
                      add.violin  = TRUE,
                      add.jitter  = FALSE,
                      add.boxplot = TRUE,
                      violin.alpha = .25,
                      jitter.alpha = .30,
                      jitter.size  = 1)
{
    hyperSpec::chk.hy(scores)
    ## Quotes are not necessary if uncommented:
    # CALL <- match.call()
    # if (!is.null(CALL$by)) {
    #     by <- as.character(c(CALL$by))
    # }

    cNames <- colnames(scores$spc)
    AMP2   <- as.data.frame(scores$spc)
    names(AMP2) <- cNames # paste0("Komp_", names(AMP2))

    if (is.null(by)) {
        scores$Groups <- "All data"
    } else {
       scores$Groups <- as.factor(scores$..[,by])
    }
    if (length(palette) < nlevels(scores$Groups)) {        palette <- NULL    }


    scores <- AMP2   %>%
        cbind(scores$..["Groups"])   %>%
        dplyr::mutate(row = row_number())  %>%
        tidyr::gather(Components, Amplitude, -Groups, -row) %>%
        dplyr::mutate(Components = factor(Components,sort(cNames),sort(cNames)))

    # Plot ====================================================================
    p <- ggplot2::ggplot(scores,
                         aes(y = Amplitude, x = Components, fill = Groups),
                         size = 1
                   )

    if (add.violin == TRUE) { p <- p +  geom_violin(alpha = violin.alpha) }
    if (add.jitter == TRUE) {
        p <- p +
            geom_point(alpha = jitter.alpha, size = jitter.size,
                       position = position_jitterdodge(dodge.width = 0.9))
    }
    if (add.boxplot == TRUE) {
        p <- p +
            geom_boxplot(alpha = .6,
                         position = position_dodge(width = 0.9))
    }

    p <- p +
        facet_grid(~Components, scales = "free") +

        ggtitle(subt(Title,subTitle)) + xlab(xLabel) + ylab(yLabel) +

        theme(axis.ticks.x = element_blank(),
              axis.text.x  = element_blank(),
              # strip.text   = element_blank(),
              # strip.background = element_blank(),
              legend.title = element_blank()              ) +

        geom_hline(yintercept = 0, size = .5,linetype = 2, alpha = .5)

    if (length(palette) > 0) {
        p <- p + scale_fill_manual(values = palette)
    }
    # # If unique values in `Groups`
    if (length(unique(scores$Groups)) == 1) {
        p <- p + guides(fill = FALSE)
        # p <- p + scale_fill_grey()
        }


    return(p)
}
