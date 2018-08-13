#' Calculate a statistic
#'
#' ### NEBAIGTA ###
#'
#'
#' @param obj hyperSpec object.
#' @param FUN a function that takes a vector and results in a single number,
#'            e.g., mean, median, etc.
#' @param label (string) a label for function to be used as column name.
#'
#' @return
#' @export
#'
#' @examples
#'
#' rez <- spStat_ci(Spectra2)
#' rez
#'
#' qplotspc(rez)
#'
#' ggplot(Spectra2) + geom_line()
#'
#'  data <- hy_spc2df(rez)
#'  ggplot(data,
#'         aes_string(x = "wl",
#'                    y = names(data)[3],
#'                    ymin = "ci_lower",
#'                    ymax = "ci_upper")
#'  ) +
#'      geom_ribbon(alpha = 0.2) +
#'      geom_line(linetype = 2)
#'
#' ggplot(Spectra2[1:20,,300~400])
#'
#' hyperSpec::aggregate(Spectra2, by = "gr", FUN = mean)

spStat_ci <- function(obj,
                      FUN = mean,
                      label = as.character(match.call()$FUN) %if_null_or_len0% "mean"){

    spc_data <- obj[[]] # Extract matrix of spectra
    n <- ncol(spc_data)

    # Initialize data frame for bootstrap confodence interval
    ci_results <- data.frame("ci_lower" = numeric(0),
                             "mean"     = numeric(0),
                             "ci_upper" = numeric(0)
    )
    names(ci_results)[2] <- label

    # do CI calculation
    for (i in 1:n){
        x <- spc_data[,i]
        ci_results[i,] <- boot_ci_fun(x, FUN)
    }

    ci_results %<>% t()

    estimates <- rownames(ci_results)

    sp_obj <- new("hyperSpec",
                  spc  = ci_results,
                  data = data.frame(estimate = factor(estimates, estimates)),
                  wavelength = wl(obj),
                  labels = labels(obj)
    )

    return(sp_obj)
}



# =============================================================================
#' @rdname spStat_ci
#' @inheritParams boot_ci_mean
#' @export
#'
#' @examples
#'
#'
#' set.seed(1)
#' amzius   <- rnorm(nrow(Spectra2))
#' spektrai <- Spectra2[,,400~500]
#'
#' set.seed(1)
#' rez <- spStat_ci_corr(spektrai, y = amzius)
#' ggplot_ci_rez(rez)
#'
spStat_ci_corr <- function(obj,
                           y = NULL,
                      FUN = mean,
                      method = c("spearman","kendall","pearson")[1],
                      use = "everything",
                      conf = 0.95,
                      R    = 1e3,
                      sim  = "balanced",
                      type = c("norm"),
                      label = paste0(spMisc::fCap(method), "'s corr. coeff.")){

    spc_data <- obj[[]] # Extract matrix of spectra
    n <- ncol(spc_data)

    # Initialize data frame for bootstrap confodence interval
    ci_results <- data.frame("ci_lower" = numeric(0),
                             "corr_coef"= numeric(0),
                             "ci_upper" = numeric(0)
    )
    names(ci_results)[2] <- label

    # do CI calculation
    for (i in 1:n){
        x <- spc_data[,i]
        ci_results[i,] <- boot_ci_corr(x,y,
                                       method = method,
                                       use  = use,
                                       conf = conf,
                                       R    = R,
                                       sim  = sim,
                                       type = type
       )
    }

    ci_results %<>% t()

    estimates <- rownames(ci_results)

    sp_obj <- new("hyperSpec",
                  spc  = ci_results,
                  data = data.frame(estimate = factor(estimates, estimates)),
                  wavelength = wl(obj),
                  labels = labels(obj)
    )
    hyperSpec::labels(sp_obj, "spc") <- label

    return(sp_obj)
}

# =============================================================================
#' @param rez a rezult of function \code{spStat_ci_corr} or \code{spStat_ci}
#'
#' @rdname spStat_ci
#' @export
#'
#'
ggplot_ci_rez <- function(rez, linetype = 1){
    data <- hy_spc2df(rez)
    ggplot(data,
           aes_string(x = "wl",
                      y = names(data)[3],
                      ymin = "ci_lower",
                      ymax = "ci_upper")
    ) +
        geom_hline(yintercept = c(-1,0,1),
                   lty = 2,
                   color = "red",
                   alpha = .4) +
        geom_ribbon(alpha = 0.2) +
        geom_line(linetype = linetype)+
        geom_point(size = 1,
                   alpha = 0.1)+
        xlab(hyperSpec::labels(rez)$.wavelength) +
        ylab(hyperSpec::labels(rez)$spc) +
        ggtitle("Correlation coeff. with bootstrapped CI") +
        spPlot::ggLims(c(-1,1))
}
