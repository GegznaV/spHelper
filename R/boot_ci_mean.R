#' Bootstrapped mean and its confidence interval
#'
#' Function calculates bootstrapped mean (or other function)
#' and its confidence interval for a vector \code{x}.
#'
#' \code{boot_ci_correlation} calculates confidence interval for correlation
#' coefficient between vectors \code{x} and \code{y}.
#'
#' @param x a vector.
#' @param y a vector.
#' @param FUN a function, that takes a vector returns one number, e.g. mean, median, etc.
#' @param label (string) a label for function to be used as column name.
#'
#' @inheritParams boot::boot
#' @inheritParams boot::boot.ci
#' @inheritParams stats::cor
#'
#'
#' @return A data frame with bootstrapped mean and its confidence interval.
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' x <- rnorm(1000, mean = .5, sd = .1)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' set.seed(1)
#' boot_ci_mean(x)
#'
#' #       ci_lower  mean     ci_upper
#' #  1   0.4923028 0.4988352 0.5053676
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' set.seed(1)
#' boot_ci_fun(x, IQR)
#'
#' #     ci_lower       IQR   ci_upper
#' # 1   0.1307229 0.1385801 0.1486593
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' set.seed(1)
#'
#' MeDiAn <- median
#' boot_ci_fun(x, MeDiAn, name = "m")
#'
#' #     ci_lower    median ci_upper
#' # 1 0.4900485 0.4964676 0.502184


boot_ci_mean <- function(x,
                         conf = 0.95,
                         R    = 1e3,
                         sim  = "balanced",
                         type = c("norm")){
    boot_ci_fun(x,
                FUN  = mean,
                conf = conf,
                R    = R,
                sim  = sim,
                type = type)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname boot_ci_mean
#' @export

boot_ci_fun <- function(x,
                        FUN,
                        conf = 0.95,
                        R    = 1e3,
                        sim  = "balanced",
                        type = c("norm"),
                        label = as.character(match.call()$FUN)
)
{
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    stopifnot("norm" %in% type) # only "norm" is supported
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    v_boot <- boot::boot(x,
                         statistic = function(x,i)
                             FUN(x[i], na.rm = TRUE),
                         R = R,
                         sim = sim
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    v_boot_ci <- boot::boot.ci(v_boot, conf = conf, type = type)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ("norm" %in% type) {
        result <- data.frame(
            "ci_lower" = v_boot_ci$normal[2],
            "estimate" = v_boot$t0,
            "ci_upper" = v_boot_ci$normal[3]
        )

        colnames(result)[2] <- label
        return(result)
    }
}


# =============================================================================
#' @rdname boot_ci_mean
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(30)
#' y <- x - rnorm(30) + runif(30,-2,2)
#' plot(x,y)
#'
#' set.seed(1)
#' boot_ci_correlation(x,y)
#'
#' #    ci_lower corr_coef  ci_upper
#' #   0.4263133 0.6258065 0.8571043
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'
#' df <- data.frame(x,y)
#' set.seed(1)
#' boot_ci_correlation(df)
#'
boot_ci_corr <- function(x,
                        y = NULL,
                        method = c("spearman","kendall","pearson")[1],
                        use = "everything",
                        conf = 0.95,
                        R    = 1e3,
                        sim  = "balanced",
                        type = c("norm"),
                        label = "corr_coef"
)
{
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.data.frame(x)) {
        if (ncol(x) != 2) stop("Number of columns in `x` must be 2")
        df <- x[,1:2]
    } else {
        df <- data.frame(x = x, y = y)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    stopifnot("norm" %in% type) # only "norm" is supported
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    v_boot <- boot::boot(df,
                         statistic = function(df,i)
                             cor(df[i,1],df[i,2],
                                 use = use,
                                 method = method),
                         R = R,
                         sim = sim
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    v_boot_ci <- boot::boot.ci(v_boot, conf = conf, type = type)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ("norm" %in% type) {
        result <- data.frame(
            "ci_lower" = v_boot_ci$normal[2],
            "estimate" = v_boot$t0,
            "ci_upper" = v_boot_ci$normal[3]
        )

        colnames(result)[2] <- label
        return(result)
    }
}
# =============================================================================
