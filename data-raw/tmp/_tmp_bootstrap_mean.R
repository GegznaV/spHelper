
library(boot)
library(bootstrap)
library(hyperSpec)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
boot_fun_DT <- function(x, d, FUN = mean, ...) {
    require(data.table)
    x <- as.data.table(x)
    return(x[d, lapply(.SD, mean)])
}

boot_fun <- function(x, d, FUN = mean, ...) {
    return(apply(x[d,], 2, mean, ...))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(865)
b = boot(as.data.frame(Spectra2[[]]),
         boot_fun,
         R    = 1000,
         sim  = "balanced")

# plot(b)
sp_bootstrapped_mean = decomposition(Spectra2, x = b$t0)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

N <- nwl(Spectra2)
b_ci <- matrix(NA, 2, N)

for (n_i in 1:N) {
    b_ci[,n_i] = boot.ci(b,
                         type = c("bca"),
                         conf = 0.95,
                         index = n_i)$bca[4:5]
}
sp_confidence_intervals = decomposition(Spectra2, x = b_ci)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plotspc(sp_confidence_intervals, fill = TRUE, fill.col = "gray")
plotspc(sp_bootstrapped_mean,    col = "red", add = TRUE)


# boot.ci(b, type = c("bca","basic"), index = n_i)$
# b_ci$t0
# b_ci$basic

# The intervals calculated using the adjusted bootstrap percentile (BCa) method.
# b_ci$bca[4:5]


# index = 1:min(2,length(b$t0))
# b$t[index[1]]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- replicate(100, rnorm(9, mean = 80, sd = 5))
dim(data)

bootstrap
