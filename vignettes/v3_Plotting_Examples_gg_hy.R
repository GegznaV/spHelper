## ----setup, include = FALSE----------------------------------------------
optDEF <- knitr::opts_chunk$get()
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>",
                      fig.width = 6,
                      fig.align = 'center')

library(hyperSpec)
library(spHelper)
library(plotly)

## ----Load packages, message = FALSE, warning = FALSE---------------------
library(hyperSpec)
library(spHelper)
library(plotly)

## ----qplotspc------------------------------------------------------------
data(chondro, package = "hyperSpec")
qplotspc(chondro)

qplotspc(paracetamol, c(2800 ~ max, min ~ 1800)) +
    scale_x_reverse(breaks = seq(0, 3200, 400)) 
# +
#     theme(strip.text = element_blank(),
#           strip.background = element_blank())

qplotspc(hyperSpec::aggregate(chondro, chondro$clusters, mean),
        mapping = aes(x = .wavelength,
                      y = spc,
                      colour = clusters)) +
    facet_grid(clusters ~ .)


qplotspc(hyperSpec::aggregate(chondro, chondro$clusters, mean_pm_sd),
        mapping = aes(x = .wavelength,
                      y = spc,
                      colour = clusters,
                      group = .rownames)) +
facet_grid(clusters ~ .)



## ----qplotc--------------------------------------------------------------
qplotc(flu)

qplotc(flu[,,440])

qplotc(flu) + geom_smooth(method = "lm",  size = .5)


## ----qplotmap------------------------------------------------------------
qplotmap(chondro)
qplotmap(chondro) + scale_fill_gradientn(colours = alois.palette())

## works also with discrete x or y axis:
qplotmap(chondro,
         mapping = aes(x = x,
                       y = as.factor(y),
                       fill = spc)
         )

## ----qplotmixmap---------------------------------------------------------
chondro <- chondro - spc.fit.poly.below(chondro)
chondro <- sweep(chondro, 1, apply(chondro, 1, mean), "/")
chondro <- sweep(chondro, 2, apply(chondro, 2, quantile, 0.05), "-")

qplotmixmap(chondro[,,c(940, 1002, 1440)],
             purecol = c(colg = "red",
                         Phe = "green",
                         Lipid = "blue"))


