# library(ggplot2)
#
# gg1 <- qplot(cyl, disp, data = mtcars, color = as.factor(cyl)) + ggtitle("1")
# gg2 <- qplot(cyl, disp, data = mtcars, color = mpg) + ggtitle("2")
# gg3 <- qplot(cyl, disp, data = mtcars, color = interaction(cyl,vs)) + ggtitle("3")
# gg4 <- qplot(cyl, disp, data = mtcars, color = as.factor(vs)) + ggtitle("4")
#
# gg <- list(gg1, gg2, gg3, gg4)
#
#
# library(cowplot)
# plot_grid(plotlist = gg, ncol=2, align="vh")
#
#
#
# library(plotly)
# p <- lapply(gg, ggplotly)
# p
#
#
# ps <- subplot(p,
#         margin = 0.05, nrows = 2,
#         shareX = F, shareY = F,
#         titleX = T, titleY = T)  %>% plotly_build
#
# ps %>% str
#
#
#
# # center_subtracted_centers -------------------------------------------------
#
# library(spHelper)
#
# filtSG_n <- 35
# filtSG_p <- 1
# sgFilt <- function(x) signal::sgolayfilt(x = x, n = filtSG_n, p = filtSG_p)
# Sp2 <- apply(Spectra2, 1, sgFilt)
#
# # add colors
# Sp <- hyAdd_color(Sp2, "gr")
# #  ------------------------------------------------------------------------
# qplot_spDistrib(Sp, by = "gr", percent = 50)
# #  ------------------------------------------------------------------------
# MED <- apply(Sp,2,median)
# MAD <- apply(Sp,2,mad)   # median absolute deviation
# #  ------------------------------------------------------------------------
#
# scale(Sp, center = MED, scale = MAD)  %>%
#     center_subtracted_centers(by = "gr")  %>%
#     qplot_sp(names.in = "gr") +
#     ggtitle(subt("CSCs of scaled data","Scaling: x = (x-median)/MAD"))
#
#
# scale(Sp,center = MED, scale = MAD)  %>%
#     center_subtracted_centers(by = "gr",
#                               balanced = TRUE,
#                               balance.FUN = median)  %>%
#     qplot_sp(names.in = "gr") +
#     ggtitle(subt("Balanced median SCs of scaled data",
#                  "Scaling: x = (x-median)/MAD"))
#
# ggplotly()  %>%
#     label_expr2text  %>%
#     plotly_modify_legend %>%
#     plotly_modify_hover  %>%
#     plotly_annotation_rm















# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Drop columns with only one unique value
# findNonSingles <- function(x)(length(unique(x))  > 1)
# findSingles    <- function(x)(length(unique(x)) == 1)
#
# uniqueInfo <- unique(Filter(findSingles, data))
# message("Variables with constanant values are eliminated:")
# row.names(uniqueInfo) <- c("Value_of_eliminated_variable")
# message(pander::pander(t(uniqueInfo)))
#
# data      <- Filter(findNonSingles, data)
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# gg <- ggplot(msleep, aes(bodywt, brainwt)) +
#     geom_point(na.rm = TRUE) +
#     scale_x_log10() +
#     scale_y_log10() +
#     theme_bw() +
#      annotation_logticks()
# gg
#
# ggplotly(gg)
#
#
# plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16),
#         name = "$\\alpha_{1c} = 352 \\pm 11 \\text{ km s}^{-1}$") %>%
#     add_trace(x = c(1, 2, 3, 4), y = c(0.5, 2, 4.5, 8),
#               name = "$\\beta_{1c} = 25 \\pm 11 \\text{ km s}^{-1}$") %>%
#     layout(xaxis = list(title = "$\\sqrt{(n_\\text{c}(t|{T_\\text{early}}))}$"),
#            yaxis = list(title = "$d, r \\text{ (solar radius)}$"))

#  ------------------------------------------------------------------------

# gg <- ls(asNamespace("ggplot2"))
# geoms <- gg[grepl("^Geom", gg)]
# # plotly geoms
# to_basic <- plotly:::to_basic
# x <- sub("to_basic\\.", "", as.character(methods(to_basic)))
# geom2trace <- plotly:::geom2trace
# y <- sub("geom2trace\\.", "", as.character(methods(geom2trace)))
# # histograms are implemented, but don't have toBasic/geom2trace converters
# pgeoms <- c(x, y, "histogram")
# setdiff(geoms, pgeoms)
#
#
#
# #  ------------------------------------------------------------------------
#
# ?groupGeneric




# Identify point in `ggplot2` ---------------------------------------------

# library(ggplot2)
# library(plotly)
#
# x <- 1:10
# y <- x^3
# names <- paste("Point", LETTERS[x])
# qplot(x, y, label = names)
#
#
# # Version 1
# downViewport('panel.3-4-3-4')
# pushViewport(dataViewport(x,y))
#
# tmp <- grid.locator('in')
# tmp.n <- as.numeric(tmp)
# tmp2.x <- as.numeric(convertX( unit(x,'native'), 'in' ))
# tmp2.y <- as.numeric(convertY( unit(y,'native'), 'in' ))
#
# w <- which.min( (tmp2.x-tmp.n[1])^2 + (tmp2.y-tmp.n[2])^2 )
# grid.text(w, tmp$x, tmp$y )
#
#
# # Version 2
# last_plot()
# plotly::ggplotly()
