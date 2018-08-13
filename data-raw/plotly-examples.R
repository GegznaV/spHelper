
# latex2exp ---------------------------------------------------------------


library(latex2exp)

latex2exp_examples()

latex2exp_supported(plot = FALSE)
latex2exp_supported(plot = TRUE)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
TeX("$\\alpha$")
TeX("The ratio of 1 and 2 is $\\frac{1}{2}$")

a <- 1:100
plot(a, a ^ 2, xlab = TeX("$\\alpha$"), ylab = TeX("$\\alpha^2$"),
     main = TeX("The ratio of 1 and 2 is $\\frac{1}{2}$"))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
x <- seq(0, 4, length.out = 100)
alpha <- 1:5

plot(x, xlim = c(0, 4), ylim = c(0, 10),
     xlab = 'x', ylab = TeX('$\\alpha  x^\\alpha$, where $\\alpha \\in 1\\ldots 5$'),
     type = 'n', main = TeX('Using $\\LaTeX$ for plotting in base graphics!'))

invisible(sapply(alpha, function(a) lines(x, a*x^a, col = a)))

legend('topleft', legend = TeX(sprintf("$\\alpha = %d$", alpha)),
       lwd = 1, col = alpha)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(plyr)
library(ggplot2)
x <- seq(0, 4, length.out = 100)
alpha <- 1:5
data <- mdply(alpha, function(a, x) data.frame(v = a*x^a, x = x), x)

p <- ggplot(data, aes(x = x, y = v, color = X1)) +
    geom_line() +
    ylab(TeX('$\\alpha  x^\\alpha$, where $\\alpha \\in 1\\ldots 5$')) +
    ggtitle(TeX('Using $\\LaTeX$ for plotting in ggplot2. I $\\heartsuit$ ggplot!')) +
    coord_cartesian(ylim = c(-1, 10)) +
    guides(color = guide_legend(title = NULL)) +
    scale_color_discrete(labels = lapply(sprintf('$\\alpha = %d$', alpha), TeX))
# Note that ggplot2 legend labels must be lists of expressions, not vectors of expressions
print(p)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
plot(TeX("A $\\LaTeX$ formula: $\\frac{2hc^2}{\\lambda^5} \\,
               \\frac{1}{e^{\\frac{hc}{\\lambda k_B T}} - 1}$"), cex = 2)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  =

# plotly ------------------------------------------------------------------

library(plotly)
p <- plot_ly(economics, x = date, y = unemploy / pop)
p
#  ------------------------------------------------------------------------
p <- plot_ly(economics, x = date, y = unemploy / pop,
             type = "scatter", mode = "markers+lines")
p
#  ------------------------------------------------------------------------
m <- loess(unemploy / pop ~ as.numeric(date), data = economics)
p <- plot_ly(economics, x = date, y = unemploy / pop, name = "raw")
p <- add_trace(p, x = date, y = fitted(m), name = "loess")
p
#  ------------------------------------------------------------------------
p <- economics %>%
    plot_ly(x = date, y = unemploy / pop) %>%
    add_trace(x = date, y = fitted(m)) %>%
    layout(showlegend = F)
p
#  ------------------------------------------------------------------------
p <- economics %>%
    transform(rate = unemploy / pop) %>%
    plot_ly(x = date, y = rate) %>%
    subset(rate ==  max(rate)) %>%
    layout(
        showlegend = F,
        annotations = list(x = date, y = rate, text = "Peak", showarrow = T)
    )
p
#  ------------------------------------------------------------------------
p <- plot_ly(z = volcano, type = "surface")
p

# R figure reference: plotly ------------------------------------------
library(plotly)

p <- plot_ly(economics,
             type = "scatter",       # all "scatter" attributes: https://plot.ly/r/reference/#scatter
             x = date,               # more about scatter's "x": /r/reference/#scatter-x
             y = uempmed,            # more about scatter's "y": /r/reference/#scatter-y
             name = "unemployment",  # more about scatter's "name": /r/reference/#scatter-name
             marker = list(          # marker is a named list, valid keys: /r/reference/#scatter-marker
                 color = "royalblue"     # more about marker's "color" attribute: /r/reference/#scatter-marker-color
             ))

p <- add_trace(p,                       # by default, traces are type "scatter"
               x = date,
               y = fitted((loess(uempmed ~ as.numeric(date)))),  # scatter's "y": /r/reference/#scatter-y
               line = list(                        # line is a named list, valid keys: /r/reference/#scatter-line
                   color = rgb(.8, 0, 0),      # line's "color": /r/reference/#scatter-line-color
                   dash = 5,                 # line's "dash" property: /r/reference/#scatter-line-dash
                   width = 1
               ),
               name = quote(alpha)
)

p
maxdf <- dplyr::filter(economics, uempmed ==  max(economics$uempmed))

p <- layout(p,              # all of layout's properties: /r/reference/#layout
            title = "unemployment", # layout's title: /r/reference/#layout-title
            xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                title = "time",     # xaxis's title: /r/reference/#layout-xaxis-title
                showgrid = F        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
            ),
            yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                title = "uidx"      # yaxis's title: /r/reference/#layout-yaxis-title
            ),
            annotations = list(         # all annotation properties: /r/reference/#layout-annotations
                list(
                    x = maxdf$date,     # annotation's x: /r/reference/#layout-annotations-x
                    y = maxdf$uempmed,  # annotation's y: /r/reference/#layout-annotations-y
                    text = "Peak",      # annotation's text: /r/reference/#layout-annotations-text
                    showarrow = T       # annotation's showarrow: /r/reference/#layout-annotations-showarrow
                )
            )
)
p

#  ------------------------------------------------------------------------
library(plotly)
plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16)) %>%
    add_trace(x = c(1, 2, 3, 4), y = c(0.5, 2, 4.5, 8)) %>%
    layout(xaxis = list(title = expression(lambda)),
           yaxis = list(title = "d, r \\text{ (solar radius)}"))



#  ------------------------------------------------------------------------

xdat <- c("Bob Dylan", "The Beatles", "David Bowie", "Randy Newman", "The Rolling Stones", "Madonna", "Frank Sinatra", "The Beach Boys", "Marvin Gaye", "Prince", "The Kinks", "Elvis Presley", "Tom Waits", "U2", "The Clash", "Johnny Cash", "Kate Bush", "The Supremes", "The Smiths", "Al Green", "Pulp", "Chuck Berry", "Elvis Costello and the Attractions", "Neil Young", "Stevie Wonder", "Ray Charles", "The Pogues", "Grace Jones", "Bill Withers", "The Who", "Paul Simon", "Roy Orbison", "Arctic Monkeys", "Bruce Springsteen", "The Police", "Rod Stewart", "Steve Earle")

ydat <- c(24, 19, 9, 8, 8, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)

p <- plot_ly(x = xdat,  y  = ydat,  name = "Number",
             marker = list(color = "#2ca02c"),
             type = "bar")
p


#  ------------------------------------------------------------------------

# volcano is a numeric matrix that ships with R
plot_ly(z = volcano, type = "surface")


#  ------------------------------------------------------------------------

t <- list(
    family = "sans serif",
    size = 18,
    color = toRGB("grey50")
)
plot_ly(mtcars, x = wt, y = mpg, text = rownames(mtcars), mode = "markers+text",
        textfont = t, textposition = "top middle")

#  ------------------------------------------------------------------------

m <- mtcars[which.max(mtcars$mpg), ]

a <- list(
    x = m$wt,
    y = m$mpg,
    text = rownames(m),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
)

plot_ly(mtcars, x = wt, y = mpg, mode = "markers") %>%
    layout(annotations = a)


#  ------------------------------------------------------------------------

p <- mtcars %>%
    plot_ly(x = disp, y = mpg, mode = "markers", color = cyl, size = wt,
            hoverinfo = "text",
            text = paste("Displacement = ", mtcars$disp, "Miles Per Gallon = ", mtcars$mpg)) %>%
    layout(title  = "Custom Hover Text")
p




# =========================================================================
# Correct `plotly` manually -----------------------------------------------
require(spHelper)
clear()

data(flu, package = "hyperSpec")

qplotspc(flu)
ggplotly()

rmExpr()
ggplotly()

#--------------------------------
p1 <- plot_sp(Loadings)
p1$labels

p2 <- rmExpr(p1)
p2$labels

ggplotly(p1)
ggplotly(p2)

#----------------------------------

p <- qplotspc(aggregate(chondro, chondro$clusters, mean_pm_sd),
         mapping = aes(x = .wavelength,
                       y = spc,
                       colour = clusters)) +
    facet_grid(clusters ~ .) + ggtitle("Spectra of Chondrocytes")

p <- rmExpr(p)
p

ggplotly(p)

# Namually correct `plotly` object
q2  <- plotly_build(p)
q1 <- plotly_build(p2)
str(q2)
# Add legend
q2$layout$showlegend <- TRUE

# Remove stripe text
q2$layout$annotations[1:3] <- NULL

# Position of legends
q2$layout$annotations[[1]]$y <- -.05
q2$layout$annotations[[2]]$x <- -.1
q2

p0 = ggplot2::last_plot()
p0$labels$x
#'
