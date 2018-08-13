library(ggrepel)
library(ggspectra)
library(spHelper)
library(spPlot)

ggplot(Spectra2[3],alpha = .5) +

    geom_line(size = 2, colour = "white") +
    geom_line(size = 1, colour = "black") +
    geom_hline(yintercept = 0, colour = "grey92") +

    stat_peaks(shape = 21, span = 25, size = 2) +
    scale_fill_identity() +
    stat_peaks(geom =  "label",
               span = 25,
               vjust = -.1,
               size = 3,
               color = "white")+
    theme_bw()
