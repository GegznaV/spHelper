# This file shows syntax how to read .png files in a working directory and
#  to merge them to a .gif animation.
#
#  "Image Magick" has to be installed (http://www.imagemagick.org/script/index.php)

require(animation)

shell(paste0('convert -loop 0 -delay 30 ',  # Parameters
             '"*.jpg" ',                    # filenames to read
             '"','Vieno meg taskai.gif"'))  # filename to save to





# =============================================================================
## To make a movie =============================================================
##
require(spHelper)

# Create images
NewID <- unclass(as.factor(training$ID))
plabels <- paste0(NewID,"[", training$taskas,"]")

proximity_training <- qplot_proximity(training1, training$gr, subTitle = "(training)", plot.scatter = FALSE) +
    geom_text(aes(label = plabels),
              size = 3,
              show.legend = TRUE,
              # position = position_jitter(.1),
              check_overlap = F,
              parse = TRUE)


# Get (and set) limits to make images stable:
# limY <- ggplot_build(proximity_training)$panel$ranges[[1]]$y.range
limY <- layer_scales(proximity_training)$y$range$range
limX <- layer_scales(proximity_training)$x$range$range

png(filename = "000.png", res = 200, width = 1000, height = 1000)
print(proximity_training)
dev.off()


for (indID in unique(NewID)) {

    plabels0 <- plabels
    plabels0[NewID != indID] <- NA
    p <- proximity_training +
        geom_text(aes(label = plabels0),
                  size = 3,
                  show.legend = TRUE,
                  color = "black",
                  position = position_jitter(.1),
                  check_overlap = F,
                  parse = TRUE) +
        xlim(limX) + ylim(limY)

    # pdf(file = sprintf("%03g.pdf",indID),width = 480, height = 480,onefile=TRUE)
    png(filename =  sprintf("%03g.png",indID), res = 200, width = 1000, height = 1000)
    print(p)
    dev.off()
}

# Convert into an animation
require(animation)

shell(paste0('convert -loop 0 -delay 30 ',  # Parameters
             '"*.png" ',                    # filenames to read
             '"','Vieno meg taskai.gif"'))  # filename to save to
