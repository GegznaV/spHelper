# save_rds_unique -------------------------------------------------------------

#' [~!~] Save object as `.RDS` with unique name (i.e., without overwritting)
#'
#' /No description yet/
#'
#' @param what Object to save.
#' @param path File name with path to save to.
#'
#' @export
#'
#' @author Ignas Ciplys
#' @family functions by Ignas Ciplys
#' @examples
#'
#' #NO EXAMPLES YET
#'
save_rds_unique <- function(what,path) {
    # [FAILŲ PAVADINIMUOSE NENAUDOTI TARPELIŲ IR TAŠKŲ]
    # what - ką saugosime?
    # path - direktorija su norodytu pavadinimu.
    #
    # programa nuskaito tik pavadinimą:
    split  <- strsplit(path,"[/]")

    # Išskiriamas pavadinimas be RDS:
    split1 <- strsplit(split[[1]][2],"[.]")[[1]][1]

    # Pridedama data ir failo numeris
    Pav_main <- paste0(split1," ",Sys.Date()," " ,0," ",".RDS")

    # Surandami aplankale esantys failai
    Sar <- list.files(paste0(split[[1]][1],"/"))

    # Išsaugomas pradinis pavadinimas, priskiriama nauja vertė.
    Pav <- Pav_main
    i <- 0
    while (TRUE) {             # Amžinas ciklas su sąlyginiu lūžiu.
        if (any(Sar == Pav))   # Jei yra nors vienas su tokiu pavadinimu,
            # pridedamas vienetukas, ciklas kartojamas
        {
            i = i + 1
            Pav <- strsplit(Pav <- Pav_main," ")
            Pav <- paste0(Pav[[1]][1]," ",Pav[[1]][2]," ",i," ",Pav[[1]][4])
        } else {# Jei pavadinimai nesutampa, ciklas nutraukiamas,
            # gaunamas naujas pavadinimas.
            Pav <- strsplit(Pav," ")
            Pav <- paste0(Pav[[1]][1]," ",Pav[[1]][2]," ",i," ",Pav[[1]][4])
            break
        }
    }
    saveRDS(what,paste0(split[[1]][1],"/",Pav))
}


#'
#' # plot_multiDim -----------------------------------------------------------
#'
#' #' [~!~] Groups dicrimination - test vs. prediction.
#' #'
#' #' /No description yet/
#' #'
#' #' @param amp ...
#' #' @param gr_test  ...
#' #' @param gr_predict ...
#' #'
#' #' @param colors ...
#' #' @param k ...
#' #' @param color_list List of colors to be used. If used, \code{colors} is ignored.
#' #'
#' #' @return ...
#' #' @export
#' #'
#' #' @author Ignas Ciplys
#' #' @family functions by Ignas Ciplys
#' #' @examples
#' #'  #NO EXAMPLES YET
#' #'
#' plot_multiDim <- function(amp, gr_test, gr_predict,
#'                           colors, k = 2,
#'                           color_list =  colors[factor(gr_predict)]) {
#'
#'     # amp <- AMP_obj$spc
#'     # gr_test <- test$gr
#'     # gr_predict <- predicted$pred
#'     #   colors <-unique(AMP_obj$.color)
#'     # colors
#'     # # Kuriama random prediction
#'     # gr_predict <- sample(levels(AMP_obj$gr),nrow(AMP_obj), replace = T)
#'     # gr_test <- AMP_obj$gr
#'     amp <- scale(amp)
#'     blogi_idx <- gr_test !=  gr_predict
#'
#'     # color_list <-  unique(AMP_obj$.color)[factor(gr_sample)]
#'     # color_list <-  colors[factor(gr_predict)] # moved to function's input.
#'     color_original <- color_list
#'     color_list[blogi_idx] <- 'red'
#'
#'     d <- dist(amp) # euclidean distances between the rows
#'     fit <- cmdscale(d,eig = TRUE, k) # k is the number of dim
#'
#'     # plot solution
#'     x <- fit$points[,1]
#'     y <- fit$points[,2]
#'     plot(x, y,
#'          xlab = "Projection 1",
#'          ylab = "Projection 2",
#'          main = subt("Proximity of Groups", "Metric multidimensional scaling"),
#'          type = "p",
#'          col = color_original,
#'          pch = 20)
#'     points(x,y,
#'            col = color_list,
#'            pch = ifelse(blogi_idx,4,20))
#'     legend("topright",
#'            legend = "Misclassified",
#'            pch = 4,
#'            col = "red",
#'            bty = "n")
#'
#'     # fit2 <- MASS::isoMDS(d, k = 2) # k is the number of dim
#'     #
#'     # # plot solution
#'     # x2 <- fit2$points[,1]
#'     # y2 <- fit2$points[,2]
#'     # plot(x2, y2, xlab = "Coordinate 1", ylab = "Coordinate 2",
#'     #      main = "NONMetric	MDS",	type = "p",
#'     #       col = color_list,
#'     #       pch = ifelse(blogi_idx,4,20))
#'
#' }





