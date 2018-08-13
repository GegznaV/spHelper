# [tags] Rename, Correct contents
#
#
correct_contents <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x  <- x0
    # Correct
    # x <- gsub("(family functions for \pkg{hyperSpec})|(family functions for \pkg{hyperSpec})",
    #           "family functions for \\\\pkg{hyperSpec}", x, perl = TRUE)

    # x <- gsub("family \\\\pkg\\{spHelper\\} functions for \\\\code\\{hyperSpec\\}",
    #           "family \\\\pkg{spHelper} functions for \\\\pkg{hyperSpec}",
    #           x, perl = TRUE)
    #
    # x <- gsub("family functions for \\\\pkg\\{hyperSpec\\}",
    #           "family \\\\pkg{spHelper} functions for \\\\pkg{hyperSpec}",
    #           x, perl = TRUE)
#
#     x <- gsub("family \\\\pkg\\{spHelper\\} functions for \\\\pkg\\{hyperSpec\\}",
#               "family \\\\pkg{spHelper} functions for spectroscopy and \\\\pkg{hyperSpec}",
#               x, perl = TRUE)



        # x <- gsub("family component analysis / factorisation related functions",
        #           "family component analysis / factorisation related functions in \\\\pkg{spHelper}",
        #           x, perl = TRUE)
        #
        # x <- gsub("family matrix operations",
        #           "family matrix operations in \\\\pkg{spHelper}",
        #           x, perl = TRUE)
        #
        # x <- gsub("family simmulation functions",
        #           "family simmulation functions in \\\\pkg{spHelper}",
        #           x, perl = TRUE)
        #
        # x <- gsub("family curves",
        #           "family curves in\\\\pkg{spHelper}",
        #           x, perl = TRUE)

    # x <- gsub("(family functions for \pkg{hyperSpec})|(family functions for \pkg{hyperSpec})",
    #           "family functions for \\\\pkg{hyperSpec}", x, perl = TRUE)
    x <- gsub("family family",
              "family", x, perl = TRUE)
    # Writte

    if (any(x0 != x))    {
        cat(FILE,sep = "\n")
        writeLines(x, con = FILE)
    }

    # print(x)
}


# Function 1 --------------------------------------------------------------

correct_contents2 <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x <- x0

    # Correct
    # x <- gsub("sp_at_zScore",  "mean_Nsd", x, fixed = TRUE)
    x <- gsub("which.outsideZ",  "outside_mean_pm_Nsd", x, fixed = TRUE)



        # Writte
    if (any(x0 != x))    { cat(FILE,sep = "\n"); writeLines(x, con = FILE) }
}

# function 1.B ------------------------------------------------------------

rename2 <- function(FILE){
    x <- FILE
    # Correct filenames
    # x <- gsub("sp_at_zScore",  "mean_Nsd", x, fixed = TRUE)

    NEW_FILE <- x
    # Rename files if needed
    if (FILE != NEW_FILE)    {
        cat(sprintf("Files renamed: %30s   >>>   %-30s", FILE, NEW_FILE),sep = "\n")
        file.rename(from = FILE, to = NEW_FILE)
    }
}



# Function 2 --------------------------------------------------------------

apply_content_corrections <- function(x){
    Start <-  Sys.time()
    setwd(x)

    AllFiles <- dir()
    FILES <- as.list(AllFiles[grepl("(.*\\.R$)|(.*\\.Rmd$)|(.*\\.html$)",AllFiles)])

    # lapply(FILES, correct_contents)
    lapply(FILES, correct_contents2)
    # lapply(FILES, rename2)

    shell.exec(getwd())
    printDuration(Start,returnString = TRUE)
}

# Function 3 --------------------------------------------------------------

require(spHelper)

# List all directories of interest
directories  <- as.list(
    c(paste0('D:\\Dokumentai\\R\\spHelper\\',
             c("R","vignettes","inst\\doc"),"\\"),

      "D:\\Dokumentai\\R\\spHelper\\",

      paste0("D:\\Dokumentai\\R\\Spektroskopija\\",
             c(".",
               "PAP_PD_2014\\",
               "PAP_RK_2014\\",
               "TD_2009\\",
               "TD_2015\\"))
      )
)

# Apply corrections
stop("This script can be harmful!!!")
lapply(directories, apply_content_corrections)


# setwd('D:\\Dokumentai\\R\\spHelper\\')







# Additional information --------------------------------------------------


## Correct file contents
# x <- gsub("plot_stat",      "qplot_stat", x, perl = TRUE)
# x <- gsub("plot_confusion", "qplot_confusion", x, perl = TRUE)
# x <- gsub("plot_infoDim",   "qplot_infoDim", x, perl = TRUE)
# x <- gsub("plot_kAmp",      "qplot_kAmp", x, perl = TRUE)
# x <- gsub("plot_kSpFacets", "qplot_kSpFacets", x, perl = TRUE)
# x <- gsub("plot_kSp",       "qplot_kSp", x, perl = TRUE)
# x <- gsub("plot_scores",    "qplot_scores", x, perl = TRUE)
# x <- gsub("plot_sp",        "qplot_sp", x, perl = TRUE)

# x <- gsub("qqplot_",        "qplot_", x, perl = TRUE)
#
# x <- gsub("qplot_spDiff",   "plot_spDiff", x, perl = TRUE)
# x <- gsub("foldsTets",      "foldTests", x, perl = TRUE)
# x <- gsub("listFunctions",  "list.functions", x, perl = TRUE)
# x <- gsub("makeFirstCapital",  "make.firstCapitals", x, perl = TRUE)
# x <- gsub("qplot_stat",  "qplolt_spStat", x, perl = TRUE)
# x <- gsub("addLabels_",  "hyAdd.Labels_", x, perl = TRUE)
# x <- gsub("sortMaxOnDiag",  "sortDescOnDiag", x, perl = TRUE)
# x <- gsub("plot_spFilt",  "plot_spCompare", x, perl = TRUE)

# =================================================================

# 2016-03-25
# x <- gsub("clear.except.class",  "clear_except_class", x, fixed = TRUE)
# x <- gsub("clear.except",        "clear_except", x, fixed = TRUE)
# x <- gsub("clear.fun",           "clear_fun", x, fixed = TRUE)
# x <- gsub("list.functions",      "list_functions", x, fixed = TRUE)
# x <- gsub("listAddRm",           "list_AddRm", x, fixed = TRUE)
# x <- gsub("make.firstCapitals",  "make_firstCapitals", x, fixed = TRUE)
# x <- gsub("new.matrix",          "newMatrix", x, fixed = TRUE)
# x <- gsub("plot_hy.palette",     "plot_hyPalette", x, fixed = TRUE)
# x <- gsub("ind.matrix",          "indMatrix", x, fixed = TRUE)
# #
# #
# ### Corrections were made in these files: ###
# #
# # 1_spHelper_description.R
# # hyAdd.Labels_PAP_PD_2014.R
# # hyAdd.Labels_PAP_RK_2014.R
# # hyAdd.Labels_TD2009.R
# # hyAdd.Labels_TD2015.R
# # indMatrix.R
# # plot_colors.R
# # plot_hy.palette.R
# # sortDescOnDiag.R
# # which.in.R
# # which.max.perRow.R
# # ChemoSpec.R
# # Import_spectra.R
# # PAP_2014-1B-Spectra preprocess.Rmd
# # PAP_2014-2B-NMF-Outliers-detection.Rmd
# # PAP_2014-2D-PCA-ALS-Outliers-detection.Rmd
# # readSp PAP_PD_2014.R
# # PAP_2014-1B-Spectra preprocess.Rmd
# # PAP_2014-2B-NMF-Outliers-detection.Rmd
# # readSp PAP_RK_2014.R
# # readSp TD_2009.R
# # TD_2009_2-1B-Spectra preprocess.Rmd
# # TD_2009_2-2B-NMF-Outliers-detection.Rmd
# # read TD_2015.R

# x <- gsub("hyAdd.",     "hyAdd_", x, fixed = TRUE)
# x <- gsub("hyGet.",     "hyGet_", x, fixed = TRUE)
# x <- gsub("hyDrop.NA",  "hyDrop_NA", x, fixed = TRUE)
# x <- gsub("plot_hy.palette",  "plot_hyPalette", x, fixed = TRUE)

# ### Corrections were made in these files: ###
# # 1_spHelper_description.R
# # hyAdd.color.R
# # hyAdd.Label.wl.R
# # hyAdd.Labels_PAP_PD_2014.R
# # hyAdd.Labels_PAP_RK_2014.R
# # hyAdd.Labels_TD2009.R
# # hyAdd.Labels_TD2015.R
# # hyDrop.NA.R
# # hyGet.palette.R
# # plot_hy.palette.R
# # qplot_kAmp.R
# # qplot_prediction.R
# # qplot_proximity.R
# # qplot_spStat.R
# # spStat.R
# # v1_spHelper_Plotting.Rmd
# # v1_spHelper_Plotting.html
# # v1_spHelper_Plotting.R
# # v1_spHelper_Plotting.Rmd
# # Import_spectra.R
# # Idejos.R
# # PAP_2014-1B-Spectra preprocess.Rmd
# # PAP_2014-2B-NMF-Outliers-detection.Rmd
# # PAP_2014-2D-PCA-ALS-Outliers-detection.Rmd
# # PAP_2014-2E-PCA-ALSComp2-wo-outliers.Rmd
# # readSp PAP_PD_2014.R
# # Idejos.R
# # PAP_2014-1B-Spectra preprocess.Rmd
# # PAP_2014-1B1-Spectra preprocess.Rmd
# # PAP_2014-1B2-Spectra preprocess.Rmd
# # PAP_2014-2B-NMF-Outliers-detection.Rmd
# # PAP_2014-3A-RFE.Rmd
# # readSp PAP_RK_2014.R
# # readSp TD_2009.R
# # TD_2009_2-1B-Spectra preprocess.Rmd
# # TD_2009_2-2B-NMF-Outliers-detection.Rmd
# # TD_2009_2-3A-RFE.Rmd
# # read TD_2015.R
# #

# x <- gsub("hyAdd_Label.wl",  "hyAdd_Label_wl", x, fixed = TRUE)
# ### Corrections were made in these files: ###
# hyAdd.Label.wl.R
# hyAdd.Labels_PAP_PD_2014.R
# hyAdd.Labels_PAP_RK_2014.R
# hyAdd.Labels_TD2009.R
# hyAdd.Labels_TD2015.R

# x <- gsub("kNames",  "cNames", x, fixed = TRUE)
# # ### Corrections were made in these files: ###
# 2_datasets.R
# getScores.R
# qplot_kAmp.R
# qplot_kSp.R
# simSpectra.R
# sortLoadings.R
# Scripts to rev.Rmd
# PAP_PD_2014 - 1.Rmd
# TD_2009_2-2C-NMF (2).Rmd



# Corrected filenames =====================================================
# x <- gsub("hyAdd.Label.wl",  "hyAdd_Label_wl",  x, fixed = TRUE)
# x <- gsub("hyAdd.",     "hyAdd_", x, fixed = TRUE)
# x <- gsub("hyAdd_R",     "hyAdd.R", x, fixed = TRUE)
#
# x <- gsub("hyGet.",     "hyGet_", x, fixed = TRUE)
# x <- gsub("hyDrop.NA",  "hyDrop_NA", x, fixed = TRUE)
# x <- gsub("plot_hy.palette",  "plot_hyPalette", x, fixed = TRUE)
