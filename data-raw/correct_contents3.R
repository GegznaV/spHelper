# [tags] Rename, Correct contents

# Function 1.A --------------------------------------------------------------

correct_contents2 <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x <- x0

    # Correct
    # # 2016-03-25
    # x <- gsub("plot_spFilt",         "plot_spCompare",     x, fixed = TRUE)
    # x <- gsub("clear.except.class",  "clear_except_class", x, fixed = TRUE)
    # x <- gsub("clear.except",        "clear_except",       x, fixed = TRUE)
    # x <- gsub("clear.fun",           "clear_fun",          x, fixed = TRUE)
    # x <- gsub("list.functions",      "list_functions",     x, fixed = TRUE)
    # x <- gsub("listAddRm",           "list_AddRm",         x, fixed = TRUE)
    # x <- gsub("make.firstCapitals",  "make_firstCapitals", x, fixed = TRUE)
    # x <- gsub("new.matrix",          "newMatrix",          x, fixed = TRUE)
    # x <- gsub("plot_hy.palette",     "plot_hyPalette",     x, fixed = TRUE)
    # x <- gsub("ind.matrix",          "indMatrix",          x, fixed = TRUE)
    # x <- gsub("hyAdd.",              "hyAdd_",             x, fixed = TRUE)
    # x <- gsub("hyGet.",              "hyGet_",             x, fixed = TRUE)
    # x <- gsub("hyDrop.NA",           "hyDrop_NA",          x, fixed = TRUE)
    # x <- gsub("hyAdd_Label.wl",      "hyAdd_Label_wl",     x, fixed = TRUE)
    # x <- gsub("kNames",              "cNames",             x, fixed = TRUE)
    # x <- gsub("sp_at_zScore",        "mean_Nsd",           x, fixed = TRUE)
    # x <- gsub("which.outsideZ",      "outside_mean_pm_Nsd",x, fixed = TRUE)

    # 2016-04-28
    # x <- gsub("qplot_sp_summary_gr",   "qplot_spIntervals"  ,x, fixed = TRUE)
    # x <- gsub("plot_sp_summary",      "plot_spDistribution",x, fixed = TRUE)
    # x <- gsub("qqplot_spIntervals",      "qplot_spIntervals",x, fixed = TRUE)

    # x <- gsub("qplot_spIntervals",      "qplot_spDistrib_gr",x, fixed = TRUE)

    # x <- gsub("qplot_spDistrib_gr",      "qplot_spDistrib",x, fixed = TRUE)
    # x <- gsub("qplolt_spStat",      "qplot_spStat",x, fixed = TRUE)
    # x <- gsub("plotly_annotation_rms",      "plotly_annotation_rm",x, fixed = TRUE)

    # x <- gsub("add_as_plotly",      "add_as_plotly_widget",x, fixed = TRUE)

    # x <- gsub("knitr_container",      "knitrContainer",x, fixed = TRUE)

    # x <- gsub("%if_null_or_len0%",      "%if_len_0%",x, fixed = TRUE)
    # x <- gsub("add_obj",      "attach_obj",x, fixed = TRUE)
    x <- gsub("extract_and_print",      "print_container",x, fixed = TRUE)


    # Writte
    if (any(x0 != x))    { cat(FILE,sep = "\n"); writeLines(x, con = FILE) }
}

# function 1.B ------------------------------------------------------------

rename2 <- function(FILE){
    x <- FILE

    # Correct filenames
    # x <- gsub("^PAP_2014-",  "", x, perl = TRUE)
    # x <- gsub("knitr_container",      "knitrContainer",x, fixed = TRUE)
    # x <- gsub("add_obj",      "attach_obj",x, fixed = TRUE)
    x <- gsub("extract_and_print",      "print_container",x, fixed = TRUE)



    NEW_FILE <- x
    # Rename files if needed
    if (FILE != NEW_FILE)    {
        cat(sprintf("Files renamed: %30s   >>>   %-30s", FILE, NEW_FILE), sep = "\n")
        file.rename(from = FILE, to = NEW_FILE)
    }
}

# Function 2 --------------------------------------------------------------

apply_content_corrections <- function(x){
    Start <-  Sys.time()
    current_wd <- getwd()
    # Reset wd on exit
    on.exit(setwd(current_wd))

    # Change wd
    setwd(x)
    AllFiles <- dir()
    FILES <- as.list(AllFiles[grepl("(.*\\.R$)|(.*\\.Rmd$)|(.*\\.html$)",AllFiles)])

    # lapply(FILES, correct_contents)
    lapply(FILES, correct_contents2)
    lapply(FILES, rename2)

    # open_wd()
    printDuration(Start,returnString = TRUE)


}

# Function 3 --------------------------------------------------------------

require(spHelper)

# List all directories of interest

directories  <- as.list(
    c(
        "D:\\Dokumentai\\R\\knitrContainer\\",

        paste0('D:\\Dokumentai\\R\\knitrContainer\\',
             c("R","vignettes","inst\\doc", "tests\\testthat"),"\\")

      )
)

# directories  <- as.list("D:\\Dokumentai\\R\\Spektroskopija\\PAP_RK_2014\\")


# Apply corrections
stop("This script can be harmful!!!")
lapply(directories, apply_content_corrections)

