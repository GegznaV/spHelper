# [tags] Rename, Correct contents

# Function 1.A --------------------------------------------------------------

correct_contents2 <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x <- x0

    # Correct
    # 2016-03-25
    x <- gsub("plot_spFilt",         "plot_spCompare",     x, fixed = TRUE)
    x <- gsub("clear.except.class",  "clear_except_class", x, fixed = TRUE)
    x <- gsub("clear.except",        "clear_except",       x, fixed = TRUE)
    x <- gsub("clear.fun",           "clear_fun",          x, fixed = TRUE)
    x <- gsub("list.functions",      "list_functions",     x, fixed = TRUE)
    x <- gsub("listAddRm",           "list_AddRm",         x, fixed = TRUE)
    x <- gsub("make.firstCapitals",  "make_firstCapitals", x, fixed = TRUE)
    x <- gsub("new.matrix",          "newMatrix",          x, fixed = TRUE)
    x <- gsub("plot_hy.palette",     "plot_hyPalette",     x, fixed = TRUE)
    x <- gsub("ind.matrix",          "indMatrix",          x, fixed = TRUE)
    x <- gsub("hyAdd.",              "hyAdd_",             x, fixed = TRUE)
    x <- gsub("hyGet.",              "hyGet_",             x, fixed = TRUE)
    x <- gsub("hyDrop.NA",           "hyDrop_NA",          x, fixed = TRUE)
    x <- gsub("hyAdd_Label.wl",      "hyAdd_Label_wl",     x, fixed = TRUE)
    x <- gsub("kNames",              "cNames",             x, fixed = TRUE)
    x <- gsub("sp_at_zScore",        "mean_Nsd",           x, fixed = TRUE)
    x <- gsub("which.outsideZ",      "outside_mean_pm_Nsd",x, fixed = TRUE)

    # Writte
    if (any(x0 != x))    { cat(FILE,sep = "\n"); writeLines(x, con = FILE) }
}

# function 1.B ------------------------------------------------------------

rename1 <- function(FILE){
    x <- FILE
    # Correct filenames
    # x <- gsub("sp_at_zScore",  "mean_Nsd", x, fixed = TRUE)
    # x <- gsub("(/)([[:digit:]]{1,})(.*)(\\.)",  " \\2\\4", x, perl = T)
    # x <- spHelper::make.filenames(x)
    # x <- gsub(" Track ",  " - ", x, fixed = TRUE)
    x <- gsub("CD",  "CD ", x, fixed = TRUE)
    
    
    NEW_FILE <- x
    # Rename files if needed
    if (FILE != NEW_FILE)    {
        cat(sprintf("Files renamed: %30s   >>>   %-30s", FILE, NEW_FILE),sep = "\n")
        file.rename(from = FILE, to = NEW_FILE)
    }
}

# Function 2 --------------------------------------------------------------

apply_corrections <- function(x){
    Start <-  Sys.time()
    setwd(x)

    AllFiles <- dir(recursive = TRUE)
    FILES <- as.list(AllFiles[grepl("(.*\\.mp3$)|(.*\\.MP3$)", AllFiles)])

    # lapply(FILES, correct_contents)
    # lapply(FILES, correct_contents2)
    lapply(FILES, rename1)

    shell.exec(getwd())
    printDuration(Start,returnString = TRUE)
}

# Function 3 --------------------------------------------------------------

require(spHelper)
CurDir <- getwd()

# List all directories of interest
directories  <- as.list("C:\\Users\\ViG\\Desktop\\Audio\\[Edu] The 4-Hour Body An Uncommon Guide to Rapid Fat-Loss Incredible Sex and Becoming Superhuman\\")

# Apply corrections
# stop("This script can be harmful!!!")
lapply(directories, apply_corrections); setwd(CurDir)








