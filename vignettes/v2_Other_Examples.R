## ----options, echo = FALSE, message = FALSE, warning = FALSE-------------
optDEF <- knitr::opts_chunk$get()
knitr::opts_chunk$set(collapse = FALSE, comment = "#>")

knitr::opts_chunk$set(fig.width = 6, fig.align = 'center')


## ----Load packages, include = FALSE, message = FALSE, warning = FALSE----
library(spHelper)

library(dplyr)

## ------------------------------------------------------------------------
text1     <- c("_A","_Bee","_CE","_D")
pattern1A <- '_(?<letter>.)'  # captures only the first symbol
regexp2df(text1, pattern1A)

pattern1B <- '_(?<word>.*)'   # captures all symbols
regexp2df(text1, pattern1B)

text2 <- c("A_111  B_aaa",
           "A_222  B_bbb",
           "A_333  B_ccc",
           "A_444  B_ddd",
           "A_555  B_eee")

pattern2 <- 'A_(?<Part_A>.*)  B_(?<Part_B>.*)'

regexp2df(text2, pattern2)

## ------------------------------------------------------------------------
   #> Error ...

## ------------------------------------------------------------------------
text3 <- c("sn555 ID_O20-5-684_N52_2_Subt2_01.",
           "sn555 ID_O20-5-984_S52_8_Subt10_11.")

pattern3 <- paste0('sn(?<serial_number>.*) ',
                   'ID_(?<ID>.*)',
                   '_(?<Class>[NS])',
                   '(?<Sector>.*)',
                   '_(?<Point>.*)',
                   '_[Ss]ubt.*\\.');
cat(pattern3)
regexp2df(text3, pattern3)

## ------------------------------------------------------------------------
 regexp2df(dir(),'(?<R_file>.*\\.[rR]$)')

## ------------------------------------------------------------------------
 library(dplyr)

 dir() %>% regexp2df('(?<R_file>.*\\.[rR]$)')

## ------------------------------------------------------------------------
 expr <- paste0('(?<R_file>.*\\.[rR]$)|',
                '(?<Rmd_file>.*\\.[rR]md$)|',
                '(?<HTML_file>.*\\.html$)')
 dir() %>% regexp2df(expr) 

## ------------------------------------------------------------------------
clear()

# Load data
data("DataSet1")

# Set parameters
nFolds = 5

# Explore data
str(DataSet1)

# table(DataSet1[,c("gr","ID")])
# summary(DataSet1)

