## ----settings, echo=FALSE, message=FALSE, warning=FALSE-----------------------
optDEF <- knitr::opts_chunk$get()
knitr::opts_chunk$set(collapse = FALSE, comment = "#>")
knitr::opts_chunk$set(fig.width = 6, fig.align = 'center')

library(spHelper)
library(dplyr)

## ----Load packages, include=TRUE, message=FALSE, warning=FALSE----------------
library(spHelper)
library(dplyr)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
   #> Error ...

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
 regexp2df(dir(),'(?<R_file>.*\\.[rR]$)')

## -----------------------------------------------------------------------------
 library(dplyr)

 dir() %>% regexp2df('(?<R_file>.*\\.[rR]$)')

## -----------------------------------------------------------------------------
 expr <- paste0('(?<R_file>.*\\.[rR]$)|',
                '(?<Rmd_file>.*\\.[rR]md$)|',
                '(?<HTML_file>.*\\.html$)')
 dir() %>% regexp2df(expr) 

## -----------------------------------------------------------------------------
m1 <- matrix(NA, 5, 5)
m1

which.in.diag(m1)

m2 <- matrix(NA, 2, 5)
which.in.diag(m2)

# ================================

which.in(diag,    m1)
which.in(offdiag, m1)

which.in(col, m1, col = 2)
which.in(row, m1, row = 2)

which.in(trilow, m1)
which.in(trilow, m1, diag = TRUE)

which.in(triupp, m1)
 
# ================================
 
r1 <- which.in(trilow, m1)
r2 <- which.in.trilow(m1)
identical(r1, r2)

## -----------------------------------------------------------------------------
# ------------------------------------------------------------
# Example 1A: Vector into a matrix

vector = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
corr_vec2mat(vector)

# ------------------------------------------------------------

 # Example 1B: Vector is transformed to a matrix by filling
 # it column-wise:

## -----------------------------------------------------------------------------
 # ---------------------------------------------------------------------
 # Example 2: inappropriate number of coefficients - warning appears

 # corr_vec2mat(vector[1:5])
    #> Warning message:
    #> Only first 3 coefficient(s) out of 5 will be used to construct symmetric matrix with 3 rows.

 # --------------------------------------------------------------------
 # Example 3: ERROR appears - values of coeffs must be between [-1;1]

 # corr_vec2mat(1:5)
    #> Error in corr_vec2mat(1:5) :
    #> All values in input vector must be between [-1 and 1]


## ---- eval = FALSE------------------------------------------------------------
#  # Data
#  df  <- mtcars[,c("cyl","gear")]
#  
#  #  Function, that uses `getVarValues`:
#  f1 <- function(data, v1) { getVarValues(v1, data) }
#  
#  # Returns values of `df$cyl`:
#  f1(df, cyl)
#  
#  f1(df, "cyl")
#  
#  cyl <- "gear"        # !!! Still values of `df$cyl`, not `df$gear`:
#  f1(df, cyl)
#  
#  # Returns values of `df$gear`:
#  f1(df, gear)
#  
#  # Returns values of vector `a`, as there is no variable `df$a`:
#  a = "cyl"
#  f1(df, a)
#  
#  var <- c("My", "variable", "var")
#  f1(df, var)

## ---- eval = FALSE------------------------------------------------------------
#  # A Data frame
#     df <- data.frame(A = "Values_A_(DATA.FRAME)",
#                      E = "Values_E_(DATA.FRAME)", stringsAsFactors = FALSE)
#  
#  # Vectors
#     A <- "Values of the vector 'A'"
#     B <- "Values of the vector 'B'"
#  
#  # A call object `CALL`:
#      fun  <- function(data, gr, ID) { match.call() }
#      CALL <- fun(df, A, B)
#      CALL
#  
#  # Outputs of `getVarValues` -------------------------------------------------
#  
#      getVarValues(VAR = gr, DATA = df, CALL = CALL)
#  
#      getVarValues(gr, df, CALL)
#  
#      getVarValues(A, df, CALL)
#  
#      getVarValues(B, df, CALL)
#  

## ---- eval = FALSE------------------------------------------------------------
#       getVarValues(ID, df, CALL) # `ID` found only in function's `fun` definition.
#                                  # `df$ID` does not exist.
#  
#       getVarValues(F, df, CALL) # `F` is a special variable: `F = FALSE`
#  
#       getVarValues(c, df, CALL) # `c()` is a function.

## -----------------------------------------------------------------------------
 #>  Error in eval(expr, envir, enclos) : object 'G' not found


