#' [!] Calculate ratios between values of every column pair.
#'
#' @param x A matrix-like data.
#' @param col.names One of the fpollowing: c("full", "short", "rm.letters").
#' \cr\cr [NOT UPDATED DESCRIPTION] If \code{FALSE}, whole names of compared columns
#'        will be used for column of ratios, e.g. \code{"Column 1/ Column 2"}. If
#'        \code{TRUE} (default), shorter version of column name will be used,
#'         where parts of name which are common for both columns will not be
#'         repeated, e.g. \code{"Column 1/2"}.
#'
#' @return A matrix with ratios between values of every column pair of the matrix \code{x}.
#' @export
#'
#' @examples
#' x <- Scores2[[]]
#' r1 <- colRatios(x)
#' r2 <- colRatios(x, col.names = "short")
#' r3 <- colRatios(x, col.names = "rm.letters")
#'
#' head(x)
#' head(r1)
#'
#' colnames(r1)
#' colnames(r2)
#' colnames(r3)
#'
colRatios <- function(x, col.names = c("full", "short", "rm.letters")) {
    x <- as.matrix(x)
    # Variable names
    cNames <- colnames(x)
    if (is.null(cNames)) cNames <- 1:ncol(x)

    # select unique combinations only
    vars <- expand.grid(cNames, cNames)[,c(2:1)] # [,c(2:1)] - to reverse order of columns
    n    <- sqrt(nrow(vars)) # number of rows in a square matrix
    ind  <- spMisc::which.in.trilow(matrix(NA,n,n))
    vars <- vars[ind,]

    # Create variable names

    fun <- switch(tolower(col.names[1]),
           full       = function(x) paste(x, collapse = " / "),
           short      = function(x) pasteMergePrefixAndSuffix(x[1], x[2], sep = ' / '),
           rm.letters = function(x) paste(gsub("[[:alpha:]]", "", x), collapse = " / ") )

    vars$Names <- apply(vars, 1, fun)

    # Variable preallocation
    ratios <- matrix(NA, nrow = nrow(x), ncol = nrow(vars))

    # Calculate ratios
    for (i in 1:nrow(vars))
        ratios[,i] <- x[,vars[i,1]] / x[,vars[i,2]]

    # Rename columns
    colnames(ratios) <- if (is.null(colnames(x))) {
        paste("Column",vars$Names)
    } else {
        vars$Names
    }

    # Return the resultS
    return(ratios)
}



#
#
#   #' Concatenate strings and discard non-unique parts
#   #'
#   #' @param vs1 String 1
#   #' @param vs2 String 2
#   #' @param sep separator
#   #'
#   #' @return Strings with concatenated unique parts.
#   #'
#   #' @examples
#   #' pasteMergePrefixAndSuffix('a','b');
#   #' ## [1] "a b"
#   #'
#   #' pasteMergePrefixAndSuffix('a','b','/');
#   #' ## [1] "a/b"
#   #'
#   #' s1 <- 'very big house';
#   #' s2 <- 'very small house';
#   #' pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "very big/small house"
#   #'
#   #' s1 <- '1b'; s2 <- '2b'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "1/2b"
#   #' s1 <- 'a_1_b'; s2 <- 'a_2_b'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "a_1/2_b"
#   #' s1 <- 'ab'; s2 <- 'ab'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "ab"
#   #' s1 <- 'xab'; s2 <- 'ab'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "x/ab"
#   #' s1 <- 'ab'; s2 <- 'abx'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "ab/x"
#   #' s1 <- 'abx'; s2 <- 'ab'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "abx/"
#   #' s1 <- 'ab'; s2 <- 'xab'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "/xab"
#   #' s1 <- ''; s2 <- 'x'; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "/x"
#   #' s1 <- 'x'; s2 <- ''; pasteMergePrefixAndSuffix(s1,s2,'/');
#   #' ## [1] "x/"
#   #'
#   #' @source \url{http://stackoverflow.com/a/36220282/4783029}
#   #'
pasteMergePrefixAndSuffix <- function(vs1,vs2,sep=' ') {
    ## cycle string vectors to same length
    vsl <- max(length(vs1),length(vs2));
    vs1 <- rep(vs1,len=vsl);
    vs2 <- rep(vs2,len=vsl);
    ## precompute character splits
    ss1 <- strsplit(vs1,'');
    ss2 <- strsplit(vs2,'');
    ## iterate over each pair of strings
    sapply(seq_along(vs1),function(si) {
        s1 <- vs1[si];
        s2 <- vs2[si];
        s1l <- length(ss1[[si]]);
        s2l <- length(ss2[[si]]);
        slmin <- min(s1l,s2l);
        ## handle trivial case of exact equality
        if (s1==s2) return(s1);
        ## get prefix and suffix lengths
        if (slmin==0L) { ## empty string cannot result in a prefix or suffix
            pl <- sl <- 0L;
        } else {
            eq <- ss1[[si]][seq_len(slmin)] == ss2[[si]][seq_len(slmin)];
            pl <- if (all(eq)) slmin else if (eq[1L]==T) which(!eq)[1L]-1L else 0L;
            eq <- rev(ss1[[si]])[seq_len(slmin)]==rev(ss2[[si]])[seq_len(slmin)];
            sl <- if (all(eq)) slmin else if (eq[1L]==T) which(!eq)[1L]-1L else 0L;
        }; ## end if
        ## paste together prefix, sep-pasted middles, and suffix
        m1 <- substr(s1,pl+1L,s1l-sl);
        m2 <- substr(s2,pl+1L,s2l-sl);
        paste0(substr(s1,1L,pl),paste(sep=sep,m1,m2),substr(s1,s1l-sl+1L,s1l));
    });
} ## end pasteMergePrefixAndSuffix()
