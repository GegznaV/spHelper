# =============================================================================
#
#' Summary statistics of factor variable in a hypecSpec object by ID
#'
#' @param OBJ hyperSpec object
#' @param Var (string) variable name of factor variable
#' @param ID (string) variable name of ID variable
#' @param na.rm (logical) used by function \code{sum()}. Default is \code{TRUE}.
#' @param decimals (integer) number of decimals to round to calculating the percentages.
#'                Default is 2.
#' @param include_na (logical) If \code{TRUE}, \code{NA} values are included.
#' @param na_level (character) Name for the level, which represents \code{NA} value.
#' @param FALSE (logical) Add row with "total".
#'
#' @return a data frame with counts and percentages by group and ID
#' @export
#'
#' @examples
#'
#' count_spectra(Spectra2, "class", ID = "gr")
#' count_spectra(OBJ = Spectra2, Var = "class", ID = "gr")
#'
#' @family count spectra functions
#' @importFrom forcats fct_explicit_na
count_spectra <- function(OBJ,
                          Var = NULL, # colnames(OBJ)[1],
                          ID = "ID",
                          decimals   = 1,
                          include_na = TRUE,
                          na_level = "(Missing)",
                          na.rm = TRUE,
                          total = FALSE) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define necessary functions

    # Get counts and percentages
    n_unique <- function(x) {unique(x) %>% length()}

    get_percent <- function(x) {
        sprintf(fmt = stringr::str_glue("%.{decimals}f%%"),
                100 * (x / sum(x, na.rm = na.rm)))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    chk.hy(OBJ)
    if (!ID %in% colnames(OBJ)) {
        stop("The dataset does not contain variable called `", ID, "`")
    }

    # For whole dataset
    if (is.null(Var)){ # new part of the function

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Extract necessary variables
        ID_  <- eval_glue("OBJ$`{ID}`")

        if (include_na == TRUE) {
            ID_  %<>% forcats::fct_explicit_na(na_level = "(Missing)")
        }

        n_ID      <- n_unique(ID_)
        n_spectra <- length(ID_)

        percent_ID      <- get_percent(n_ID)
        percent_spectra <- get_percent(n_spectra)


    # For subsets by values of `VAR`
    } else { # The original part of the function

    if (!Var %in% colnames(OBJ)) {
        stop("The dataset does not contain variable called `", Var, "`")
    }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Extract necessary variables
        Var_ <- eval_glue("OBJ$`{Var}`")
        ID_  <- eval_glue("OBJ$`{ID}`")

        # Get counts and percentages
        n_unique <- function(x) {unique(x) %>% length()}

        if (include_na == TRUE) {
            ID_  %<>% forcats::fct_explicit_na(na_level = "(Missing)")
            Var_ %<>% forcats::fct_explicit_na(na_level = "(Missing)")
        }

        n_ID      <- tapply(ID_, Var_, n_unique, default = 0)
        n_spectra <- tapply(ID_, Var_, length,   default = 0)

        percent_ID      <- get_percent(n_ID)
        percent_spectra <- get_percent(n_spectra)

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return results

    if (total == FALSE) {
        data.frame(n_ID,
                   n_spectra,
                   percent_ID,
                   percent_spectra,
                   check.names = FALSE)
    } else {
        data.frame(n_ID, n_spectra, percent_ID, percent_spectra,
                   check.names = FALSE) %>%
            tibble::rownames_to_column("Group") %>%
            tibble::add_case(Group = "TOTAL:",
                             n_ID = sum(n_ID),
                             n_spectra = sum(n_spectra),
                             percent_ID = "",
                             percent_spectra = "" ) %>%
            tibble::column_to_rownames("Group")
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# =============================================================================
#
#' Get names of groups that has to few unique cases.
#'
#' @param OBJ hyperSpec object
#' @param Var (string) variable name of factor variable
#' @param ID (string) variable name of ID variable
#' @param n_min (integer) minimum number of unique IDs not to be treated as too few.
#' @param na.rm (logical) used by function \code{sum()}. Default is \code{TRUE}.
#'
#' @return Vector of strings.
#' @export
#'
#' @examples
#' has_too_few_IDs(Spectra2, "class", ID = "gr")
#'
#' has_too_few_IDs(Spectra2, "class", ID = "gr", n_min = 2)
#'
#' @family count spectra functions
#'
has_too_few_IDs <- function(OBJ,
                            Var = colnames(OBJ)[1],
                            ID = "ID",
                            n_min = 5,
                            na.rm = TRUE){

    DF <- OBJ[, c(ID, Var)]$..
    if (na.rm) DF <- tidyr::drop_na(DF)
    DF <- distinct(DF, .keep_all = TRUE)
    gr_ <- table(DF[,2], useNA = "ifany")
    names(gr_[gr_ < n_min])

    # gr_ <- count_spectra(OBJ, Var = Var, ID = ID, na.rm = na.rm)$n_ID
    # names(gr_[gr_ < n_min])
}

# =============================================================================
#
#' Get names of groups that has enough unique cases.
#'
#' @param OBJ hyperSpec object
#' @param Var (string) variable name of factor variable
#' @param ID (string) variable name of ID variable
#' @param n_min (integer) minimum number of unique IDs not to be treated as too few.
#' @param na.rm (logical) used by function \code{sum()}. Default is \code{TRUE}.
#'
#' @return Vector of strings.
#' @export
#'
#' @examples
#' has_enough_IDs(Spectra2, "class", ID = "gr")
#'
#' has_enough_IDs(Spectra2, "class", ID = "gr", n_min = 2)
#'
#' @family count spectra functions
#'
has_enough_IDs <- function(OBJ,
                           Var = colnames(OBJ)[1],
                           ID = "ID",
                           n_min = 5,
                           na.rm = TRUE){


    DF <- OBJ[, c(ID, Var)]$..
    if (na.rm) DF <- tidyr::drop_na(DF)
    DF <- distinct(DF, .keep_all = TRUE)
    gr_ <- table(DF[,2], useNA = "ifany")
    names(gr_[gr_ >= n_min])

    # gr_ <- count_spectra(OBJ, Var = Var, ID = ID, na.rm = na.rm)$n_ID
    # names(gr_[gr_ >= n_min])
}

# =============================================================================
