# #'  [!!!] Simulate/Construct a correlation matrix
# #'
# #' @param n A number of rows/columns in a symmetric matrix. Used
# #' to construct a matrix with random coefficeints from
# #' uniform distribution with values distributed between [-1;1].
# #'
# #' @param vec A Vector with known correlation coefficients to construct
# #'            a correlation matrix.
# #'
# #' @return A simulated/constructed correlation matrix.
# #' @export
# #' @seealso \link{nTri2nRow}, \link{nRow2nTri}, \link{simCorrStNorm}
# #' @examples
# #'
# #' # ------------------------------------------------------------
# #' # Example 1: generate a matrix of random coefficients
# #'
# #' simCorrMat(3)
# #'
# #' # ------------------------------------------------------------
# #' # Example 2A: Vector into a matrix
# #'
# #' vector = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
# #' simCorrMat(vector)
# #'
# #' # ------------------------------------------------------------
# #' # Example 2B: Vector is transformed to a matrix by filling
# #' #  it column-wise:
# #'
# #'  # vector = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
# #'  #
# #'  #  matrix =
# #'  #       [,1] [,2] [,3] [,4]
# #'  #  [1,]   .     .   .    .
# #'  #  [2,]  0.1    .   .    .
# #'  #  [3,]  0.2  0.4   .    .
# #'  #  [4,]  0.3  0.5  0.6   .
# #'  #
# #'  # In this example only the matrix elements of interest are shown.
# #'
# #' # ------------------------------------------------------------
# #' # Example 3: inappropriate number of coefficients - warning appears
# #'
# #' simCorrMat(vector[1:5])
# #'  ## Warning message:
# #'  ## Only first 3 coefficient(s) out of 5 will be used to construct symmetric matrix with 3 rows.
# #'
# #' # ------------------------------------------------------------
# #' # Example 4: ERROR appears - coeefs must be between [-1;1]
# #'
# #' simCorrMat(1:5)
# #'  ## Error in simCorrMat(1:5) :
# #'  ## All values in input vector must be between [-1 and 1]
# #'
# #' @family simmulation functions
# simCorrMat <- function(n=5, vec = NULL){
#     # If n is not a scalar
#     if(is.vector(n)&length(n)>1) vec <- n
#
#     if (!is.null(vec)){
#         rm(n)
#
#         if (!all(abs(vec)<=1)) stop("All values in input vector must be between [-1 and 1]")
#         nCoeff  <- length(vec)
#         n       <- nTri2nRow(nCoeff) # calculate number of summetric matrix rows
#         n.floor <- floor(n)
#         n.ceil  <- ceiling(n)
#         if (n.floor != n.ceil) { # if not an integer
#             warning(sprintf('Only first %d coefficient(s) out of %d will be used to construct symmetric matrix with %d rows.',
#                             nRow2nTri(n.floor), nCoeff,n.floor))
#             n   <- n.floor
#             vec <- vec[1:n] # prevent additional warning with `lower.tri()`
#         }
#     } else {
#         # Create a Vector of RANDOM coefficients
#         nCoeff <- nRow2nTri(n)
#         vec <- runif(nCoeff, min = -1, max = 1)
#     }
#
#     if (n<2) stop("`n` must be ineger grerater than or equal to 2.")
#
#     # Generate an idendity matrix (ones in diagonal)
#     M <- diag(n)
#
#     # Fill lower part of the matrix with coefficients
#     M[lower.tri(M)] <- vec
#
#     # Make the matrix symmetric
#     M[upper.tri(M)] <- t(M)[upper.tri(M)]
#
#
#     return(M)
# }
