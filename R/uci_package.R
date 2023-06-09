#' uci: Urban Centrality Index
#'
#' Calculate Urban Centrality Index
#'
#' @section Usage:
#' Please check the vignettes and data documentation on the
#' [website](https://rafapereirabr.github.io/uci/).
#'
#' @docType package
#' @name uci
#' @aliases uci-package
#'
#' @importFrom utils globalVariables
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables( c('b',
                          'c') )

