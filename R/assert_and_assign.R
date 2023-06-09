#' @keywords internal
assert_var_name <- function(sf_object, var_name) {

  checkmate::assert_data_frame(sf_object)
  checkmate::assert_class(sf_object, 'sf')
  checkmate::assert_names(
    names(sf_object),
    must.include = var_name,
    .var.name = "sf_object"
  )

  return(invisible(TRUE))
}


