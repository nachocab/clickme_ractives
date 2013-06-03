#' Generates a JavaScript visualization using Vega
#'
#' @param data input data
#' @param spec Name of the vega spec file to use, it must match a file within \code{vega/data/spec/}
#' @param ... additional arguments for \code{clickme}
#' @export
clickme_vega <- function(data, spec, ...){
    dots <- list(...)

    if (is.null(dots$params)) {
        params <- list(spec = spec)
    } else {
        params <- dots$params
        dots$params <- NULL
        params$spec = spec
    }

    if (is.null(dots$data_prefix)){
        dots$data_prefix <- paste0("data_", spec)
    }
    data_prefix <- dots$data_prefix
    dots$data_prefix <- NULL

    if (is.null(dots$browse)){
        dots$browse <- interactive()
    }
    browse <- dots$browse
    dots$browse <- NULL

    if (length(dots) != 0){
        clickme(data, "vega", browse = browse, params = params, data_prefix = data_prefix, dots)
    } else {
        clickme(data, "vega", browse = browse, params = params, data_prefix = data_prefix)
    }
}