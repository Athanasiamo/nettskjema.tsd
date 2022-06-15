
#' Check if on Linux on package load
#'
#' Some package functionality relies on
#' Linux (like the GPG decryption).
#' Outputting a warning at package load
#' if not on Linux to make this obvious.
#'
#' @noRd
.onLoad <- function(libname, pkgname){
    on_tsd()
}
