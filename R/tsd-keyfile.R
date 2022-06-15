#' Nettskjema TSD keyfile location
#'
#' As a security measure, all submissions to Nettskjema data in to
#' TSD are encrypted. These encrypted files need to be decrypted
#' before they can be sorted and prepared for use in R (or  other
#' programs). To decrypt the data, R needs to have access to the
#' secret key that will verify that the decryption is ok to carry
#' out.
#' This function can verify the existence of a key-file given a
#' specified location, or attempt to auto-detect a key-file.
#'
#' @param path full path to the key-file
#' @template project
#' @template verbose
#'
#' @return character with the full verified path
#' @export
#'
#' @examples
#' \dontrun{
#' nettskjema_tsd_keyfile()
#' nettskjema_tsd_keyfile(path = "/my/keyfile/path/secret.asc")
#' }
nettskjema_tsd_keyfile <- function(path = Sys.getenv("nettskjema.tsd.keyfile"),
                                   project = NULL,
                                   verbose = NULL){
  stopifnot(on_tsd())
  if(is.null(project))
    project <- identify_project()

  if(is.null(verbose))
    verbose <- verbosity()

  if(path != ""){
    if(file.exists(path)) return(path)
    stop("Nettskjema gpg keyfile not found in specified location.",
         "Check for spelling mistakes:\n",
         path, call. = FALSE)
  }

  if(verbose) message("Attempting to auto-detect gpg keyfile.\n")

  path <- list.files(durable_path(project), "gpg",
                     full.names = TRUE, ignore.case = TRUE)
  path <- list.files(path, "priv|secr", full.names = TRUE)

  if(length(path) == 0)
    stop("gpg keyfile not found. Please provide the path manually.",
         call. = FALSE)

  path
}

#' Import GPG key
#'
#' As a security measure, all submissions to Nettskjema data in to
#' TSD are encrypted. These encrypted files need to be decrypted
#' before they can be sorted and prepared for use in R (or  other
#' programs). To decrypt the data, R needs to have access to the
#' secret key that will verify that the decryption is ok to carry
#' out.
#' This function will import the necessary key for the decryption
#' to take place.
#' If the key-file for the decryption is not automatically detected,
#' contact IT to learn where your key has been stored. Any R-script
#' you will run can then specify the location of the key-file either
#' directly in the call to this function, or as an R-environment variable
#' 'nettskjema.tsd.keyfile=/full/path/to/keyfile'.
#'
#' @param path full path to the key-file.
#' @template verbose
#'
#' @return Nothing. Imports the key to the session
#' @export
#'
#' @examples
#' \dontrun{
#' nettskjema_tsd_import_key()
#' nettskjema_tsd_import_key(verbose = FALSE)
#' nettskjema_tsd_import_key(path = "/my/keyfile/path/secret.asc")
#' }
nettskjema_tsd_import_key <- function(path = NULL,
                                      verbose = NULL){
  stopifnot(on_tsd())
  if(is.null(verbose))
    verbose <- verbosity()
  if(is.null(path))
    path <- nettskjema_tsd_keyfile(verbose = verbose)
  k <- sapply(path, function(x)
    system2(
      "gpg",
      sprintf("--import %s", x),
      stdout = TRUE, stderr = TRUE
    )
  )
  if(verbose)
    invisible(
      lapply(k, function(x) message(paste(x, collapse="\n")))
    )
  invisible(k)
}


#' Detect key hash
#' @template project
#' @return character of length 15.
#' @noRd
key_hash <- function(project = NULL){
  stopifnot(on_tsd())
  if(is.null(project))
    project <- identify_project()
  path <- sprintf("%s/nettskjema-submissions",
                  durable_path(project))
  k <- list.files(path,
                  recursive = FALSE)
  k[grepl("[0-9A-Z]{15}$", k)]
}

#' Check if Nettskjema key is imported
#' @return logical. If key is imported
#' @noRd
check_key <- function(){
  stopifnot(on_tsd())
  hash <- basename(nettskjema_tsd_submission_path())
  hash <- substr(hash, 9, 16)
  resp <- system2("gpg", "--list-secret-keys", stdout = TRUE)
  any(grepl(hash, resp))
}
