#' Decrypt nettskjema file
#'
#' @param path path to file to decrypt
#' @param output_path path to output decrypted file to. NULL will return
#'     file content to R.
#' @template pretty
#'
#' @return raw decrypted file content as text
#' @export
#'
# #' @examples
nettskjema_tsd_decrypt_file <- function(path,
                                        output_path = gsub("\\.asc", "", path),
                                        pretty = TRUE
){

  if(!check_key())
    nettskjema_tsd_import_key()

  args <- sprintf("--decrypt %s --batch --yes", path)

  if(!is.null(output_path))
    args <- sprintf("--output %s %s",
                    output_path, args)

  ret <- system2("gpg", args, stdout = TRUE, stderr = TRUE)

  if(pretty){
    prettify_jsons(output_path)
  }

  invisible(ret)
}


#' Decrypt all files associated with a form
#'
#' @param path path to the folder with encrypted data
#' @template rerun_all
#' @template output_dir
#' @param ... other arguments to \link{nettskjema_tsd_decrypt_file}
#'
#' @return nothing. Writes decrypted files to a folder.
#' @export
#'
# #' @examples
nettskjema_tsd_decrypt_form <- function(path,
                                        rerun_all = FALSE,
                                        output_dir = NULL,
                                        ...
){
  if(is.null(output_dir))
    output_dir <- output_path()

  if(is.null(verbose))
    verbose = verbosity()

  if(verbose) message("Decrypting form ", basename(path), "\n")

  files <- list.files(path, "asc$", full.names = TRUE)
  outfiles <- gsub("\\.asc", "", basename(files))

  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
  }else{
    if(!rerun_all){
      ext <- list.files(output_dir)
      idx <- outfiles %in% ext
      files <- files[!idx]
      outfiles <- outfiles[!idx]
    }
  }

  outfiles <- file.path(output_dir, basename(outfiles))

  if(length(files) == 0){
    warning("No new files to decrypt", call. = FALSE)
    return(invisible())
  }

  if(verbose){
    pbapply::pbmapply(
      nettskjema_tsd_decrypt_file,
      path = files,
      output_path = outfiles,
      MoreArgs = list(...)
    )
  }else{
    mapply(
      nettskjema_tsd_decrypt_file,
      path = files,
      output_path = outfiles,
      MoreArgs = list(...),
    )
  }
}

#' Decrypt all Nettskjema forms
#'
#' Given that all forms are placed in the same basic folder
#' structures (as they are delivered to TSD), this function
#' will detect all forms in the path provided and decrypt all
#' the forms available.
#'
#' @param path path to the folder containing all the forms
#' @template project
#' @template output_dir
#' @template rerun_all
#' @template verbose
#' @param ... other arguments to \code{\link[base]{mapply}}
#'
#' @export
#' @return success list
nettskjema_tsd_decrypt_all_forms <- function(
  path = NULL,
  project = NULL,
  output_dir = NULL,
  rerun_all = FALSE,
  verbose  = NULL,
  ...){

  if(is.null(path))
    path <- nettskjema_tsd_submission_path()

  formids <- list.files(path, "^[0-9]{5}", recursive = FALSE)
  mapply(nettskjema_tsd_decrypt_form,
         path = file.path(path, formids),
         output_dir = file.path(output_dir, formids),
         MoreArgs = list(
           project = project,
           rerun_all = rerun_all,
           verbose = verbose),
         ...
  )
}


#' Find the path for encrypted Nettskjema data
#'
#' @template project
#'
#' @return single character path
#' @export
nettskjema_tsd_submission_path <- function(project = NULL){
  if(is.null(project))
    project <- identify_project()
  path <- sprintf("%s/nettskjema-submissions/%s",
                  durable_path(project), key_hash())
  if(!dir.exists(path))
    stop("Submission path does not exist.\n", path, call. = FALSE)
  path
}

