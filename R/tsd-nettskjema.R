#' Decrypt all files associated with a form
#'
#' @template formid
#' @template project
#' @param output_dir directory to output data
#' @template rerun_all
#' @template verbose
#' @template pretty
#'
#' @return logical. If new entries were decrypted or not.
#' @export
#'
# #' @examples
nettskjema_tsd_organise_form <- function(
  formid,
  project = NULL,
  output_dir = NULL,
  rerun_all = FALSE,
  verbose = NULL,
  pretty = TRUE){

  if(is.null(project))
    project <- identify_project()

  if(is.null(output_dir))
    output_dir <- output_path()

  if(is.null(verbose))
    verbose <- verbosity()

  path <- file.path(nettskjema_tsd_submission_path(project), formid)
  if(verbose) message("\nProcessing form ", basename(path))

  output_dir   <- file.path(output_dir, formid)
  output_dir_d <- file.path(output_dir, "submissions/decrypted")

  files <- list.files(path, "asc$", full.names = TRUE)
  outfiles <- gsub("\\.asc", "", basename(files))

  mkdir(output_dir_d, mode = "0777")
  if(!rerun_all){
    ext <- list.files(output_dir_d)
    idx <- outfiles %in% ext
    files <- files[!idx]
    outfiles <- outfiles[!idx]
  }

  backup_data(files, output_dir = file.path(output_dir, "submissions/encrypted"))
  outfiles <- file.path(output_dir_d, basename(outfiles))
  if(length(files) == 0){
    if(verbose) message("... no new files to process")
   return(FALSE)
  }
  if(verbose) message("... decrypting")
  k <- mapply(
    nettskjema_tsd_decrypt_file,
    path = files,
    output_path = outfiles,
    MoreArgs = list(pretty = pretty))

  combine_data(formid,
               input_dir = output_dir_d,
               output_dir = output_dir,
               verbose = verbose)

  tidy_attachments(
    output_dir_d,
    file.path(output_dir, "attachments"),
    verbose = verbose
  )

  Sys.chmod(paths = c(outfiles, files), mode = "0755")

  TRUE
}

#' Decrypt all forms associated with a project
#'
#' @template rerun_all
#' @param ... arguments to pass to \link{nettskjema_tsd_organise_form}
#'
#' @return nothing. Writes decrypted files to a folder.
#' @export
#'
# #' @examples
nettskjema_tsd_organise_all_forms <- function(
  rerun_all = FALSE,
  ...
){
  formids <- list.files(nettskjema_tsd_submission_path(),
                        "^[0-9]{5}", recursive = FALSE)

  mapply(nettskjema_tsd_organise_form,
         formid = formids,
         MoreArgs = list(
           rerun_all = rerun_all,
           ...)
  )
}
