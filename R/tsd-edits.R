#' Apply edits to nettskjema data
#'
#' Nettskjema data can at times contain errors
#' that require manual editing. Editing raw
#' files directly is not recommended practice,
#' as logging these changes often is not maintained
#' over time. Applying edits to nettskjema data
#' through a more transparent manner means keeping
#' raw data in its original state, while also
#' keeping track of the changes needed. This function
#' needs a json formatted file to apply edits to
#' either raw csv or json data from nettskjema.
#' TODO: make vignette with instructions on edit.json
#' TODO: make functions to initiate/add edits
#'
#' @param input_file string. file path to the raw nettskjema
#'     file, that contains all answers for a form, you want to apply edits to.
#' @param edits_file string. file path to the json with
#'     the edits that should be applied
#' @template output_file
#' @template verbose
#'
#' @return data.frame with edited nettskjema data
#' @export
#'
# @examples
nettskjema_tsd_apply_edits <- function(input_file,
                                       edits_file,
                                       output_file = NULL,
                                       verbose = NULL){
  stopifnot(file.exists(input_file))
  stopifnot(file.exists(edits_file))
  if(is.null(verbose))
    verbose <- verbosity()

  if(verbose) message("... applying data edits")
  apply_edits(input_file,
              edits_file,
              output_file)
}

#' Apply edits to all nettskjema data
#'
#' @param input_dir directory path where the input files are
#' @param pattern regex pattern to match files with
#' @template project
#' @template output_dir
#' @param suffix character. String to be added at the end of files with edits applied.
#'
#' @return data frame with edits applied
#' @export
#'
# #' @examples
nettskjema_tsd_apply_edits_all <- function(input_dir = NULL,
                                           pattern = NULL,
                                           suffix = "-ed",
                                           project = NULL,
                                           output_dir = NULL){
  if(is.null(input_dir))
    input_dir <- output_path()

  if(is.null(project))
    project <- identify_project()

  if(is.null(output_dir))
    output_dir <- output_path()

  forms <- list.dirs(input_dir, recursive = FALSE)
  forms <- forms[grepl("^[0-9]{4}", basename(forms))]
  form_ids <- basename(forms)

  if(is.null(pattern)){
    files <- unlist(lapply(c(".csv", "-json.csv"), function(x){
      sprintf("%s/form-%s%s", forms, form_ids, x)
    }))
    files <- files[order(files)]
  }else{
    files <- unname(unlist(sapply(forms, list.files, pattern = pattern, full.names = TRUE)))
  }
  stopifnot(length(files) > 1)
  dt <- data.frame(
    form = basename(dirname(files)),
    input = files,
    output = gsub("\\.csv", "-%s.csv", files, suffix),
    stringsAsFactors = FALSE
  )
  dt$edits <- sprintf("%s/edits-%s.json", dirname(dt$input), dt$form)
  ed_idx <- sapply(dt$edits, file.exists)
  dt <- dt[ed_idx, ]
  ret <- mapply(nettskjema_tsd_apply_edits,
                input_file = dt$input,
                edits_file = dt$edits,
                output_file = dt$output,
                MoreArgs = list(verbose = FALSE)
  )
  names(ret) <- dt$output
  ret
}

#' @importFrom jsonlite read_json
#' @importFrom utils read.delim write.table
#' @noRd
apply_edits <- function(path, edits, outfile = NULL){
  if(file.info(path)$size <= 1) return()
  dt <- read.delim(path, stringsAsFactors = FALSE)
  ed <- read_json(edits)
  for(x in names(ed)){
    tmp <- ed[[x]][["data"]]
    idx <- match(x, dt[,"submissionId"])
    if(is.na(idx)){
      warning(sprintf("SubmissionId '%s' not found in data file %s", x, path), call. = FALSE)
    }else if(length(tmp) == 0){
      dt <- dt[idx*-1,]
    }else{
      for(i in 1:length(tmp)){
        if("value" %in% names(tmp[[i]])){
          dt[idx, names(tmp)[i]] <- tmp[[i]][["value"]]
        }else{
          warning(sprintf("No new value is set for column '%s' for SubmissionId '%s' in data file %s",
                          names(tmp)[i], x, path),
                  call. = FALSE)
        }
      }
    }
  }

  if(!is.null(outfile))
    write.table(dt, outfile, sep = "\t", row.names = FALSE, quote = FALSE)
  dt
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("change_comment", "edit", "key", "SubmissionId"))
}
