

#' Combine csv's from Nettskjema (on TSD)
#'
#' The submissions received on TSD arrive as both
#' csv and jsons. Combining all these files
#' produce the entire data from a form.
#' This function combines all the jsons within TSD
#' to create a unified data.frame. If \code{output_file}
#' is provided, will also write the the data for a file.
#'
#' @template input_dir
#' @template output_file
#'
#' @return data.frame with all data
#' @export
#'
#' @importFrom jsonlite read_json
#' @importFrom stats na.omit
#' @importFrom utils write.table
#' @examples
#' \dontrun{
#' nettskjema_tsd_json2data("/durable/nettskjema-submissions/cleaned/80902")
#'
#' nettskjema_tsd_json2data("/durable/nettskjema-submissions/cleaned/80902",
#'     output_file = "/durable/nettskjema-submissions/cleaned/80902/data.csv")
#' }
nettskjema_tsd_json2data <- function(input_dir, output_file = NULL){
  files <- list.files(input_dir, "^[0-9]{5}.*json$", full.names = TRUE)
  files <- files[!grepl("-|_", basename(files))]
  jsons <- lapply(files, jsonlite::read_json)

  if(length(jsons) == 0){
    warning("No data to combine", call. = FALSE)
    return(NA)
  }

  dt <- lapply(jsons, function(dt){
    flat <- rapply(dt, function(x){x})

    main_el <- flat[!grepl("^answersAsMap", names(flat))]

    type <- sapply(dt$answersAsMap, function(x) length(x$answerOptions) != 0)

    answ <- sapply(1:length(dt$answers),
                   function(x){
                     if(type[x]){
                       paste0(dt$answers[[x]]$answerOptions[[1]]$externalAnswerOptionId,
                              collapse = ";-;")
                     }else{
                       dt$answers[[x]]$unfilteredTextAnswer
                     }
                   })
    names(answ) <- names(type)
    c(main_el, answ)
  })
  cols <- unique(unlist(sapply(dt, names, simplify = FALSE)))
  dt <- lapply(dt,
               fix_data,
               cols = cols
  )
  dt <- do.call(rbind, dt)
  dt <- as.data.frame(dt, stringsAsFactors = FALSE)
  names(dt) <- cols
  dt$submissionId <- gsub("\\.json", "", basename(files))
  cols_nm <- codebook_cols(input_dir)

  if(!all(is.na(cols_nm))){
    cols_mm <- na.omit(match(names(dt), cols_nm))
    names(dt)[names(dt) %in% cols_nm] <- names(cols_nm)[cols_mm]
  }

  if(any(is.na(names(dt))))
    stop("Codebook is missing for at least one question. Cannot combine data.",
         call. = FALSE)

  col_order <- c("submissionId", cols[!grepl("^[0-9]{4}", cols)], names(cols_nm)[names(cols_nm) %in% names(dt)])
  dt <- dt[,col_order]
  dt <- as.data.frame(lapply(dt, function(x) gsub("\n|\t|\r", ", ", x)))
  dt <- dt[order(dt$submissionId),]
  if(!is.null(output_file))
    utils::write.table(dt,
                       output_file,
                       sep = "\t",
                       row.names = FALSE,
                       quote = FALSE)
  dt
}

#' Combine csv's from Nettskjema (on TSD)
#'
#' The submissions received on TSD arrive as both
#' csv and jsons. Combining all these files
#' produce the entire data from a form.
#' This function combines all the csv within TSD
#' to create a unified data.frame. If \code{output_file}
#' is provided, will also write the the data for a file.
#'
#' @template input_dir
#' @template output_file
#'
#' @return data.frame with all data
#' @export
#'
#' @examples
#' \dontrun{
#' nettskjema_tsd_csv2data("/durable/nettskjema-submissions/cleaned/80902")
#'
#' nettskjema_tsd_csv2data("/durable/nettskjema-submissions/cleaned/80902",
#'     output_file = "/durable/nettskjema-submissions/cleaned/80902/data.csv")
#' }
nettskjema_tsd_csv2data <- function(input_dir, output_file = NULL){
  files <- list.files(input_dir, "^[0-9]{5}.*csv$", full.names = TRUE)

  # remove any file with - or _ these are attachments
  files <- files[!grepl("-|_", basename(files))]

  if(length(files) == 0){
    warning("No data to combine", call. = FALSE)
    return(NA)
  }

  dt <- lapply(files, read_nettskjema_tsd)
  cols <- unique(unlist(sapply(dt, names)))
  miss <- sapply(dt, function(x) any(!cols %in% names(x)))

  if(any(miss)){
    for(k in which(miss)){
      tmp <- !cols %in% names(dt[[k]])
      dt[[k]][,cols[tmp]] <- NA
    }
  }

  dt <- do.call(rbind, dt)
  if(!"submissionId" %in% names(dt)){
    dt$submissionId <- gsub(".csv", "", basename(files))
    dt <- dt[, c("submissionId", names(dt)[-ncol(dt)])]
  }
  dt <- dt[order(dt$submissionId),]

  if(!is.null(output_file))
    utils::write.table(dt, output_file,
                       sep = "\t",
                       row.names = FALSE,
                       quote = FALSE)
  return(dt)
}


combine_data <- function(formid, input_dir, output_dir,
                         verbose = verbosity()){

  files <- list.files(input_dir, "^[0-9]{5}.*json$", full.names = TRUE)
  files <- files[!grepl("-|_", basename(files))]

  if(length(files) == 0){
    if(verbose) message("... no files to combine")
    return()
  }

  if(verbose) message("... combining submissions")

  dt_json <- nettskjema_tsd_json2data(
    input_dir,
    sprintf("%s/form-%s-json.csv",
            output_dir, formid)
  )

  dt <- nettskjema_tsd_csv2data(
    input_dir,
    sprintf("%s/form-%s.csv",
            output_dir, formid)
  )
}

#' tidy attachments
#'
#' Attachments are decrypted into the raw folder.
#' This function symlinks those to another folder
#' for easier and more transparent access
#'
#' @template input_dir
#' @template output_dir
#' @template verbose
#' @param force logical, if new symlinks should be force created.
#' @param ... other arguments to \code{\link[base]{mapply}}
#'
#' @return invisible logical list of whether the symlink is made
#' @noRd
tidy_attachments <- function(input_dir, output_dir, force = TRUE, verbose = NULL, ...){
  if(is.null(verbose))
    verbose <- verbosity()

  files <- list.files(input_dir, "^[0-9]{5}.*[-|_]", full.names = TRUE)
  links <- file.path(output_dir, basename(files))

  if(length(links) > 1){
    if(verbose) cat("... tidying attachments\n")
    mkdir(output_dir)
    if(force){
      links_e <- sapply(links, is_symlink)
      k <- sapply(links[links_e], file.remove)
    }
    invisible(
      mapply(file.symlink,
             to = links,
             from = files,
             ...)
    )
  }
}

#' Restructure data
#'
#' Some times internal or even research data in forms get altered.
#' This creates issues when mergin data across the files.
#' This function attempts to re-order the data to a pre-specified
#' order that is the same across all files.
#' @param x data.frame to fix
#' @param cols columns names to reorder as
#' @return re-ordered data.frame
#' @noRd
fix_data <- function(x, cols){
  dt <- data.frame(rr = 1:2, stringsAsFactors = FALSE)
  sapply(cols, function(i) x[i])
}

#' Clean jsons
#'
#' Default jsons come in one line, saved space but is
#' hard to read for humans. This function replaces
#' decrypted jsons with 'pretty' jsons on multiple
#' lines
#'
#' @param files character vector of full paths to jsons
#'     you want re-written
#' @param ... other arguments to \code{\link[base]{mapply}}
#' @noRd
prettify_jsons <- function(files, ...){
  files <- files[grepl("json$", basename(files))]
  jsons <- lapply(files, jsonlite::read_json)
  invisible(
    mapply(jsonlite::write_json,
           x = jsons,
           path = files,
           MoreArgs = list(pretty = TRUE,
                           auto_unbox = TRUE),
           ...
    )
  )
}


backup_data <- function(files,
                        output_dir = NULL,
                        ...){
  mkdir(output_dir, mode = "0755")

  invisible(
    mapply(file.copy,
           from = files,
           to = file.path(output_dir, basename(files)),
           MoreArgs = list(
             copy.mode = TRUE,
             copy.date = TRUE),
           ...
    )
  )

}

