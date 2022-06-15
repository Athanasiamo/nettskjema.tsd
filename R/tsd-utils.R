#' Read nettskjema data
#'
#' Utility function to read in nettskjema data.
#' forced all columns to character to not loose leading zero.
#' @importFrom utils read.delim
#' @return a dataframe with nettskjema data
#' @param path string path to a nettskjema file
#' @examples
#' \dontrun{
#' read_nettskjema_tsd("/tsd/pxx/nettskjema/form.csv")
#' }
read_nettskjema_tsd <- function(path){
  read.delim(path, stringsAsFactors = FALSE, colClasses = character)
}

#' @noRd
on_tsd <- function(){
  on_linux()
  k <- dir.exists("/tsd")
  if(!k)
    message("You are not on linux TSD.\n ",
            "Some functions from this package may not work as intended.",
            call. = FALSE)
  invisible(k)
}

#' @noRd
on_linux <- function(){
  k <- grepl("linux|gnu", utils::sessionInfo()$platform)
  if(!k)
    message("You are not on a Linux computer.\n ",
            "Some functions from this package may not work as intended.",
            call. = FALSE)
  invisible(k)
}

#' @noRd
identify_project <- function(){
  stopifnot(on_tsd())
  project = Sys.getenv("nettskjema.tsd.project")
  if(project != "") return(project)
  strsplit(system2("whoami", stdout = TRUE), "-")[[1]][1]
}

#' @noRd
verbosity <- function(verbose = Sys.getenv("nettskjema.tsd.verbose")){
  verbose <- as.logical(verbose)
  if(!is.na(verbose) &&  is.logical(verbose)) return(verbose)
  TRUE
}

#' @noRd
durable_path <- function(project = Sys.getenv("nettskjema.tsd.project")){
  if(project == "")
    project <- identify_project()
  sprintf("/tsd/%s/data/durable", project)
}

#' @noRd
output_path <- function(output_dir = Sys.getenv("nettskjema.tsd.output.dir"),
                        project = identify_project(),
                        verbose = verbosity()){
  if(output_dir == ""){
    output_dir <- file.path(durable_path(project),
                            "nettskjema-submissions/cleaned/")
    if(verbose)
      message("'output_dir' not set. defaulting to:\n", output_dir,
              call. = FALSE)
  }
  output_dir
}

#' @importFrom stats setNames
#' @noRd
codebook_cols <- function(input_dir){
  docs <- list.files(input_dir, "^form", full.names = TRUE)
  if(length(docs) == 0) return(NA_character_)
  docs <- jsonlite::read_json(docs[length(docs)])
  k <- lapply(docs$pages, function(p){
    lapply(p$elements, function(z){
      lapply(z$questions, function(x){
        setNames(x$questionId, x$externalQuestionId)
      })
    })
  })
  unlist(k)
}

#' @noRd
mkdir <- function(dir, ...){
  if(!dir.exists(dir))
    dir.create(dir, recursive = TRUE, ...)
}

is_symlink <- function(paths){
  isTRUE(nzchar(Sys.readlink(paths), keepNA=TRUE))
}
