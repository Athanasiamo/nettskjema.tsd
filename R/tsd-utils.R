#' @noRd
on_tsd <- function(){
  on_linux()
  k <- dir.exists("/tsd")
  if(!k)
    warning("You are not on linux TSD.\n ",
            "Some functions from this package may not work as intended.",
            call. = FALSE)
  k
}

#' @noRd
on_linux <- function(){
  k <- grepl("linux|gnu", utils::sessionInfo()$platform)
  if(!k)
    warning("You are not on a Linux computer.\n ",
            "Some functions from this package may not work as intended.",
            call. = FALSE)
  k
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
      warning("'output_dir' not set. defaulting to:\n", output_dir,
              call. = FALSE)
  }
  output_dir
}

#' @noRd
#' @importFrom stats setNames
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
