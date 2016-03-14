cacheEnv <- new.env()

assign('wanted_files',list(),envir=cacheEnv)

is_repo <- function() {
  git2r::in_repository('.')
}

#' Reset watchfile list
#' @export
reset <- function() {
  assign('wanted_files',list(),envir=cacheEnv)
}

#' source files
#' @export
source <- function(file,...) {
  if ( ! is_repo() ) {
    return(base::source(file,...))
  }
  status = git2r::status(repo())
  dirty_files = unlist(status$unstaged)
  missing_files = unlist(status$untracked)

  # Autocommit any staged files when
  # there's a flag

  # Autostage and autocommit any files
  # when a flag is set
  dirty = F
  if (file %in% dirty_files) {
    message(file," has untracked changes, commit to get a version number.")
    dirty=T
  }
  if (file %in% missing_files) {
    message(file," is not in the repository, you can't reproduce this.")
    dirty=T
  }
  if (! dirty) {
    message(file,git2r::commits(repo())[[1]]@sha)
  }
  # We should show the SHA hash for the commit object for this repository
  # and store the diff somewhere if it has been changed
  assign('wanted_files', unique( c( get('wanted_files',envir=cacheEnv), file ) ))
  base::source(file,...)
}

#' Add a table to the watchfiles
#' @export
read.delim <- function(...) {
  read.table(...)
}

#' Add a table to the watchfiles
#' @export
read.table <- function(file=filename,...) {
  if ( ! is_repo() ) {
    return(utils::read.table(file,...))
  }
  status = git2r::status(repo())
  dirty_files = unlist(status$unstaged)
  missing_files = unlist(status$untracked)

  # Autocommit any staged files when
  # there's a flag

  # Autostage and autocommit any files
  # when a flag is set
  dirty = F
  if (file %in% dirty_files) {
    message(file," has untracked changes, commit to get a version number.")
    dirty=T
  }
  if (file %in% missing_files) {
    message(file," is not in the repository, you can't reproduce this.")
    dirty=T
  }
  if (! dirty) {
    message(file,git2r::commits(repo())[[1]]@sha)
  }
  # We should show the SHA hash for the commit object for this repository
  # and store the diff somewhere if it has been changed
  assign('wanted_files', unique( c( get('wanted_files',envir=cacheEnv), file ) ))
  utils::read.table(file,...)
}

#' Add a file read in via readLines
#' @export
readLines <- function(con = stdin(),...) {
  if ( ! is_repo() || ! is.character(con) ) {
    return(base::readLines(con,...))
  }
  status = git2r::status(repo())
  dirty_files = unlist(status$unstaged)
  missing_files = unlist(status$untracked)

  # Autocommit any staged files when
  # there's a flag

  # Autostage and autocommit any files
  # when a flag is set
  dirty = F
  if (con %in% dirty_files) {
    message(con," has untracked changes, commit to get a version number.")
    dirty=T
  }
  if (con %in% missing_files) {
    message(con," is not in the repository, you can't reproduce this.")
    dirty=T
  }
  if (! dirty) {
    message(con,git2r::commits(repo())[[1]]@sha)
  }
  # We should show the SHA hash for the commit object for this repository
  # and store the diff somewhere if it has been changed
  assign('wanted_files', unique( c( get('wanted_files',envir=cacheEnv), con ) ))
}

#' Generate a patch file from all the files we are watching
#' @export
generatePatch <- function(patchfile='changed.patch') {
  if ( ! is_repo() ) {
    return()
  }
  file.remove(patchfile)
  status = git2r::status(repo())
  missing_files = intersect ( unlist(status$untracked) , get('wanted_files',envir=cacheEnv) )
  for (file in missing_files) {
    git2r::add(repo(),file)
  }
  wd.patch = tempfile("patch")
  index.patch = tempfile("patch")
  git2r::diff(repo(),filename=wd.patch,as_char=T)
  git2r::diff(repo(),filename=index.patch,index=T,as_char=T)
  fileConn<-file(patchfile)
  writeLines(c( readChar(index.patch, file.info(index.patch)$size), readChar(wd.patch, file.info(wd.patch)$size) ), fileConn)
  close(fileConn)

  for (file in missing_files) {
    .Call(git2r:::git2r_index_remove_bypath,repo(),file)
  }
}

repo <- function() {
  git2r::repository('.')
}
