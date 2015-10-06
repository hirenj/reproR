
wanted_files <- list()

#' @export
reset <- function() {
  wanted_files <<- list()
}

#' @export
source <- function(file,...) {
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
  wanted_files <<- unique( c( wanted_files, file ) )
  base::source(file,...)
}

#' @export
read.delim <- function(...) {
  read.table(...)
}

# we also need read.table
#' @export
read.table <- function(file=filename,...) {
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
  wanted_files <<- unique( c( wanted_files, file ) )
  utils::read.table(file,...)
}

#' @export
generatePatch <- function(patchfile='changed.patch') {
  file.remove(patchfile)
  status = git2r::status(repo())
  missing_files = intersect ( unlist(status$untracked) , wanted_files )
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
