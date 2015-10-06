source <- function(...) {
  git2r::hashfile()
  base::source(...)
}

repo <- function() {
  git2r::repository('.')
}
