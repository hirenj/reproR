#' Generate markdown
#' @export
status.md <- function(session=F) {
	current_commit = git2r::commits(repo())[[1]]@sha
	sessionText='TRUE'
	if (session) {
	  sessionText='sessionInfo()'
	}
	return(paste('## Commit ',current_commit,' \n```{r}\n',sessionText,'\n```\n\n<object type="application/pdf" data="file://changed.patch" data-attachment="patchfile.patch" ></object>',sep=''))
}
