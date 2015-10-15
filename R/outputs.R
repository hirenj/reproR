parseRemote <- function(url,sha) {
	url_components = strsplit(url,'/')
	lapply(url_components,function(components) {
		if ("github.com" %in% components) {
			return(paste("https://github.com/",components[4],"/",gsub('\\.git','',components[5]),"/commit/",sha,sep=""))
		}
		if (length(grep("@github.com",components)) > 0) {
			return(paste("https://github.com/",gsub(".*:","",components[1]),"/",gsub('\\.git','',components[2]), "/commit/",sha,sep="" ))
		}
		if ("gist.github.com" %in% components) {
			return(paste("https://gist.github.com/",gsub('\\.git','',components[4]),"/",sha,sep=""))
		}
		if (length(grep("@gist.github.com",components)) > 0) {
			return(paste("https://gist.github.com/",gsub( '\\.git','',gsub(".*:","",components[1])),"/",sha,sep="" ))
		}
	})
}

#' Generate markdown
#' @export
status.md <- function(session=F) {
  if ( ! is_repo() ) {
    return('## Not tracked in a repository')
  }
  current_commit = git2r::commits(repo())[[1]]@sha
	sessionText='TRUE'
	if (session) {
	  sessionText='sessionInfo()'
	}
	remote_url = parseRemote(git2r::remote_url(repo()),current_commit)
	patch_file=''
	if (file.exists('changed.patch')) {
		patch_file = '<object type="application/pdf" data="file://changed.patch" data-attachment="patchfile.patch" ></object>'
	}
	return(paste('## Commit ',current_commit,'\n [',current_commit,'](',remote_url,')\n\n```{r}\n',sessionText,'\n```\n\n',patch_file,sep=''))
}
