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
	generatePatch('changed.patch')
	patch_file=''
	if (file.exists('changed.patch')) {
		patch_file = '<object type="application/pdf" data="file://changed.patch" data-attachment="patchfile.patch" ></object>'
	}
	return(paste('## Commit ',current_commit,'\n [',current_commit,'](',remote_url,')\n\n```{r}\n',sessionText,'\n```\n\n',patch_file,sep=''))
}

#' Perform a Note
#' @export
note <- function(filename='analysis.Rmd',notebook=getOption('knoter.default.notebook'),sharepoint=getOption('knoter.sharepoint'),output=NULL) {
  pwd = rev(unlist(strsplit(getwd(),'/')))[1]
  if (grepl('^proj_',pwd)) {
    pwd = gsub('^proj_','',pwd)
  }
  if ( ! file.exists(filename) ) {
    stop(paste('No ',filename,sep=''))
  }
  knitr::opts_chunk$set(cache=TRUE,cache.path=paste('cache_',gsub('\\..*','',filename),'/',sep=''))
  if (is.null(notebook) && is.null(output)) {
    loaded_data = new.env()
    output_text = knoter::knit(text=paste(c(readLines(filename),status.md(session=T)), collapse="\n"),envir=loaded_data)
    return(loaded_data)
  }
  if (is.null(notebook) && ! is.null(output)) {
    loaded_data = new.env()
    output_text = knoter::knit(text=paste(c(readLines(filename),status.md(session=T)), collapse="\n"),envir=loaded_data,output=output)
    return(loaded_data)
  }
  knoter::knote(text=paste(c(readLines(filename),status.md(session=T)), collapse="\n"),output=paste('knitr.',pwd,'.html',sep=''),notebook = notebook,section=pwd,sharepoint=sharepoint,auto.archive = T)
}
