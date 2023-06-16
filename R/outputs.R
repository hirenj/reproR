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
  current_commit = git2r::commits(repo())[[1]]$sha
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

#' Reload a fully cached Note run
#' @export
load_cached_note_run = function (filename = "analysis.Rmd") 
{
  Rgator:::getDataEnvironment()

  orig_cache_path = paste("cache_", gsub("\\..*", "", filename), "/", sep = "")
  orig_wd=getwd()

  temp_cache_path = tempfile()
  dir.create(temp_cache_path)
  fs::dir_copy(orig_cache_path, temp_cache_path)
  if (dir.exists('cache_common')) {
    fs::dir_copy('cache_common', temp_cache_path)    
  }
  for (file in list.files(pattern = "\\.Rmd$")) {
    fs::file_copy(file,temp_cache_path)    
  }
  setwd(temp_cache_path)

  hashes = list()
  knitr::opts_chunk$set(cache = TRUE, cache.path = orig_cache_path )
  knitr::opts_knit$set(verbose=T);
  knitr::knit_hooks$set(dynamic.cache=function(before,options,envir) {
    if (before) {
      knitr::opts_chunk$set(cache.path=paste('cache_common',gsub('\\..*','',options$child.md),'/',sep='/'))
    } else {
      knitr::opts_chunk$set(cache.path=orig_cache_path)
    }
  })

  loaded_data = new.env()

  assign('params',list(),loaded_data)

  output_text = withCallingHandlers( {
    knoter::knit(text = paste(c(readLines(filename), status.md(session = T)), collapse = "\n"), envir = loaded_data)
    }, message=function(x) { hashes <<- c(hashes,x); message(x); invokeRestart("muffleMessage") });
  setwd(orig_wd)
  
  unique_hashes = unique(hashes)
  unique_hashes = stringr::str_replace(stringr::str_replace(string=unique_hashes[which(grepl("^ *loading",unique_hashes))],pattern = '.* from ',''), '\n','')
  unique_hashes = stringr::str_replace(unique_hashes,orig_cache_path,"")
  
  attributes(loaded_data)$hashes = unique_hashes
  return(loaded_data)
}

#' Perform a Note testrun
#' @export
note_testrun <- function(filename='analysis.Rmd',output='testrun.html',reuse.cache=TRUE,params_file=NULL,params=list()) {
  if ( ! reuse.cache ) {
    cache_path=paste('cache_',gsub('\\..*','',filename),'/',sep='')
    if ( file.exists(cache_path) ) {
      unlink( cache_path, recursive = TRUE )
    }
  }
  note(filename,notebook=NULL,output=output,params_file=params_file,params=params)
}

merge_lists <- function(base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0)
    overlay_list
  else if (length(overlay_list) == 0)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive)
        merged_list[[name]] <- merge_lists(base, overlay)
      else {
        merged_list[[name]] <- NULL
        merged_list <- append(merged_list,
                              overlay_list[which(names(overlay_list) %in% name)])
      }
    }
    merged_list
  }
}

#' Perform a Note
#' @export
note <- function(filename='analysis.Rmd',notebook=getOption('knoter.default.notebook'),sharepoint=getOption('knoter.sharepoint'),output=NULL,batch.chunks=10,params_file=NULL,params=list()) {

  Rgator:::getDataEnvironment();

  pwd = rev(unlist(strsplit(getwd(),'/')))[1]
  if (grepl('^proj_',pwd)) {
    pwd = gsub('^proj_','',pwd)
  }
  if ( ! file.exists(filename) ) {
    stop(paste('No ',filename,sep=''))
  }

  parent_cache=paste('cache_',gsub('\\..*','',filename),'/',sep='')

  knitr::opts_chunk$set(cache=TRUE,cache.path=parent_cache)
  knitr::knit_hooks$set(dynamic.cache=function(before,options,envir) {
    if (before) {
      knitr::opts_chunk$set(cache.path=paste('cache_common',gsub('\\..*','',options$child.md),'/',sep='/'))
    } else {
      knitr::opts_chunk$set(cache.path=parent_cache)
    }
  })
  loaded_data = new.env()
  default_params = knitr:::flatten_params(knitr:::knit_params(readLines(file(filename))))

  if ( ! is.null(params_file)) {
    default_params = merge_lists(default_params,
                                 knitr:::flatten_params(knitr:::knit_params(c('---',readLines(file(params_file)),'---')))
                                 )
    assign('PARAMS_FILE',params_file,loaded_data)
  }

  if ( ! is.null(params) ) {
    # verify they are a list
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("render params argument must be a named list")
    }

    # verify that all parameters passed are also in the yaml
    invalid_params <- setdiff(names(params), names(default_params))
    if (length(invalid_params) > 0) {
      stop("render params not declared in YAML: ",
           paste(invalid_params, collapse = ", "))
    }
    params = merge_lists(default_params, params, recursive = FALSE)
  }
  class(params) <- c(class(params),'knit_param_list')
  assign('params',params,loaded_data)


  if (is.null(notebook) && is.null(output)) {
    output_text = rmarkdown::render(filename,envir=loaded_data,output_format=knoter::note_page())
#    output_text = knoter::knit(text=paste(c(readLines(filename),status.md(session=T)), collapse="\n"),envir=loaded_data)
    return(loaded_data)
  }
  if (is.null(notebook) && ! is.null(output)) {
    output_text = rmarkdown::render(filename,output_file=output,envir=loaded_data,output_format=knoter::note_page())
#    output_text = knoter::knit(text=paste(c(readLines(filename),status.md(session=T)), collapse="\n"),envir=loaded_data,output=output)
    return(loaded_data)
  }
  rmarkdown::render(filename,output_file=paste('knitr.',pwd,'.html',sep=''),envir=loaded_data,output_format=knoter::note_page(notebook=notebook,section=pwd,sharepoint=sharepoint,batch.chunks=batch.chunks))
}
