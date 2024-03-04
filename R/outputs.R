parseRemote <- function(url,sha) {
  components = unlist(strsplit(url,'/'))
  if (any(grep("@",components))) {

    host = gsub( ':.*', '', gsub('.*@','',components[1]))
    username = gsub(".*:","",components[1])
    repo = gsub('\\.git','',components[2])

    if (host == "gist.github.com") {
      return(paste("https://gist.github.com/",gsub( '\\.git','',gsub(".*:","",components[1])),"/",sha,sep="" ))
    }

    return(paste("https://",host,"/",username,"/",repo, "/commit/",sha,sep="" ))
  }
  if (components[1] == 'https:') {
    if ("gist.github.com" %in% components) {
      return(paste("https://gist.github.com/",gsub('\\.git','',components[4]),"/",sha,sep=""))
    }
    return(paste(components[1],"//",components[3],"/", components[4],"/",gsub('\\.git','',components[5]),"/commit/",sha,sep=""))
  }

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

load_cache_file  = function(filename) {
  envir=readRDS(filename)
  with(attributes(envir), {
    hash=stringr::str_match(path,'_([0-9a-f]{7})_')[,2]
    sapply(file.path(path,hashes),function(x) {
        if (stringr::str_detect(x,'cache_common')) {
            x = stringr::str_replace(x,'^.*_([0-9a-f]{7})_(.*)/cache_common','cache_common_\\1_\\2')
        }

        if (file.exists(paste(x,'.rdx',sep=''))) {
            lazyLoad(x,envir = envir)
        } else if (file.exists(paste(x,'.RData',sep='')))  {
            load(paste(x,'.RData',sep=''),envir=envir)
        }
    })
  })
  envir
}

hydrate_cache_file  = function(filename,threshold=1024*1024*75,keep=NA) {
  loaded_data=load_cache_file(filename)
  message("Finding large objects")
  large_objects = names(which(sapply(ls(loaded_data),function(obj) { object.size(get(obj,loaded_data)) > threshold; },USE.NAMES = T)))
  message("Removing large objects")
  large_objects = large_objects[! large_objects %in% keep ]
  rm(list=large_objects,envir=loaded_data)
  obj_sizes = sapply(ls(loaded_data),function(obj) {
    object.size(get(obj,loaded_data))
  },USE.NAMES = T)

  df = data.frame(obj=names(obj_sizes),
               size_raw=obj_sizes,
               size=utils:::format.object_size(obj_sizes,units="MiB"))
  sizes = dplyr::arrange(df,size_raw)

  message("Output environment contains in total ",utils:::format.object_size(sum(sizes$size_raw),units="MiB"))
  saveRDS(loaded_data,paste(tools::file_path_sans_ext(filename),'.hydrated.Rds',sep='',collapse=''))
}

#' Expand loaded object
#' @export
hydrate_cached_note_run = function(filename=NULL)
{
  loaded_data = load_cache_file(filename)
  message("Finding large objects")
  large_objects = names(which(sapply(ls(loaded_data),function(obj) { object.size(get(obj,loaded_data)) > 1024*1024*75; },USE.NAMES = T)))
  message("Removing large objects")
  rm(list=large_objects,envir=loaded_data)
  obj_sizes = sapply(ls(loaded_data),function(obj) {
    object.size(get(obj,loaded_data))
  },USE.NAMES = T)

  df = data.frame(obj=names(obj_sizes),
               size_raw=obj_sizes,
               size=utils:::format.object_size(obj_sizes,units="MiB"))
  sizes = dplyr::arrange(df,size_raw)

  message("Output environment contains in total ",utils:::format.object_size(sum(sizes$size_raw),units="MiB"))
  saveRDS(loaded_data,paste(tools::file_path_sans_ext(filename),'.hydrated.Rds',sep='',collapse=''))
}

#' Reload a fully cached Note run
#' @export
load_cached_note_run = function (filename = "analysis.Rmd",thin=FALSE)
{
  Rgator:::getDataEnvironment()
  parent_path = getOption('repro.cache.path',default=Sys.getenv('REPROR_CACHE_PATH'))
  orig_cache_path = paste(xfun::sans_ext(filename),'_cache','/',sep='')
  if (parent_path != "") {
    message("Parent cache path set, skipping copying cache files")
    orig_cache_path = file.path(parent_path,orig_cache_path)
  }
  orig_wd=getwd()

  temp_cache_path = tempfile()
  dir.create(temp_cache_path)

  # There's no point in copying the cache files over
  # when there is a global cache path
  # because knitr embeds the cache path into
  # the objects it saves, and it doesn't look
  # easy to remove that dependency
  
  if (parent_path == "") {
    fs::dir_copy(orig_cache_path, temp_cache_path)
    cache_common_path='cache_common'
    if (parent_path != "") {
      cache_common_path = file.path(parent_path,cache_common_path)
    }
    if (dir.exists(cache_common_path)) {
      fs::dir_copy(cache_common_path, temp_cache_path)
    }
  }

  for (file in list.files(pattern = "\\.Rmd$")) {
    fs::file_copy(file,temp_cache_path)
  }
  setwd(temp_cache_path)

  hashes = list()

  knitr::opts_knit$set(verbose=T);

  knitr::opts_hooks$set(cache = function(options) {
    if (options$cache == FALSE) {
      options$eval = FALSE
    }
    options
  })

  loaded_data = new.env()

  output_text = withCallingHandlers( {
    loaded_data = note_testrun(filename=filename)
  }, message=function(x) { hashes <<- c(hashes,x); message(x); invokeRestart("muffleMessage") });
  setwd(orig_wd)
  message("Finished collecting chunk hashes")
  unique_hashes = unique(hashes)
  unique_hashes = stringr::str_replace(stringr::str_replace(string=unique_hashes[which(grepl("^ *loading",unique_hashes))],pattern = '.* from ',''), '\n','')
  unique_hashes = stringr::str_replace(unique_hashes,orig_cache_path,"")


  message("Setting hashes")

  attributes(loaded_data)$hashes = unique_hashes

  if (thin)  {
    message("Finding large objects")
    large_objects = names(which(sapply(head(ls(loaded_data),3),function(obj) { message(obj); get(obj,loaded_data); },USE.NAMES=T))); #object.size(get(obj,loaded_data)) > 1024*1024*75; },USE.NAMES = T)))
    message("Removing large objects")
    rm(list=large_objects,envir=loaded_data)
    obj_sizes = sapply(ls(loaded_data),function(obj) {
      object.size(get(obj,loaded_data))
    },USE.NAMES = T)

    df = data.frame(obj=names(obj_sizes),
                 size_raw=obj_sizes,
                 size=utils:::format.object_size(obj_sizes,units="MiB"))
    sizes = dplyr::arrange(df,size_raw)

    message("Output environment contains in total ",utils:::format.object_size(sum(sizes$size_raw),units="MiB"))

  }

  return(loaded_data)
}

#' Perform a Note testrun
#' @export
note_testrun <- function(filename='analysis.Rmd',output=paste(xfun::sans_ext(filename),'_testrun.html',sep=''),reuse.cache=TRUE,params_file=NULL,params=list()) {
  if ( ! reuse.cache ) {
    parent_path = getOption('repro.cache.path',default=Sys.getenv('REPROR_CACHE_PATH'))
    cache_path=paste(xfun::sans_ext(filename),'_cache','/',sep='')
    if (parent_path != "") {
      cache_path = file.path(parent_path,cache_path)
    }
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

read_utf8 <- function(file) {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
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

  parent_path = getOption('repro.cache.path',default=Sys.getenv('REPROR_CACHE_PATH'))
  parent_cache=paste(xfun::sans_ext(filename),'_cache','/',sep='')

  if (parent_path != "") {
    parent_cache=file.path(parent_path, parent_cache)
  }


  dir.create(parent_cache,showWarnings=FALSE)

  knitr::opts_knit$set(root.dir = getwd())

  knitr::opts_chunk$set(cache=TRUE,cache.path=parent_cache)

  # Reset the cache path before running any chunks
  knitr::opts_chunk$set(reset.cache.path=TRUE)
  knitr::knit_hooks$set(reset.cache.path=function(before,options,envir) {
    if (before) {
      if ( knitr::opts_chunk$get('reset.cache.path') ) {
        message("\nSetting parent cache to ",parent_cache)
        knitr::opts_chunk$set(cache.path=parent_cache)
      }
      knitr::opts_chunk$set(reset.cache.path=FALSE)
    }
  })

  knitr::knit_hooks$set(dynamic.cache=function(before,options,envir) {
    if (before) {
      child_sans_ext = gsub( '/','_', basename(xfun::sans_ext(options$child.md)))
      child_path = file.path('cache_common', child_sans_ext)
      if (parent_path != "") {
        child_path = file.path(parent_path,child_path)
      }
      message("\nSetting child cache path to ",child_path)
      knitr::opts_chunk$set(cache.path=paste(child_path,'/',sep=''))
    } else {
      message("\nResetting cache path to ",parent_cache)
      knitr::opts_chunk$set(cache.path=parent_cache)
    }
  })
  loaded_data = new.env()
  default_params = knitr:::flatten_params(knitr:::knit_params(read_utf8(filename)))

  if ( ! is.null(params_file)) {
    default_params = merge_lists(default_params,
                                 knitr:::flatten_params(knitr:::knit_params(c('---',read_utf8(params_file),'---')))
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
  } else {
    params = default_params
  }
  class(params) <- c(class(params),'knit_param_list')

  torender <- tempfile(fileext = ".Rmd",tmpdir=getwd())

  temp_cache=paste(xfun::sans_ext(torender),'_cache','/',sep='')

  on.exit({ file.remove(torender); unlink(temp_cache,recursive=T) })

  cat(paste(c('```{r cache=F,include=F}\nTRUE\n```\n\n' ,read_utf8(filename),status.md(session=T)), collapse="\n"), file = torender)

  if (is.null(notebook) && is.null(output)) {
    output_text = rmarkdown::render(torender,envir=loaded_data,params=params,output_format=knoter::note_page())
  } else if (is.null(notebook) && ! is.null(output)) {
    output_text = rmarkdown::render(torender,output_file=output,envir=loaded_data,params=params,output_format=knoter::note_page())
  } else {
    output_text = rmarkdown::render(torender,output_file=paste('knitr.',pwd,'.html',sep=''),envir=loaded_data,params=params,output_format=knoter::note_page(notebook=notebook,section=pwd,sharepoint=sharepoint,batch.chunks=batch.chunks))
  }

  to_copy = list.files(temp_cache)

  file.copy(file.path(temp_cache,to_copy), parent_cache,recursive=TRUE)

  return(loaded_data)
}
