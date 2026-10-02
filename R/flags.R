##  R package rjags file R/flags.R
##  Copyright (C) 2006-2026 Martyn Plummer and Matt Denwood
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License version
##  2 as published by the Free Software Foundation.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/
##

pkg.compile.flags <- function(type = c("BINCOMPAT", "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="BINCOMPAT")
{
  type <- match.arg(type)
  flags <- list(
    "CPPFLAGS" = .Call("get_rjags_cppflags", PACKAGE="rjags"),
    "CXXFLAGS" = .Call("get_rjags_cxxflags", PACKAGE="rjags"),
    "LIBS" = .Call("get_rjags_libs", PACKAGE="rjags")    
  )
  value <- c(flags, "BINCOMPAT"=list(c(list("MAJOR_VERSION"=as.numeric(jags.version()[,1])), flags)))[[type]]
  
  if(isTRUE(print)){
    cat(value)
    invisible(value)
  }else{
    return(value)
  }
}

load.pkg.module <- function(name, bin.compat, quiet=FALSE)
{
  ## Extract the relevant libs folder:
  path <- system.file(package = name, "libs")
  stopifnot(length(path)==1L)
  if (path=="") {
    stop("Package not found: ", name)
  }
  if (.Platform$r_arch!="") {
    path <- file.path(path, .Platform$r_arch)
  }
  if (!file.exists(file.path(path, paste0(name, .Platform$dynlib.ext)))) {
    stop("File not found: ", file.path(path, paste0(name, .Platform$dynlib.ext)))
  }
  
  ## Check binary compatibility:
  if (missing(bin.compat) || !is.list(bin.compat) ||
        !"MAJOR_VERSION"%in%names(bin.compat)) {
    stop("The bin.compat argument must be provided: this should be
      saved within the package namespace using a call to
      rjags:::pkg.compile.flags(\"BINCOMPAT\") at build time")
  }
  rjags.bin <- pkg.compile.flags("BINCOMPAT")
  if (bin.compat[["MAJOR_VERSION"]] != rjags.bin[["MAJOR_VERSION"]]) {
    stop("Mismatched JAGS major versions:\n\t", name, ": ", bin.compat[["MAJOR_VERSION"]], "\n\trjags: ", rjags.bin[["MAJOR_VERSION"]], "\nThe ", name, " package must either be recompiled or reinstalled following recompilation on CRAN")
  }
  
  ## Try to load the module:
  tryCatch({
    load.module(name, path, quiet)
  }, error = function(x){
    stop("The ", name, " module failed to load for an unknown reason.\nIt may help to inspect the following PKG_LIB for mismatches:\n\t", name, ": ", bin.compat[["LIBS"]], "\n\trjags: ", rjags.bin[["LIBS"]], "\nEnsure that ", name, " and rjags are compiled against the same build of JAGS.", call.=FALSE)
  })
    
  invisible()  
}
