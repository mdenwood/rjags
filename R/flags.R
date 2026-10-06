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

pkg.compile.flags <- function(type = c("ALL", "MAJOR_VERSION", "JAGS_ROOT", "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="ALL")
{

  ## TODO: we need an rjags environmental variable for the current path on Windows, and a function that substitutes JAGS_ROOT 
  ## in the below versions (on Windows only) and returns the runtime JAGS_ROOT (on Windows and unix - get prefix from jags.pc
  ## in autoconfigure
  
  flags <- get.compile.flags()
  type <- match.arg(type)
  value <- c(flags, "ALL"=list(flags))[[type]]
  
  if(isTRUE(print)){
    cat(value)
    invisible(value)
  }else{
    return(value)
  }
}

load.pkg.module <- function(pkg, lib, bin.compat, quiet=FALSE)
{
  ## Extract the relevant libs folder:
  if(missing(lib)){
    path <- system.file(package = pkg, "libs")      
  }else{
    path <- file.path(lib, pkg, "libs")
  }
  stopifnot(length(path)==1L)
  if (path=="") {
    stop("Package not found: ", pkg)
  }
  if (.Platform$r_arch!="") {
    path <- file.path(path, .Platform$r_arch)
  }
  if (!file.exists(file.path(path, paste0(pkg, .Platform$dynlib.ext)))) {
    stop("File not found: ", file.path(path, paste0(pkg, .Platform$dynlib.ext)))
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
    stop("Mismatched JAGS major versions:\n\t", pkg, ": ", bin.compat[["MAJOR_VERSION"]], "\n\trjags: ", rjags.bin[["MAJOR_VERSION"]], "\nThe ", pkg, " package must either be recompiled or reinstalled following recompilation on CRAN")
  }
  
  ## Try to load the module:
  tryCatch({
    load.module(pkg, path, quiet)
  }, error = function(x){
    stop("The ", pkg, " module failed to load for an unknown reason.\nIt may help to inspect the following PKG_LIB for mismatches:\n\t", pkg, ": ", bin.compat[["LIBS"]], "\n\trjags: ", rjags.bin[["LIBS"]], "\nEnsure that ", pkg, " and rjags are compiled against the same build of JAGS.", call.=FALSE)
  })
    
  invisible()  
}
