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

pkg.compile.flags <- function(type = c("ALL", "JAGS_ROOT", "JAGS_VERSION", "RJAGS_VERSION", "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="ALL")
{

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

load.pkg.module <- function(pkgname, libname, bin.compat, quiet=FALSE)
{
  ## Extract the relevant libs folder:
  if(missing(libname)){
    path <- system.file(package = pkgname, "libs")
    stopifnot(length(path)==1L)
    if (path=="") {
      stop("Package ", pkgname, " not found")
    }
  }else{
    path <- file.path(libname, pkgname, "libs")
    stopifnot(length(path)==1L)
    if (!file.exists(path)) {
      stop("Package ", pkgname, " not found in library ", libname)
    }
  }
  if (.Platform$r_arch!="") {
    path <- file.path(path, .Platform$r_arch)
  }
  if (!file.exists(file.path(path, paste0(pkgname, .Platform$dynlib.ext)))) {
    stop("File not found: ", file.path(path, paste0(pkgname, .Platform$dynlib.ext)))
  }

  ## Check binary compatibility:
  if (missing(bin.compat) || !is.list(bin.compat) ||
        !"JAGS_VERSION"%in%names(bin.compat)) {
    stop("The bin.compat argument must be provided: this should be
      saved within the package namespace using a call to
      rjags:::pkg.compile.flags(\"ALL\") at build time")
  }
  rjags.bin <- pkg.compile.flags("ALL")

  ## There is scope for additional checks here (e.g. based on parsing PKG_LIBS),
  ## but for now just check the major version of JAGS:
  maj.vers.compiled <- as.numeric(bin.compat[["JAGS_VERSION"]][,1])
  maj.vers.runtime <- as.numeric(rjags.bin[["JAGS_VERSION"]][,1])
  if (maj.vers.compiled != maj.vers.runtime) {
    stop("Mismatched JAGS major versions:\n\t", pkgname, ": ", maj.vers.compiled, "\n\trjags: ", maj.vers.runtime, "\nThe ", pkgname, " package must either be recompiled or reinstalled following recompilation on CRAN")
  }

  ## Try to load the module:
  tryCatch({
    load.module(pkgname, path, quiet)
  }, error = function(x){
    stop("The ", pkgname, " module failed to load for an unknown reason.\nIt may help to inspect the following PKG_LIB for mismatches:\n\t", pkgname, ": ", bin.compat[["LIBS"]], "\n\trjags: ", rjags.bin[["LIBS"]], "\nEnsure that ", pkgname, " and rjags are compiled against the same build of JAGS.", call.=FALSE)
  })

  invisible()
}
