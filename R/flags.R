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

  ## For compatibility with JAGS 4->5 transition on macOS:
  if (.Platform$OS.type=="unix" &&
      as.numeric(flags$JAGS_VERSION[,1])==4 &&
      flags[["CPPFLAGS"]] == "-I/usr/local/include/JAGS" &&
      !dir.exists("/usr/local/include/JAGS") &&
      dir.exists("/opt/jags/versions/jags/current-4/include/JAGS")
  ) {
    flags[["JAGS_ROOT"]] <- "-I/opt/jags/versions/jags/current-4"
    flags[["CPPFLAGS"]] <- "-I/opt/jags/versions/jags/current-4/include/JAGS"
    flags[["LIBS"]] <- "-L/opt/jags/versions/jags/current-4/lib -ljags"
  }

  type <- match.arg(type)
  value <- c(flags, "ALL"=list(flags))[[type]]

  if (isTRUE(print)){
    cat(value)
    invisible(value)
  } else {
    return(value)
  }
}

check.bin.compat <- function(bin.rjags, bin.compat, pkgname, stop.on.error=TRUE){

  if (!is.list(bin.compat) || !"JAGS_VERSION"%in%names(bin.compat) || !"JAGS_ROOT"%in%names(bin.compat)) {
    stop("The bin.compat argument must be provided: this should be\nsaved within the package namespace using a call to\nrjags:::pkg.compile.flags(\"ALL\") at build time", call.=FALSE)
  }
  stopifnot(is.list(bin.rjags), "JAGS_VERSION"%in%names(bin.rjags), "JAGS_ROOT"%in%names(bin.rjags))
  stopifnot(is.character(pkgname), length(pkgname)==1L, !is.na(pkgname))

  ## There is scope for additional checks here (e.g. based on parsing PKG_LIBS),
  ## but for now just check the major version of JAGS and (on unix) JAGS_ROOT

  msg <- NULL

  maj.vers.pkg <- as.numeric(bin.compat[["JAGS_VERSION"]][,1])
  maj.vers.runtime <- as.numeric(bin.rjags[["JAGS_VERSION"]][,1])
  stopifnot(!is.na(maj.vers.pkg), length(maj.vers.pkg)==1L, !is.na(maj.vers.pkg), length(maj.vers.runtime)==1L)
  if (maj.vers.pkg != maj.vers.runtime) {
    msg <- c(msg, paste0("Mismatched JAGS major versions:\n\t", pkgname, ": ", maj.vers.pkg, "\n\trjags: ", maj.vers.runtime))
  }

  if (.Platform$OS.type=="unix") {
    jags.root.pkg <- bin.compat[["JAGS_ROOT"]]
    jags.root.rjags <- bin.rjags[["JAGS_ROOT"]]
    stopifnot(is.character(jags.root.pkg), !is.na(jags.root.pkg), length(jags.root.pkg)==1L, is.character(jags.root.rjags), !is.na(jags.root.rjags), length(jags.root.rjags)==1L)
    if (jags.root.pkg != jags.root.rjags) {
      msg <- c(msg, paste0("Mismatched JAGS_ROOT variables:\n\t", pkgname, ": ", jags.root.pkg, "\n\trjags: ", jags.root.rjags))
    }
  }

  if (length(msg)>0L && isTRUE(stop.on.error)) {
    stop("rjags/", pkgname, " package incompatibility detected:\n\n", paste0(msg, collapse="\n\n"), "\n\nThe ", pkgname, " package must either be recompiled or reinstalled\nfollowing recompilation on CRAN.", call.=FALSE)
  }
  return(msg)
}

load.pkg.module <- function(pkgname, libname, bin.compat, quiet=FALSE)
{
  ## Check arguments:
  stopifnot(is.character(pkgname), length(pkgname)==1L, !is.na(pkgname))
  stopifnot(is.logical(quiet), length(quiet)==1L, !is.na(quiet))

  ## Extract the relevant libs folder:
  if (missing(libname)){
    path <- system.file(package = pkgname, "libs")
    stopifnot(length(path)==1L)
    if (path=="") {
      stop("Package ", pkgname, " not found", call.=FALSE)
    }
  } else {
    stopifnot(is.character(libname), length(libname)==1L, !is.na(libname))
    path <- file.path(libname, pkgname, "libs")
    stopifnot(length(path)==1L)
    if (!file.exists(path)) {
      stop("Package ", pkgname, " not found in library ", libname, call.=FALSE)
    }
  }
  if (.Platform$r_arch!="") {
    path <- file.path(path, .Platform$r_arch)
  }
  if (!file.exists(file.path(path, paste0(pkgname, .Platform$dynlib.ext)))) {
    stop("File not found: ", file.path(path, paste0(pkgname, .Platform$dynlib.ext)), call.=FALSE)
  }

  ## Check binary compatibility:
  bin.rjags <- pkg.compile.flags("ALL")
  check.bin.compat(bin.rjags, bin.compat, pkgname)

  ## Try to load the module:
  tryCatch ({
    load.module(pkgname, path, quiet)
  }, error = function(e){
    stop("The ", pkgname, " module failed to load for an unknown reason.\nIt may help to inspect the following PKG_LIB for mismatches:\n\t", pkgname, ": ", bin.compat[["LIBS"]], "\n\trjags: ", bin.rjags[["LIBS"]], "\nEnsure that ", pkgname, " and rjags are compiled against the same build of JAGS.\n\nThe error returned from load.module() was:\n", conditionMessage(e), call.=FALSE)
  })

  invisible()
}
