# Dealing with  Non-standard evaluation from "."
.onLoad <- function(libname = find.package("promoutils"),
                    pkgname = "promoutils"){
  if(getRversion() >= "2.15.1")
    utils::globalVariables(".")
  invisible()
}
