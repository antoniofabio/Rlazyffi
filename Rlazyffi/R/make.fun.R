make.fun <-
function(symbol) {
  force(symbol)
  function(..., RETURN=voidType) {
    dictionary <- list(double = doubleType, integer = sint32Type,
                       character = stringType)
    args <- list(...)
    modes <- sapply(args, storage.mode)
    lengths <- sapply(args, length)
    isscalar <- lengths == 1
    ffiTypes <- vector(length(modes), mode="list")
    if(length(ffiTypes) > 0) {
      inDict <- modes %in% names(dictionary)
      ffiTypes[inDict & isscalar] <- dictionary[modes[inDict & isscalar]]
      for(i in which(inDict & !isscalar)) {
        ffiTypes[[i]] <- arrayType(dictionary[[ modes[[i]] ]], lengths[i])
      }
      ffiTypes[!(inDict & isscalar)] <- pointerType
    }
    cif <- prepCIF(RETURN, ffiTypes)
    ans <- callCIF(cif, symbol, ...)
    if(identical(RETURN, voidType)) {
      return(invisible(NULL))
    } else {
      if(is.list(ans) && ("value" %in% names(ans))) {
        return(ans$value)
      } else {
        return(ans)
      }
    }
  }
}
