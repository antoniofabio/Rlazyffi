dyn.constant <-
function(symbs, envir=parent.frame(), type=pointerType) {
  for(symbol in symbs) {
    f <- function() {
      force(symbol)
      return(getNativeValue(getNativeSymbolInfo(symbol)$address, type))
    }
    assign(symbol, f, envir=envir)
  }
}

