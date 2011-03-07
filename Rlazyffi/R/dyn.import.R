dyn.import <-
function(symbs, envir=parent.frame()) {
  for(symbol in symbs) {
    assign(symbol, make.fun(symbol), envir=envir)
  }
}

