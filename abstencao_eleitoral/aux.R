
fmtP = function(x) {
  paste0(format(x * 100, digits=4), "%")
}

dnormC = function(x, y) {  # Abrevia dnormalComp
  dnormalComp(mean(x), sd(x), mean(y), sd(y))
}

comparaTaxaA = function(x, y, turno=1) {
  x_t = split(x, x$NUM_TURNO)
  y_t = split(y, y$NUM_TURNO)
  return(dnormC(x_t[[turno]]$TAXA_ABSTENCAO, y_t[[turno]]$TAXA_ABSTENCAO))
}

comparaTurnos = function(x) {
  x_t = split(x, x$NUM_TURNO, drop = T)
  if (length(x_t) == 1) {
    return(function(x) {"Não houve segundo turno"})
  }
  return(dnormC(x_1[[1]]), x_t[[2]])
  
}

spliTurno = function(x) {
  list(x[x$NUM_TURNO == 1,], x[x$NUM_TURNO == 2,])  
}

taxaA = function(x) {
  x$TAXA_ABSTENCAO
}
