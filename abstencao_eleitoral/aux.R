
fmtP = function(x) {
  paste0(format(x * 100, digits=4), "%")
}

dnormC = function(x, y, main_title="Curva Normal") {  # Abrevia dnormalComp
  dnormalComp(mean(x), sd(x), mean(y), sd(y), main_title=main_title)
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
  t1 = x[x$NUM_TURNO == 1,]
  t2 = x[x$NUM_TURNO == 2,]
  if (nrow(t2) == 0 ) {
    return(list(t1))
  }
  #list(x[x$NUM_TURNO == 1,], x[x$NUM_TURNO == 2,])  
  list(t1, t2)
}

taxaA = function(x) {
  x$TAXA_ABSTENCAO
}
