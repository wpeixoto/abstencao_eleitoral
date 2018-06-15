
fmtP = function(x) {
  paste0(format(x * 100, digits=4), "%")
}

identix = function(label, particular, seqq) {
  # Compõe um identificador para quadros de avisos
  paste0(label, particular, as.character(seqq))
}

msg_err_file = function(filename, label, particular, title="Erro no arquivo") {
  # Formata mensagem de erro em um arquivo
  paste(title, filename, label, particular)
}

dnormC = function(x, y,                      # Abrevia dnormalComp Naive
                  main_title="Curva Normal", 
                  subtitle="Naïve",
                  a1n = "",
                  a2n = "") {  
  dnormalComp(mean(x), sd(x), 
              mean(y), sd(y), 
              main_title=main_title, 
              subtitle = subtitle,
              a1n = a1n,
              a2n = a2n)
}

ErrP = function(x, sigma=sd(x)) {sigma/sqrt(length(x))}

dnormCC = function(x, y, 
                   main_title="Curva Normal (EP)", 
                   subtitle="Com Erro Padrão",
                   a1n = "",
                   a2n = "") {  # Abrevia dnormalComp com Erro Padrão
  dnormalComp(media1 = mean(x), dp1=ErrP(x), 
              media2=mean(y), dp2 = ErrP(y), 
              main_title=main_title, subtitle = subtitle,
              a1n = a1n, a2n = a2n)
}

tit = function(x) {
  uni = function(vv, title="objeto") {
    ob = unique(vv)
    if (length(ob) > 1) {
      ob = paste(ob, collapse = ", ")
      warning(paste("Mais de um ", title, " nesse DataFrame:", ob))
      return(ob)
    } else {
      #return(ob[1])
      return(ob)
    }
  }
  
  uf = uni(x$SIGLA_UF, "UF")
  ano = uni(x$ANO_ELEICAO, "ANO")
  turno = uni(x$NUM_TURNO, "Turno")
  return(paste0(uf, ": ", ano, "(", turno, ")"))
}

comparaTaxaA = function(x, y, 
                        turno=1, 
                        main_title=paste("Compara", tit(x), " e ", tit(y), "<Naïve>")) {
  # x_t = split(x, x$NUM_TURNO)
  # y_t = split(y, y$NUM_TURNO)
  x_t = spliTurno(x)
  y_t = spliTurno(y)
  
  if (length(x_t) == 1) {
    warning(paste("Não há segundo turno pada", tit(x)))
    return()
  }
  
  if (length(y_t) == 1) {
    warning(paste("Não há segundo turno pada", tit(y)))
    return()
  }
  
  #return(
    dnormC(x_t[[turno]]$TAXA_ABSTENCAO, y_t[[turno]]$TAXA_ABSTENCAO, main_title = main_title)
  #)
}

comparaTaxaA_EP = function(x, y, 
                           turno=1, 
                           main_title=paste("Compara", tit(x), " e ", tit(y), "<EP>")) {
  x_t = split(x, x$NUM_TURNO)
  y_t = split(y, y$NUM_TURNO)
  return(
    dnormCC(x_t[[turno]]$TAXA_ABSTENCAO, y_t[[turno]]$TAXA_ABSTENCAO, main_title = main_title)
    )
}


comparaTurnos = function(x) {
  x_t = split(x, x$NUM_TURNO, drop = T)
  if (length(x_t) == 1) {
    return(function(x) {"Não houve segundo turno"})
  }
  return(dnormC(x_1[[1]]), x_t[[2]])
}

comparaTurnosEP = function(x) {
  x_t = split(x, x$NUM_TURNO, drop = T)
  if (length(x_t) == 1) {
    return(function(x) {"Não houve segundo turno"})
  }
  return(dnormCC(x_1[[1]]), x_t[[2]])
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
