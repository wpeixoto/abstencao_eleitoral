source("get_filenames.R")
source("get_dv.R")

plotak = function(x, title) {
  # hist(x, breaks = min(15, length(x)/20), main = title)
  
  plot(density(x), main=title)
}

desc_norm <- function(dados) {
  vetfoco = dados$TAXA_ABSTENCAO
  plotak(vetfoco, title=tit(dados))
  
  print(paste("Teste de normalidade de", tit(dados)))
  if (length(vetfoco) > 5000) {
    print(paste("Shapiro, primeiros 4998 de", nrow(dados)))
    print(shapiro.test(vetfoco[1:4998]))
  } else {
    print("Shapiro, todos")
    print(shapiro.test(vetfoco)    )
  }
}

filenames = get_filenames(anos=c(1998, 2002, 2006, 2014),
                          ufs=c("DF"))

grandezas = data.frame(
  eleitores = rep(NA, length(filenames) * 2),
  zonas = rep(NA, length(filenames) * 2),
  secoes = rep(NA, length(filenames) * 2),
  stringsAsFactors = FALSE
)

cg = 1
for (fn in filenames) {
  #tryCatch({
    dados = get_dv(fn, full=T)
  #  }
  #  ,     warning=function(){}
  #)
  
  if (unique(dados$ANO_ELEICAO) == 1998) {
    dados$SIGLA_UF = "DF"  # Corrige erro em 1998
  }

  
  for (dd in spliTurno(dados))  {
    grandezas[cg, ] = list(
      eleitores = sum(dd$QTD_APTOS),
      zonas = length(unique(dd$NUMERO_ZONA)),
      secoes = length(unique(dd$NUMERO_SECAO))
    )
    rownames(grandezas)[cg] = tit(dd)
    cg = cg+1
    desc_norm(dd)
  }

}

knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=" ")
})

library(knitr)
print(kable(grandezas))


