rm(list = ls(all = TRUE))
source("get_filenames.R")
source("get_dv.R")


if (!exists("avisos")) {
  avisos = list()
}

plotak = function(x, title) {
  # hist(x, breaks = min(15, length(x)/20), main = title)
  
  plot(density(x), main=title)
}

teste_shapiro <- function(vetfoco, title) {
  print(paste("Teste de normalidade de", title))
  if (length(vetfoco) > 5000) {
    print(paste("Shapiro, primeiros 4998 de", length(vetFoco)))
    w =shapiro.test(vetfoco[1:4998])
  } else {
    print("Shapiro, todos")
    w = shapiro.test(vetfoco)
  }
}

desc_norm <- function(dados) {
  vetfoco = dados$TAXA_ABSTENCAO
  plotak(vetfoco, title=tit(dados))
  
  #W = teste_shapiro(vetfoco, tit(dados))
  #W$data.name = tit(dados)
}

filenames = get_filenames(anos=c(1998, 2002, 2006, 2014),
                          ufs=c("DF", "BR_DF"))

maxSize = length(filenames) * 2
grandezas = data.frame(
  eleitores = rep(NA, maxSize),
  ausentes  = rep(NA, maxSize),
  zonas = rep(NA, maxSize),
  secoes = rep(NA, maxSize),
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
      ausentes = sum(dd$QTD_ABSTENCOES),
      zonas = length(unique(dd$NUMERO_ZONA)),
      secoes = length(unique(dd$NUMERO_SECAO))
    )
    rownames(grandezas)[cg] = tit(dd)
    cg = cg+1
    desc_norm(dd)
    qqplot(dd)
  }

}

grandezas = grandezas[!is.na(grandezas), ]

library(knitr)
knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=" ")
})


print(kable(grandezas))


