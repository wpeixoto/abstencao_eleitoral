
# RStudio Version 1.1.442 
DATA_DIR = "/home/william/Downloads/Dados/TSE/mz"
# DATA_DIR = "/home/william/temp/dd/mz"

source("aux.R")
source("NormalCompara.R")
library(knitr)
library(plotrix)

anoturno = function(ano, turno) {
  ano_s = as.character(ano)
  paste0(ano_s, "_", turno)
}

dados = list()
for (ano in c(1998, 2002, 2006, 2010, 2014)) {
  for (turno in c("1", "2")) {
    
    filename = paste0(DATA_DIR, "/", anoturno(ano, turno), ".csv")
    
    if (!file.exists(filename)) {
      stop(paste("Arquivo", filename, "não existe"))
    }
    
    df = read.csv(filename, encoding = "ISO-8859", sep = ",", stringsAsFactors = F)

    dados[[anoturno(ano, turno)]] = list(ANO=ano, TURNO=turno, DF=df)
  }
}


zera_comparacao <- function() {
  comparacao = data.frame(
    ano = rep(0, 5),
    media = rep(0, 5),
    t = rep(0, 5),
    p_value = rep(0, 5),
    #intervalo = rep(0, 5)
    low = rep(NA, 5),
    upp = rep(NA, 5)
  )
}
comparacao = zera_comparacao()

cnt=1
for (i in c(1,3,5,7,9)) { # Há 10 data frames com dados de 5 anos
  df1 = dados[[i]]$DF   # Primeiro turno
  df2 = dados[[i+1]]$DF # Segundo turno
  diferenca_turnos = df1$TAXA_ABSTENCAO - df2$TAXA_ABSTENCAO
  T = t.test(diferenca_turnos)
  comparacao[cnt, ] = list(
    dados[[i]]$ANO,
    mean(diferenca_turnos),
    T$statistic,
    T$p.value,
    # paste0("[", as.character(T$conf.int[1]), ", ", as.character(T$conf.int[2]), "]") 
    T$conf.int[1],
    T$conf.int[2]
  )
  cnt = cnt + 1
  
  m1 = mean(df1$TAXA_ABSTENCAO)
  sd1 = ErrP(df1$TAXA_ABSTENCAO)
  
  m2 = mean(df2$TAXA_ABSTENCAO)
  sd2 = ErrP(df2$TAXA_ABSTENCAO)

  # print(" ")
  dnormalComp(m1, sd1, m2, sd2, 
              main_title = paste(dados[[i]]$ANO),
              a1n="1º Turno",
              a2n="2º Turno",
              x_lab = "Diferença entre as médias")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calcular regiões críticas ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  z_calc = (abs(m1 - m2)) / (sd1/sqrt(length(df1$TAXA_ABSTENCAO)))
  print(
    paste(
      "z-calc", z_calc, "p=", format(proba_z_1_2(z_calc, 4)) #, scientific = TRUE)
    )
  )
}

kable(comparacao, format="pandoc", caption="Comparações entre turnos")

# TODO: Mostrar esta tabela lá em baixo
# kable(comparacao2, format="pandoc", caption="Comparações entre anos distintos")


# plot(comparacao$media)
# xx = 1:5
# plotCI(xx, comparacao$media, ui=comparacao$upp, li=comparacao$low)
plotCI(c(1998, 2002, 2006, 2010, 2014), 
       main="Médias das diferenças entre abstenções\ndo primeiro para o segundo turno",
       comparacao$media, 
       ui=comparacao$upp, 
       li=comparacao$low, 
       ylab="Médias das diferenças", 
       xlab="anos", 
       xlim=c(1997, 2015)
       )
abline(0,0)

# plotCI(c(1998, 2002, 2006, 2010, 2014), 
#        main="Médias das diferenças entre abstenções\ndo primeiro para o segundo turno",
#        comparacao$media, 
#        ui=comparacao$upp, 
#        li=comparacao$low, 
#        ylab="Médias das diferenças", 
#        xlab="anos", 
#        xlim=c(1997, 2015)
#        )
# abline(0,0)


# source("NormalPadrao.R")


# dnormalComp()

comparacao2 = zera_comparacao()
cnt=1
for (i in c(2,4,6, 8)) { # Não compara 2014 com eleição ainda não realizada
  df1 = dados[[i]]$DF   # Segundo turno de um ano
  df2 = dados[[i+1]]$DF # Primeiro turno do ano subsequente
  # diferenca_turnos = df1$TAXA_ABSTENCAO - df2$TAXA_ABSTENCAO
  
  T = t.test(df1$TAXA_ABSTENCAO, df2$TAXA_ABSTENCAO, paired = FALSE, var.equal = FALSE)
  ano1 <-     dados[[i]]$ANO
  ano2 <-     dados[[i+1]]$ANO
  
  # print(paste(i, ano1, ano2))
  
  comparacao2[cnt, ] = list(
    paste(ano1, "x", ano2),
    mean(diferenca_turnos),
    T$statistic,
    T$p.value,
    # paste0("[", as.character(T$conf.int[1]), ", ", as.character(T$conf.int[2]), "]") 
    T$conf.int[1],
    T$conf.int[2]
  )
  cnt = cnt + 1
  
  m1 = mean(df1$TAXA_ABSTENCAO)
  sd1 = ErrP(df1$TAXA_ABSTENCAO)
  
  m2 = mean(df2$TAXA_ABSTENCAO)
  sd2 = ErrP(df2$TAXA_ABSTENCAO)

  # print(" ")
  dnormalComp(m1, sd1, m2, sd2, 
              main_title = paste(ano1, "x", ano2),
              a1n=paste(ano1, "2º T"),
              a2n=paste(ano2, "1º T"),
              x_lab="Taxas de abstenção")
}

kable(comparacao2, format="pandoc", caption="Comparações entre anos distintos")

