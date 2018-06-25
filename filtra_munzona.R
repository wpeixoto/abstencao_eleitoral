DATA_DIR = "/home/william/Downloads/Dados/TSE"
NOMES_CAMPOS = list()
NOMES_CAMPOS$ate12 = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", 
                       "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", 
                       "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", 
                       "CODIGO_CARGO", "DESCRICAO_CARGO", "QTD_APTOS", 
                       "QTD_SECOES", "QTD_SECOES_AGREGADAS", "QTD_APTOS_TOT", "QTD_SECOES_TOT", 
                       "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", 
                       "QTD_VOTOS_NOMINAIS", "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "QTD_VOTOS_LEGENDA", "QTD_VOTOS_ANULADOS_APU_SEP", 
                       "DATA_ULT_TOTALIZACAO", "HORA_ULT_TOTALIZACAO")
NOMES_CAMPOS$e2014 = c(NOMES_CAMPOS$ate12, "TRANSITO")
# NOMES_CAMPOS$e2016 = c(NOMES_CAMPOS$e2014, "QTD_VOTOS_ANULADOS")

library(dplyr)
load_df = function(filename, ano) {
  df = read.csv(filename, header = F, encoding = "ISO-8859", sep = ";", stringsAsFactors = F)
  if (ano == 2014) {
    names(df) = NOMES_CAMPOS$e2014
  } else {
    names(df) = NOMES_CAMPOS$ate12
  }
  df = df[df$CODIGO_CARGO %in% c(1, 3), ]  # Apenas governador
  df = df[df$SIGLA_UF == "DF", ]
  df = df[, c("NUM_TURNO", "NUMERO_ZONA", "CODIGO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS")]
  df$TAXA_ABSTENCAO = df$QTD_ABSTENCOES / df$QTD_APTOS
  turnos = spliTurno(df)
  # dados[[paste0("a", ano_s, "_1")]] = list(ANO=ano, DF=turnos[[1]])
  # if (length(turnos) == 2) {
  #   dados[[paste0("a", ano_s, "_2")]] = turnos[[2]]
  # }
  arrange(df, NUM_TURNO, NUMERO_ZONA)
  return(df)
}
source("aux.R")
# dados = list()
for (ano in c(1998, 2002, 2010, 2014)) {
  ano_s = as.character(ano)
  filename = paste0(DATA_DIR, "/detalhe_votacao_munzona_", ano_s, "_DF.txt")
  # print(filename)
  #df = filename
  turnos = spliTurno(load_df(filename, ano))
  for (t in turnos) {
    out_filename = paste0(DATA_DIR, "/mz/", ano_s, "_", unique(t$NUM_TURNO), ".csv")
    write.csv(t, file=out_filename, row.names = F)
  }
}

# 2006
print(2006)
filename = paste0(DATA_DIR, "/detalhe_votacao_munzona_2006_BR.txt")
ano = 2006
ano_s = "2006"
df06 <- load_df(filename, ano)
turnos = spliTurno(df06)
for (t in turnos) {
  out_filename = paste0(DATA_DIR, "/mz/", ano_s, "_", unique(t$NUM_TURNO), ".csv")
  write.csv(t, file=out_filename, row.names = F)
}
