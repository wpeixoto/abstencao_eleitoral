UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ED", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "RJ", "PI", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
# UFs = c("AC", "AL")  # , "AP", "AM", "BA", "CE", "DF", "ED", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "RJ", "PI", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

get_dv = function(filename, all = FALSE, full=FALSE, cargo=3) {
  dv = read.csv(filename, header = F, encoding = "ISO-8859", sep = ";")
  names(dv) = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", "CODIGO_CARGO", "DESCRICAO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP")
  
  if (!all) {
    dv = dv[dv$CODIGO_CARGO == cargo, ]  # Restringir a Governador, que tem os dois turnos. TODO: O que fazer se não tiver tido 2º turno? Presidente quase sempre tem
  }
  
  
  dv$TAXA_COMPARECIMENTO = dv$QTD_COMPARECIMENTO / dv$QTD_APTOS
  dv$TAXA_ABSTENCAO = dv$QTD_ABSTENCOES / dv$QTD_APTOS
  verif_taxa = dv$TAXA_ABSTENCAO + dv$TAXA_COMPARECIMENTO
  if (any(verif_taxa != 1)) { 
    head(dv[verif_taxa != 1, ])
    warning(paste("Inconsistênca entre quantidades de abstenções e de comparecimentos em ", unique(dv$ANO_ELEICAO), " ", unique(dv$SIGLA_UF), "\n"))
  }
  
  library(stringr)
  dv$NUMERO_SECAO = str_pad(dv$NUMERO_SECAO, 3, pad="0") 
  dv$NUM_ZONECAO = paste0(dv$NUMERO_ZONA, dv$NUMERO_SECAO)  
  
  if (!full) {  # Excluir campos desnecessários
    # dv$QTD_COMPARECIMENTO = NULL
    # dv$QTD_APTOS = NULL
    # dv$DESCRICAO_CARGO = NULL
    dv = dv[, c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", "CODIGO_CARGO", "NUM_ZONECAO")]
    
  }
  
  return(dv)
}

# ===============================================================
# AQUI
# ===============================================================
MaxSize = length(UFs) * 12
rm(ttt)
ttt = data.frame(
  ano=rep(NA, MaxSize), 
  uf=rep("", MaxSize), 
  isna=rep(NA, MaxSize),
  orig_rows=rep(NA, MaxSize),
  n_rows_gov=rep(NA, MaxSize),
  verif_taxa_consistente=rep(NA, MaxSize),
  stringsAsFactors = F
  )

cont = 1

for (ano in seq(1994,2016,2)) {
  for (uf in UFs) {
    s_ano = as.character(ano)
    filename = paste0("~/data/TSE/Resultados/", s_ano, "/", "detalhe_votacao_secao_", s_ano, "_", uf, ".txt")
    if (!file.exists(filename)) {
      print(paste("Arquivo '", filename, "' não encontrado"))
      next
    }
    all = FALSE
    full=FALSE 
    cargo=3
    dv = read.csv(filename, header = F, encoding = "ISO-8859", sep = ";")
    orig_lines = nrow(dv)
    names(dv) = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", "CODIGO_CARGO", "DESCRICAO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP")
    
    if (!all) {
      dv = dv[dv$CODIGO_CARGO == cargo, ]  # Restringir a Governador, que tem os dois turnos. TODO: O que fazer se não tiver tido 2º turno? Presidente quase sempre tem
    }
    
    
    dv$TAXA_COMPARECIMENTO = dv$QTD_COMPARECIMENTO / dv$QTD_APTOS
    dv$TAXA_ABSTENCAO = dv$QTD_ABSTENCOES / dv$QTD_APTOS
    verif_taxa = dv$TAXA_ABSTENCAO + dv$TAXA_COMPARECIMENTO
    if (any(verif_taxa != 1)) { 
      head(dv[verif_taxa != 1, ])
      print(paste("Inconsistênca entre quantidades de abstenções e de comparecimentos em ", unique(dv$ANO_ELEICAO), " ", unique(dv$SIGLA_UF), "\n"))
    }
    
    
    Any_NA = any(is.na(dv))
    
    ttt[cont,] = list(ano, uf, Any_NA,orig_lines,  nrow(dv), !any(verif_taxa != 1))
    cont = cont + 1
  }
}

ttt