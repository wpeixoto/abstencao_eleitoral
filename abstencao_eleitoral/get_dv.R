# Função get_dv para todos

warn_nas = function(x, ignore.nas = F, field.name = "", msg="") {
  if (msg == "") {
    if (field.name == "") {
        msg = "Há NAs no conjunto indicado"
    }
    else {
        msg = paste0("Há NAs no campo '", field.name, "'.")
    }
  }
  if (any(!ignore.nas & is.na(x))) {
    warning(msg)
  }    
}

get_dv = function(filename,        # Nome de um arquivo do TSE
                  all = FALSE,     #  TRUE == Todas as linhas
                  full=FALSE,      #  TRUE == todas as colunas originais
                  cargos= c(3, 9), #  Vetor com cargos para filtrar. Default: Governador e Presidente
                  ignore.nas = F)  
  {
  NOMES_CAMPOS_ORIG = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", 
                        "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", 
                        "CODIGO_CARGO", "DESCRICAO_CARGO", 
                        "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", 
                        "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP")

  CAMPOS_RELEVANTES_S = c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", 
                          "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "MUMERO_SECAO", "CODIGO_CARGO",
                          "DESCRICAO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES",
                          "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "TAXA_COMPARECIMENTO", "TAXA_ABSTENCAO")
  
  # Mínimo para a análise
  CAMPOS_MAIS_RELEVANTES = c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", 
                             "CODIGO_MUNICIPIO", "NUMERO_ZONA", "MUMERO_SECAO", "CODIGO_CARGO",
                             "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "TAXA_ABSTENCAO")
  
  if (!file.exists(filename)) {
    stop(paste("Arquivo '", filename, "' não foi encontrado."))
    return()
  }
  dv = read.csv(filename, header = F, encoding = "ISO-8859", sep = ";")

  if (length(dv) != length(NOMES_CAMPOS_ORIG)) {
    warning(paste("Largura diferente!", length(dv)))
  } 

  names(dv) = NOMES_CAMPOS_ORIG
  
  if (!all) {
    dv = dv[dv$CODIGO_CARGO %in% cargos, ]  # Restringir a Governador e prefeito, que tem os dois turnos. TODO: O que fazer se não tiver tido 2º turno? Presidente quase sempre tem. Prefeito parece ser o contrário
  }

  warn_nas(dv$QTD_COMPARECIMENTO, ignore.nas, field.name = "QTD_COMPARECIMENTO")  
  warn_nas(dv$QTD_ABSTENCOES, ignore.nas, field.name = "QTD_ABSTENCOES")  
  warn_nas(dv$QTD_APTOS, ignore.nas, field.name = "QTD_APTOS")  
  
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
    dv = dv[, CAMPOS_MAIS_RELEVANTES]
    
  }
  
  return(dv)
}
