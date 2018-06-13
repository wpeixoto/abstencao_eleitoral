# ====================================================================
# Verificação de sanidade dos arquivos
#
# Este script tenta descobrir quais arquivos têm problemas que podem
# dificultar ou impedir a análise.

library(utils)
UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", 
        "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
        "PE", "RJ", "PI", "RN", "RS", "RO", "RR", "SC", 
        "SP", "SE", "TO")

ANOS = seq(1994,2016,2) # Todos os anos, para verificações gerais
ANOS_GERAIS = seq(1994, 2014, 4)  # Eleições gerais (Presidente, Senador, Governador e Deputados)
ANOS_MUNICIPAIS = seq(1996, 2016, 4)  # Eleições municipais (Prefeito e vereadores)

# Campos relevantes para chegagem de sanidade
CAMPOS_RELEVANTES_S = c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", 
                        "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "MUMERO_SECAO", "CODIGO_CARGO",
                        "DESCRICAO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES",
                        "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "TAXA_COMPARECIMENTO", "TAXA_ABSTENCAO")

# Mínimo para a análise
CAMPOS_MAIS_RELEVANTES = c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", 
                           "CODIGO_MUNICIPIO", "NUMERO_ZONA", "MUMERO_SECAO", "CODIGO_CARGO",
                           "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "TAXA_ABSTENCAO")

source("get_dv.R")
# ===============================================================
# AQUI
# ===============================================================

MaxSize = length(UFs) * length(ANOS)
rm(ttt)
ttt = data.frame(
 ano=rep(NA, MaxSize), 
 uf=rep("", MaxSize), 
 any_na=rep(NA, MaxSize),
 orig_rows=rep(NA, MaxSize),
 n_rows_gov=rep(NA, MaxSize),
 verif_taxa_consistente=rep(NA, MaxSize),
 # qtd_faltam_colunas=rep(NA, MaxSize),
 qtd_colunas=rep(NA, MaxSize),
 stringsAsFactors = F
 )


cont = 1

# ===============================================================
# Geração de nomes de arquivos
# ===============================================================
filenames = character(MaxSize)
not_found_filenames = character(MaxSize/2)  # Estimo que haverá poucos inexistentes
i = 1
j = 1

pb = txtProgressBar(min=0, max=MaxSize, style = 3)
pbc = 0
for (ano in ANOS) {
  for (uf in UFs) {
    pbc = pbc + 1
    s_ano = as.character(ano)
    filename = paste0("~/data/TSE/Resultados/", s_ano, "/", "detalhe_votacao_secao_", s_ano, "_", uf, ".txt")
    if (file.exists(filename)) {
      filenames[i] = filename
      i = i + 1
    } 
    else
    {
      not_found_filenames[j] = filename
      j = j+1
      # print(paste("Arquivo '", filename, "' não encontrado"))
      # next
    }
    setTxtProgressBar(pb, pbc)
  }
}

# ===============================================================
# Carregar os arquivos
# ===============================================================
filenames = filenames[filenames!=""]
NOMES_CAMPOS = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", 
                 "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", 
                 "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", "CODIGO_CARGO", 
                 "DESCRICAO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", 
                 "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", 
                 "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP")


ktchall = function(o) {
  print(paste("::>:: MSG", o, "\nArquivo", filename))
}

print(paste("Há", length(filenames), "arquivos existentes"))
pb = txtProgressBar(min=0, max=MaxSize, style = 3)
pbc = 0

for (filename in filenames) {
  # break
    all = FALSE
    full=FALSE 
    # cargo=3
    cargos = c(3, 9)  # Vereador e prefeito
    pbc = pbc + 1
    tryCatch({
      dv = read.csv(filename, header = F, encoding = "ISO-8859", sep = ";")
    }, error=ktchall, warning=ktchall)
    orig_lines = nrow(dv)
    if (length(dv) != length(NOMES_CAMPOS)) {
      print(paste("Erro no arquivo", filename, ":: Largura diferente:", length(dv)))
      next
    }
    names(dv) = NOMES_CAMPOS

    dv$TAXA_COMPARECIMENTO = dv$QTD_COMPARECIMENTO / dv$QTD_APTOS
    dv$TAXA_ABSTENCAO = dv$QTD_ABSTENCOES / dv$QTD_APTOS
    verif_taxa = dv$TAXA_ABSTENCAO + dv$TAXA_COMPARECIMENTO
    # if (any(verif_taxa != 1)) { 
    #  head(dv[verif_taxa != 1, ])
    #  print(paste("Inconsistênca entre quantidades de abstenções e de comparecimentos em ", unique(dv$ANO_ELEICAO), " ", unique(dv$SIGLA_UF), "\n"))
    #}
    
    Any_NA = any(is.na(dv))
    # Faltam_colunas = any(is.na(dv$QT_VOTOS_ANULADOS_APU_SEP))
    # qtd_faltam_colunas = length(dv[is.na(dv$QT_VOTOS_ANULADOS_APU_SEP), ])
    
    ttt[cont,] = list(ano, uf, Any_NA,orig_lines,  nrow(dv), !any(verif_taxa != 1), length(dv))
    cont = cont + 1
    setTxtProgressBar(pb, pbc)
  }


# ttt