# ===========================================
# Testes exploratórios do DF
#

library(utils)
ANOS_GERAIS = seq(1998, 2014, 4)  # Eleições gerais (Presidente, Senador, Governador e Deputados)

# Campos relevantes para chegagem de sanidade
CAMPOS_RELEVANTES_S = c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", 
                        "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "MUMERO_SECAO", "CODIGO_CARGO",
                        "DESCRICAO_CARGO", "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES",
                        "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "TAXA_COMPARECIMENTO", "TAXA_ABSTENCAO")

# Mínimo para a análise
CAMPOS_MAIS_RELEVANTES = c("ANO_ELEICAO", "NUM_TURNO", "SIGLA_UF", "SIGLA_UE", 
                           "CODIGO_MUNICIPIO", "NUMERO_ZONA", "MUMERO_SECAO", "CODIGO_CARGO",
                           "QTD_VOTOS_BRANCOS", "QTD_VOTOS_NULOS", "TAXA_ABSTENCAO")

DATA_DIR = "./"

eleicoes = rep(NA, length(ANOS_GERAIS))
c_e = 1
source("get_dv.R")

# ddv = get_dv()

for (ano in ANOS_GERAIS) {
    s_ano = as.character(ano) 
    eleicoes[c_e] = get_dv(paste0(DATA_DIR, "detalhe_votacao_secao_", s_ano, "_DF.txt"))
    c_e = c_e + 1
}
