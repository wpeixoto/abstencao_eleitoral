# Geração de novos arquivos com os dados tratados (ou um só)

# No caso deste estudo, vai coletar os dados originais de resultados de eleições no DF,
# corrigir problemas detectados, remover colunas não utilizadas 
# e salvar os dados em um único arquivo .csv.

# ANO_ELEICAO, NUM_TURNO, NUMERO_ZONA, NUMERO_SECAO, QTD_APTOS, TAXA_ABSTENCAO, 
# QT_VOTOS_BRANCOS, QT_VOTOS_NULOS

source("get_filenames.R")
source("get_dv.R")

input_data_dir = "~/data/TSE/Resultados/"
output_data_dir = "~/data/TSE/Resultados/recortes/"
file_base_name = "detalhe_votacao_secao_"

# build_filename <- function(data_dir = input_data_dir, 
#                            data_subdir = "", 
#                            file_base_name = file_base_name, 
#                            ano,
#                            uf) {
#   s_ano = paste0(as.character(ano), "/")
#   return(paste0(data_dir, data_subdir, file_base_name, s_ano, uf) )
# }

print("Apenas 1998")
br_1998 = read.csv("./dados/recortes/detalhe_votacao_secao_1998_BR.txt", header = F, encoding = "ISO-8859", sep = ";", stringsAsFactors = F)
# br_1998 = read.csv(build_filename(data_subdir = "1998/", ano=1998, uf="DF"), header = F, encoding = "ISO-8859", sep = ";", stringsAsFactors = F)
NOMES_CAMPOS_ORIG = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", 
                      "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", 
                      "CODIGO_CARGO", "DESCRICAO_CARGO", 
                      "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", 
                      "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP")

names(br_1998) = NOMES_CAMPOS_ORIG

df_br_1998 = br_1998[br_1998$SIGLA_UF == "DF", ]

s_ano = as.character(1998)
filename = paste0(input_data_dir, s_ano, "/", file_base_name, s_ano, "_", "DF", ".txt")

df_1998 = get_dv(filename = filename, full=T)

df_1998$SIGLA_UF = "DF"  # Corrige erro nos dados
