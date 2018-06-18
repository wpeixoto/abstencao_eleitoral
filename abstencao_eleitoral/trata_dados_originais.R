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

#print("Apenas 1998")
# br_1998 = read.csv("./dados/recortes/detalhe_votacao_secao_1998_BR.txt", header = F, encoding = "ISO-8859", sep = ";", stringsAsFactors = F)
# # br_1998 = read.csv(build_filename(data_subdir = "1998/", ano=1998, uf="DF"), header = F, encoding = "ISO-8859", sep = ";", stringsAsFactors = F)
# NOMES_CAMPOS_ORIG = c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", 
#                       "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", "NUMERO_SECAO", 
#                       "CODIGO_CARGO", "DESCRICAO_CARGO", 
#                       "QTD_APTOS", "QTD_COMPARECIMENTO", "QTD_ABSTENCOES", 
#                       "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP")
# 
# names(br_1998) = NOMES_CAMPOS_ORIG
# 
# df_br_1998 = br_1998[br_1998$SIGLA_UF == "DF", ]



bld_filename <- function(ano) {  # Abrevia nome de arquivo
  s_ano = as.character(ano)
  paste0(input_data_dir, s_ano, "/", file_base_name, s_ano, "_", "DF", ".txt")
}

df_1998 = get_dv(filename = bld_filename(1998), full=T)

df_1998$SIGLA_UF = "DF"  # Corrige erro nos dados

# Apenas governador. Excesso de zelo, pois só tem resultados para governador
recorte_1998 = df_1998[df_1998$CODIGO_CARGO == 3, ]  
recorte_1998 = recorte_1998[, c("ANO_ELEICAO", "NUM_TURNO", 
                        "NUMERO_ZONA", "NUMERO_SECAO", 
                        "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", 
                        "TAXA_ABSTENCAO")]

outfile_name = paste0(output_data_dir, "DF_1998.csv")
write.csv(recorte_1998, outfile_name, sep=";", row.names = F)

rm(df_1998, recorte_1998)

for (ano in c(2002, 2006, 2010, 2014)) {
  dd = get_dv(filename = bld_filename(ano), full=T)
  

  
  df_rec = dd[dd$CODIGO_CARGO == 3, ]  
  df_rec = df_rec[, c("ANO_ELEICAO", "NUM_TURNO",
                      "NUMERO_ZONA", "NUMERO_SECAO", 
                      "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS", 
                      "TAXA_ABSTENCAO")]
  outfile_name = paste0(output_data_dir, "DF_", ano, ".csv")
  file.remove(outfile_name)
  write.csv(df_rec, outfile_name, row.names = F)
}
