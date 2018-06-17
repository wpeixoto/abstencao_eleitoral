library(utils)
source("aux.R")

# TODO: Verificar existência da lista de avisos

get_filenames = function (anos, ufs, data_dir = "~/data/TSE/Resultados/")  {
  # ===============================================================
  # Geração de nomes de arquivos
  # ===============================================================
  MaxSize = length(anos) * length(ufs)
  
  file_base_name = "detalhe_votacao_secao_"
  cont = 1
  
  filenames = character(MaxSize)
  not_found_filenames = character(MaxSize/2)  # Estimo que haverá poucos inexistentes
  i = 1
  j = 1
  
  pb = txtProgressBar(min=0, max=MaxSize, style = 3)
  pbc = 0
  for (ano in anos) {
    for (uf in ufs) {
      pbc = pbc + 1
      s_ano = as.character(ano)
      filename = paste0(data_dir, s_ano, "/", file_base_name, s_ano, "_", uf, ".txt")
      if (file.exists(filename)) {
        filenames[i] = filename
        i = i + 1
      } 
      else
      {
        avisos[[paste0(uf,"/",s_ano)]] == paste("Arquivo ", filename, "não encontrado")
        not_found_filenames[j] = filename
        j = j+1
        # print(paste("Arquivo '", filename, "' não encontrado"))
        # next
      }
      setTxtProgressBar(pb, pbc)
    }
  }
  
  return(filenames)
}
