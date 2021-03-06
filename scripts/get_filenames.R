library(utils)
library(dplyr)
source("aux.R")

if (!exists("avisos")) {
  avisos = list()
}


get_filenames = function (anos, 
                          ufs, 
                          data_dir = "~/data/TSE/Resultados/",
                          kind = "secao")  {
  # ===============================================================
  # Geração de nomes de arquivos
  # ===============================================================
  MaxSize = length(anos) * length(ufs)
  
  if (!kind %in% c("secao", "munzona")) {
    stop(paste("kind deve ser secao ou munzona, e não", kind))
  }
  
  if (kind == "secao") {
    file_base_name = "detalhe_votacao_secao_"
  } else {
    file_base_name = "detalhe_votacao_munzona_"
  }
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
