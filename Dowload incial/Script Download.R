# install.packages("remotes")
#remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(tidyverse)

# Optou-se por realizar o Dowload de dados dessa forma pois tinha-se a intenção de ter um daset bruto completo, 
# mas ao tentar baixar os dados de forma única se demostrava impossível dado o consumo de memório, 
# logo opote-se por fracionar em datasets específicos

# Dados do TO, para verificação de volume
dadosTO <- fetch_datasus(
  year_start = 2010,
  year_end = 2022,
  uf = "TO",
  information_system = "SIM-DO"
)
dadosTO <- process_sim(dadosTO)
# Dados dos Primeiros 6 Estados
dadosP6 <- fetch_datasus(
  year_start = 2010,
  year_end = 2022,
  uf = c("AC", "AL", "AP", "AM", "BA", "CE"),
  information_system = "SIM-DO"
)
dadosP6 <- process_sim(dadosP6)
# Unindo o TO aos depois estados
dadosP7 <- bind_rows(dadosP6, dadosTO)
#Salvando dataset
saveRDS(dadosP7,"AC_AL_AP_AM_BA_CE_TO_NFILTER.RDS")

#Dados 3 maiores estados
dadosG3 <- fetch_datasus(
  year_start = 2010,
  year_end = 2022,
  uf = c("SP", "RJ", "MG"),
  information_system = "SIM-DO"
)
dadosG3 <- process_sim(dadosG3)
#Salvando dataset
saveRDS(dadosG3,"SP_RJ_MG_NFILTER.RDS")

##Dados de mais 6 estados

dadosN6 <- fetch_datasus(
  year_start = 2010,
  year_end = 2022,
  uf = c("DF", "ES", "GO", "MA", "MS", "PA"),
  information_system = "SIM-DO"
)
dadosN6 <- process_sim(dadosN6)
#Salvando dataset
saveRDS(dadosN6,"DF_ES_GO_MA_MS_PA_NFILTER.RDS")

##Dados de mais 6 estados

dadosE6 <- fetch_datasus(
  year_start = 2010,
  year_end = 2022,
  uf = c("PB", "PR", "PE", "PI", "RN", "RS"),
  information_system = "SIM-DO"
)
dadosE6 <- process_sim(dadosE6)
#Salvando dataset
saveRDS(dadosE6,"PB_PR_PE_PI_RN_RS_NFILTER.RDS")

#Dados dos últimos 5 estados
dadosF5 <- fetch_datasus(
  year_start = 2010,
  year_end = 2022,
  uf = c("MT", "RO", "RR", "SC", "SE"),
  information_system = "SIM-DO"
)
dadosF5 <- process_sim(dadosF5)
#Salvando dataset
saveRDS(dadosF5,"MT_RO_RR_SC_SE_NFILTER.RDS")
#############################################################
#Carregando Datasets
dados1 <- readr::read_rds("PB_PR_PE_PI_RN_RS_NFILTER.RDS")
dados2 <- readr::read_rds("AC_AL_AP_AM_BA_CE_TO_NFILTER.RDS")
dados3 <- readr::read_rds("SP_RJ_MG_NFILTER.RDS")
dados4 <- readr::read_rds("MT_RO_RR_SC_SE_NFILTER.RDS")
dados5 <- readr::read_rds("DF_ES_GO_MA_MS_PA_NFILTER.RDS")

#Filtrando os dados para as variáveis de auto-lesão
dados1_filtered <- dados1[dados1$CAUSABAS >= "X60" & dados1$CAUSABAS <= "X84"| dados1$CAUSABAS == "Y870", ]
dados2_filtered <- dados2[dados2$CAUSABAS >= "X60" & dados2$CAUSABAS <= "X84"| dados2$CAUSABAS == "Y870", ]
dados3_filtered <- dados3[dados3$CAUSABAS >= "X60" & dados3$CAUSABAS <= "X84"| dados3$CAUSABAS == "Y870", ]
dados4_filtered <- dados4[dados4$CAUSABAS >= "X60" & dados4$CAUSABAS <= "X84"| dados4$CAUSABAS == "Y870", ]
dados5_filtered <- dados5[dados5$CAUSABAS >= "X60" & dados5$CAUSABAS <= "X84"| dados5$CAUSABAS == "Y870", ]

maindataset <- bind_rows(dados2_filtered, dados3_filtered, dados5_filtered, dados1_filtered, dados4_filtered)
#Salvando dataset de autolesão entre 2010 a 2022
saveRDS(maindataset,"maindataset_NFILTER.RDS")
