######################################## Script referente a limpeza e consolidação de dados ###############################
#Ambiente
rm(list=ls(all=TRUE))
options(scipen=999)

#Bibliotecas
library(tidyverse)
library(basedosdados)
library(gargle)
library(bigrquery)
library(lubridate)
library(sidrar)
library(scales)
library(readxl)
library(zoo)

# Carregando dados baixados anteriormente
maindataset <- readRDS("maindataset_NFILTER.RDS")

## Selecionando variáveis úteis e aplicando ao Dataset
keeps <- c("DTOBITO", "DTNASC", "SEXO","RACACOR", "ESTCIV", "ESC", "OCUP", "CODMUNRES","LOCOCOR", "CODMUNOCOR",
           "ESCMAE" ,"OCUPMAE","LINHAA", "LINHAB", 
           "ASSISTMED", "CAUSABAS", "CAUSABAS_O", "IDADEanos", "munResNome", "munResUf",
           "munResLat", "munResLon", "munResArea")

df <- maindataset[keeps]

## Transformação da data e ano específico
df$DTOBITO <- as.Date(df$DTOBITO)
df$ano_obito <- year(df$DTOBITO)

# Criando um novo dataset
state_mapping <- data.frame(
  StateName = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"), # Add all states
  UF = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO") # Corresponding UFs
)

# Onehot coding sexo data
df$SEXO_encoded <- ifelse(df$SEXO == "Masculino", 1, 0)

# Grouping by munResuf and ano_obito and counting males and females
result <- df %>%
  group_by(munResUf, ano_obito, SEXO_encoded) %>%
  summarise(count = n()) %>%
  spread(key = SEXO_encoded, value = count, fill = 0) %>%
  rename(female = `0`, male = `1`)

new_dataset <- df %>%
  group_by(munResUf, ano_obito) %>%
  summarise(deaths = n(), .groups = 'drop')

# Joining the datasets
final_dataset <- left_join(new_dataset, state_mapping, by = c("munResUf" = "StateName")) %>%
  left_join(result, by = c("munResUf", "ano_obito"))

#Realizando o Dowwloand dos dados de população e PIB
#Setando o o billing do Google Clound
basedosdados::set_billing_id("workshop-base-dos-dados-376715")

#Puxandos os dados referentes a população do municipio dos UF de 2010 a 2020
pib_2007_2020 <- basedosdados::read_sql(query =("SELECT
    sigla_uf,
    ano,
    pib.pib,
FROM 
    `basedosdados.br_ibge_pib.uf` pib
WHERE 
    pib.ano BETWEEN 2007 AND 2020"))

pib_2012_2020 <-  pib_2007_2020 %>%
  filter(ano>=2012)

pib2021 <- data.frame(sigla_uf = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  pib = c(21374440000, 76265620000, 20099851000, 131531038000, 352617852000, 194884802000, 286943782000, 186336505000, 269627874000, 124980720000, 857593214000, 142203766000, 233390203000, 262904979000, 77470331000, 220813522000, 64028303000, 549973062000, 949300770000, 80180733000, 58170096000, 18202579000, 581283677000, 428570889000, 51861397000, 2719751231000, 51780764000))
## Criar uma nova coluna "ano" preenchida com o valor 2021 e consolidando dados de pib
pib2021$ano <- 2021
pib_2012_2020$pib <- as.double(pib_2012_2020$pib)
pib_2012_2020$ano <- as.double(pib_2012_2020$ano)

pib_total <- full_join(pib_2012_2020, pib2021, by = c("sigla_uf", "ano", "pib"))

## Unindo os datasets até 2021
final_dataset_filtered <- final_dataset %>% 
  filter(ano_obito <= 2021)
final_dataset_death <- left_join(final_dataset_filtered, pib_total, by = c("UF" = "sigla_uf", "ano_obito" = "ano"))

saveRDS(final_dataset_death, "final_dataset_death.RDS")
## Limpando memória
rm(df,state_mapping,result,new_dataset,pib2021,final_dataset_filtered,maindataset,pib_2012_2020,final_dataset,keeps,pib_total)

#################################################################################
# Baixando dados de desemprego referentes a 2012 a 2021
# Fetching data
# desemp_2012_2021 <- get_sidra(
#   x = 4092,
#   geo = "State",
#   header = TRUE,
#   period = "201201-202104"
# ) %>%
#   janitor::clean_names()

## Processing data
## Agrupando dados trimestrais a níveis anuais e realizando as operações
## Baixando dados de desemprego referentes a 2012 a 2021
## Fetching data
# desemp_2012_2021 <- get_sidra(
#   x = 4092,
#   geo = "State",
#   header = TRUE,
#   period = "201201-202104"
# ) %>%
#   janitor::clean_names()
# 
# # Processing data
# # Agrupando dados trimestrais a níveis anuais e realizando as operações
# desemp_2012_2021 <- desemp_2012_2021 %>%
#  filter(
#    condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao %in% c(
#      "Força de trabalho - desocupada",
#     "Força de trabalho"
#   ),
#   unidade_de_medida == "Mil pessoas"
# ) %>%
# select(unidade_da_federacao, trimestre_codigo, condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao, valor) %>%
# pivot_wider(
#   names_from = condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao,
#   values_from = valor
# ) %>%
# mutate(
#   year = as.integer(substring(trimestre_codigo, 1, 4))
# ) %>%
# group_by(unidade_da_federacao, year) %>%
# summarize(
#   taxa_des = sum(`Força de trabalho - desocupada`) / sum(`Força de trabalho`) * 100
# ) %>%
# ungroup()

## Read sheet and join datasets dos outros dados coletado
dados_complentares <- read_excel("dados_complentares.xlsx")
dados_complentares <- dados_complentares %>%
  select(-state)
final_dataset_death_1 <- left_join(final_dataset_death, dados_complentares, by = c("UF" = "uf", "ano_obito" = "ano"))
# Unindo dados da taxa de desemprego e filtrando para os anos de 2012 a 2021
# final_dataset_death_2 <- left_join(final_dataset_death_1, desemp_2012_2021, 
                                     # by = c("munResUf" = "unidade_da_federacao", "ano_obito" = "year"))
# arquivo do backup com a forma com os dados de tax_desemp do sidra, para agilizar o precesso
abc <- readRDS("abc.RDS")
abc1 <- abc %>% 
  select(uf, ano_obito ,taxa_des)

final_dataset_death_2 <- left_join(final_dataset_death_1, abc1,
                                   by =c("UF" = "uf", "ano_obito"))
df_consol <- final_dataset_death_2 %>%
  rename(estados = munResUf) %>%
  filter(ano_obito >= 2012, ano_obito <= 2021) %>%
  janitor::clean_names()

## Calculado suic_rate
df_consol_final <- df_consol %>%
  mutate(suic_rate_total = (deaths / pop_total) * 100000) %>%
  mutate(suic_rate_masc = (male / dh_pop_total) * 100000) %>% 
  mutate(suic_rate_fema = (female / dm_pop_total) * 100000)

saveRDS(df_consol_final, "df_consol_final.RDS")
## Limpando memória
rm(desemp_2012_2021,final_dataset_death_1,final_dataset_death_2,dados_complentares,df_consol,final_dataset_death)
#########################################################################################
# Baixando valores IPCA
IPCA_2007_2021 <- basedosdados::read_sql(query =("SELECT ano, variacao_doze_meses
    FROM basedosdados.br_ibge_ipca.mes_brasil
    WHERE mes = 12
    AND ano BETWEEN 2007 AND 2021
    ORDER BY ano"))

IPCA_2012_2021 <- IPCA_2007_2021 %>%
  filter(ano>=2012)
  
IPCA_2012_2021$ano <- as.double(IPCA_2012_2021$ano)
df_conso_norm <- left_join(df_consol_final, IPCA_2012_2021, by = c("ano_obito" = "ano"))

# Deflacionando pib
## Selecionando a valor útilmo valor da coluna, repetindo 270 para adequar ao dataset
df_conso_norm$last_value <- as.double(rep(tail(df_conso_norm[,37],1),270))
## Criando a coluna deflacionamento
df_conso_norm$pib_norma = df_conso_norm[,8]*(df_conso_norm[,38]/df_conso_norm[,37])
## Removendo colunas sobressalentes
df_conso_norm <- df_conso_norm %>%
  select(-last_value, -variacao_doze_meses)
# Alterando df para coluna e renomeand
df_conso_norm <- df_conso_norm %>%
  unnest(pib_norma, names_repair = "unique") %>%
  rename(pib_normalizado = pib...38, pib = pib...8)

# Criando a proporção da população acima de 65 anos para os 3 níveis de agregação (masculino, feminino e ambos os sexos)
df_conso_norm$populacao_de_65_anos_ou_mais <- as.numeric(as.character(df_conso_norm$populacao_de_65_anos_ou_mais))
df_conso_norm$pop_65 = df_conso_norm$populacao_de_65_anos_ou_mais/df_conso_norm$pop_total
df_conso_norm$h_populacao_de_65_anos_ou_mais <- as.numeric(as.character(df_conso_norm$h_populacao_de_65_anos_ou_mais))
df_conso_norm$h_pop_65 = df_conso_norm$h_populacao_de_65_anos_ou_mais/df_conso_norm$dh_pop_total
df_conso_norm$f_populacao_de_65_anos_ou_mais <- as.numeric(as.character(df_conso_norm$f_populacao_de_65_anos_ou_mais))
df_conso_norm$f_pop_65 = df_conso_norm$f_populacao_de_65_anos_ou_mais/df_conso_norm$dm_pop_total

# Criando a taxa de divorcio para os 3 níveis de agregação (masculino, feminino e ambos os sexos)
df_conso_norm$populacao_de_18_anos_ou_mais <- as.numeric(as.character(df_conso_norm$populacao_de_18_anos_ou_mais))
df_conso_norm$tx_div = (df_conso_norm$div/df_conso_norm$populacao_de_18_anos_ou_mais)*100000
df_conso_norm$h_populacao_de_18_anos_ou_mais <- as.numeric(as.character(df_conso_norm$h_populacao_de_18_anos_ou_mais))
df_conso_norm$h_tx_div = (df_conso_norm$div/df_conso_norm$h_populacao_de_18_anos_ou_mais)*100000
df_conso_norm$f_populacao_de_18_anos_ou_mais <- as.numeric(as.character(df_conso_norm$f_populacao_de_18_anos_ou_mais))
df_conso_norm$f_tx_div = (df_conso_norm$div/df_conso_norm$f_populacao_de_18_anos_ou_mais)*100000


# saldo_emprego_caged <- basedosdados::read_sql(query =("SELECT
#   sigla_uf,
#   ano,
#   SUM(CASE WHEN CAST(sexo AS INT64) = 1 THEN saldo_movimentacao ELSE 0 END) AS total_saldo_movimentacao_male,
#   SUM(CASE WHEN CAST(sexo AS INT64) = 2 THEN saldo_movimentacao ELSE 0 END) AS total_saldo_movimentacao_female,
#   SUM(CASE WHEN CAST(sexo AS INT64) = 1 THEN saldo_movimentacao ELSE 0 END) +
#   SUM(CASE WHEN CAST(sexo AS INT64) = 2 THEN saldo_movimentacao ELSE 0 END) AS total_saldo_movimentacao
# FROM
#   `basedosdados.br_me_caged.microdados_antigos`
# WHERE ano BETWEEN 2012 AND 2019
# GROUP BY
#   sigla_uf,
#   ano
# UNION ALL
# -- Segunda consulta para 2020-2021
# SELECT
#   sigla_uf,
#   ano,
#   SUM(CASE WHEN CAST(sexo AS INT64) = 1 THEN saldo_movimentacao ELSE 0 END) AS total_saldo_movimentacao_male,
#   SUM(CASE WHEN CAST(sexo AS INT64) = 3 THEN saldo_movimentacao ELSE 0 END) AS total_saldo_movimentacao_female,
#   SUM(CASE WHEN CAST(sexo AS INT64) = 1 THEN saldo_movimentacao ELSE 0 END) +
#   SUM(CASE WHEN CAST(sexo AS INT64) = 3 THEN saldo_movimentacao ELSE 0 END) AS total_saldo_movimentacao
# FROM
#   `basedosdados.br_me_caged.microdados_movimentacao`
# WHERE ano IN (2020, 2021)
# GROUP BY
#   sigla_uf,
#   ano
# ORDER BY
#   sigla_uf, ano"))

saldo_emprego_caged <- basedosdados::read_sql(query =("WITH dados_agrupados AS (
  SELECT
    sigla_uf,
    ano,
    SUM(CASE WHEN CAST(sexo AS INT64) = 1 AND saldo_movimentacao = -1 THEN 1 ELSE 0 END) AS total_male_negative,
    SUM(CASE WHEN CAST(sexo AS INT64) = 2 AND saldo_movimentacao = -1 THEN 1 ELSE 0 END) AS total_female_negative,
    SUM(CASE WHEN CAST(sexo AS INT64) = 1 AND saldo_movimentacao = 1 THEN 1 ELSE 0 END) AS total_male_positive,
    SUM(CASE WHEN CAST(sexo AS INT64) = 2 AND saldo_movimentacao = 1 THEN 1 ELSE 0 END) AS total_female_positive,
    SUM(CASE WHEN saldo_movimentacao = -1 THEN 1 ELSE 0 END) AS total_negative,
    SUM(CASE WHEN saldo_movimentacao = 1 THEN 1 ELSE 0 END) AS total_positive
  FROM
    `basedosdados.br_me_caged.microdados_antigos`
  WHERE ano BETWEEN 2012 AND 2019
  GROUP BY
    sigla_uf, ano
  UNION ALL
  SELECT
    sigla_uf,
    ano,
    SUM(CASE WHEN CAST(sexo AS INT64) = 1 AND saldo_movimentacao = -1 THEN 1 ELSE 0 END) AS total_male_negative,
    SUM(CASE WHEN CAST(sexo AS INT64) = 3 AND saldo_movimentacao = -1 THEN 1 ELSE 0 END) AS total_female_negative,
    SUM(CASE WHEN CAST(sexo AS INT64) = 1 AND saldo_movimentacao = 1 THEN 1 ELSE 0 END) AS total_male_positive,
    SUM(CASE WHEN CAST(sexo AS INT64) = 3 AND saldo_movimentacao = 1 THEN 1 ELSE 0 END) AS total_female_positive,
    SUM(CASE WHEN saldo_movimentacao = -1 THEN 1 ELSE 0 END) AS total_negative,
    SUM(CASE WHEN saldo_movimentacao = 1 THEN 1 ELSE 0 END) AS total_positive
  FROM
    `basedosdados.br_me_caged.microdados_movimentacao`
  WHERE ano IN (2020, 2021)
  GROUP BY
    sigla_uf, ano
)
SELECT
  sigla_uf,
  ano,
  total_male_negative - total_male_positive AS saldo_male,
  total_female_negative - total_female_positive AS saldo_female,
  total_negative - total_positive AS saldo_total
FROM
  dados_agrupados
ORDER BY
  sigla_uf, ano"))

saldo_emprego_caged$ano <- as.double(saldo_emprego_caged$ano)
df_conso_norm <- left_join(df_conso_norm, saldo_emprego_caged, by = c("ano_obito" = "ano", "uf" = "sigla_uf")) 


# Calculo taxa de desemprego por Admissões no Caged
df_conso_norm <- df_conso_norm %>%
  mutate(
    pea_18_anos_ou_mais = as.numeric(pea_18_anos_ou_mais),
    h_pea_18_anos_ou_mais = as.numeric(h_pea_18_anos_ou_mais),
    f_pea_18_anos_ou_mais = as.numeric(f_pea_18_anos_ou_mais),
    tx_desemp_caged_ambos = (taxa_des/ pea_18_anos_ou_mais)*100000,
    tx_desemp_caged_mascu = (taxa_des/h_pea_18_anos_ou_mais)*100000,
    tx_desemp_caged_femini = (taxa_des/f_pea_18_anos_ou_mais)*100000
  )


saveRDS(df_conso_norm, "df_conso_norm.RDS")
## Limpando memória
rm(IPCA_2012_2021,df_consol_final,IPCA_2007_2021,pib_2007_2020,saldo_emprego_caged)

###########################
df_conso_n_test <- df_conso_norm 
  # filter(ano_obito <= 2015)

df_test <- df_conso_n_test %>% 
  select(all_of(colunas_unicas),-na, -suic_rate_masc,-suic_rate_fema,
         -tx_desemp_caged_femini,-tx_desemp_caged_mascu)

df_test <- df_test %>%
  mutate_at(vars(-uf, -estados, -deaths), as.double)
df_ambos_test <- df_test %>%
  rename(
    tx_desem = tx_desemp_caged_ambos,
    hope = esperanca_de_vida_ao_nascer,
    ivs_infra = ivs_infraestrutura_urbana,
    idhm_educ = idhm_educacao,
    gini = indice_de_gini,
    ano = ano_obito)

data_describ_variable_teste <- df_ambos_test %>%
  select(hope, gini, idhm_educ, ivs_infra, tx_desem, tx_div, pop_65) %>%
  summarise_all(~list(Média = round(mean(., na.rm = TRUE), 2),
                      Mediana = round(median(., na.rm = TRUE), 2),
                      Mínimo = round(min(., na.rm = TRUE), 2),
                      Máximo = round(max(., na.rm = TRUE), 2),
                      DesvioPadrao = round(sd(., na.rm = TRUE), 2))) %>%
  unnest(cols = everything())

kable(data_describ_variable_teste, format = "latex", booktabs = TRUE,
      caption = "Estatísticas Descritivas das Variáveis Selecionadas")
