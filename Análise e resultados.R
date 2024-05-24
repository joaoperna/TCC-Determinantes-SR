######################################## Script análise e resultados ###############################
#Ambiente
rm(list=ls(all=TRUE))
options(scipen=999)

#Bibliotecas
library(tidyverse)
library(stargazer)
library(psych)
library(stats)
library(fBasics)
library(knitr)
library(writexl)
library(plm)
library(corrplot)
library(car)
library(lmtest)
library(xtable)

# Carregando dataset
df_conso_norm <- readRDS("df_conso_norm.RDS")

# Read the dataset and compute average rates
average_rates <- df_conso_norm %>%
  select(uf, ano_obito, suic_rate_masc, suic_rate_total, suic_rate_fema) %>%
  group_by(ano_obito) %>%
  summarise(
    suic_rate_masc = mean(suic_rate_masc),
    suic_rate_total = mean(suic_rate_total),
    suic_rate_fema = mean(suic_rate_fema)
  )

# Criando o gráfico para a Evolução da Taxa de suicídio 
ggplot(average_rates, aes(x = ano_obito)) +
  geom_line(aes(y = suic_rate_masc, linetype = "Masculino", color = "Masculino"), size = 1, alpha = 0.7) +
  geom_line(aes(y = suic_rate_total, linetype = "Ambos os Sexos", color = "Ambos os Sexos"), size = 1, alpha = 0.7) +
  geom_line(aes(y = suic_rate_fema, linetype = "Feminino", color = "Feminino"), size = 1, alpha = 0.7) +
  labs(
    #title = "Evolução da Taxa de Suicídio no Brasil entre 2012 e 2021"
    x = "", y = "Taxa de Suicídio") +  # Removendo o nome do eixo x
  scale_color_manual(values = c("Masculino" = "black", "Ambos os Sexos" = "black", "Feminino" = "grey")) +
  scale_linetype_manual(values = c("Masculino" = "solid", "Ambos os Sexos" = "dashed", "Feminino" = "solid")) +
  theme_minimal() +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = "")) +
  theme(legend.position = "bottom", 
        plot.caption = element_text(family = "Times New Roman", size = 12, face = "italic", hjust = 0),  
        text = element_text(family = "Times New Roman", size = 12)) + 
  ylim(0, 13)

# Criando tabela de Análise descritiva da taxa de suicídio por genero
df_conso_norm %>%
  select(suic_rate_masc, suic_rate_total, suic_rate_fema) %>% 
  summary()

df_conso_norm %>%
  select(uf, ano_obito, suic_rate_masc, suic_rate_total, suic_rate_fema) %>% 
  summary()



# Agrupado por estado 
uf_stats_summary_sr <- df_conso_norm %>%
  group_by(uf) %>%
  summarise(
    Média  = round(mean(suic_rate_total, na.rm = TRUE), 2),
    Máximo  = round(max(suic_rate_total, na.rm = TRUE), 2),
    Mínimo  = round(min(suic_rate_total, na.rm = TRUE), 2),
    Mediana  = round(median(suic_rate_total, na.rm = TRUE), 2),
    DesvioPadrao  = round(sd(suic_rate_total, na.rm = TRUE), 2),
    Coeficiente_de_Variacao = round(sd(suic_rate_total, na.rm = TRUE) / mean(suic_rate_total, na.rm = TRUE), 2)
  )

kable(uf_stats_summary_sr, format = "latex", booktabs = TRUE,
      caption = "Estatísticas Descritivas por Estado")

# Boxlot para regiões
# Create the state_region_mapping dataset
state_region_mapping <- data.frame(
  uf = c("AC", "RR", "AP", "AM", "PA", "RO", "TO", 
            "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
            "DF", "GO", "MT", "MS", 
            "MG", "ES", "RJ", "SP",
            "PR", "SC", "RS"),
  regiao = c(rep("Norte", 7), rep("Nordeste", 9), rep("Centro-Oeste", 4), rep("Sudeste", 4), rep("Sul", 3))
)

boxplott_data <- df_conso_norm %>%
  left_join(state_region_mapping, by = "uf") %>%
  group_by(regiao, ano_obito)

ggplot(boxplott_data, aes(x = regiao, y = suic_rate_total, fill = regiao)) +
  geom_boxplot() +
  labs(#title = "Boxplot da média da taxa de suicídio por região e ano",
       #x = "Regiões",
       y = "Taxa de suicídio",
       #fill = "Legenda"
       ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
         legend.position = "none"
    #,plot.title = element_text(hjust = 0.5)
    ) +
  xlab("")

# Scatterplots
average_rates_desemp <- df_conso_norm %>%
  select(uf, ano_obito, suic_rate_total, taxa_des) %>%
  group_by(uf) %>%
  summarise(
    suic_rate_total = mean(suic_rate_total),
    taxa_des = mean(taxa_des))


ggplot(average_rates_desemp, aes(x = taxa_des, y = suic_rate_total)) +
  geom_point() +
  geom_text(aes(label = uf), check_overlap = TRUE, vjust = -0.5) + # Move label here
  geom_smooth(method = "lm", color = "black", se = FALSE) + # Trend line
  labs(x = "Taxa de Desemprego",
       y = "Taxa de Suicídio") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        ) +
  ylim(2, 21)


### Separarantdo e 3 datasets distindos por genero

df_male <- df_conso_norm %>% 
  select(estados,uf,ano_obito, male , pib, tx_desemp_caged_mascu , suic_rate_masc,starts_with("dh"),starts_with("m"),starts_with("h")) %>% 
  rename(ano = ano_obito)


df_female <- df_conso_norm %>% 
  select(estados,uf,ano_obito, female, tx_desemp_caged_femini, suic_rate_fema, pib,starts_with("dm"),starts_with("f")) %>% 
  rename(ano = ano_obito)


# Criando um dataset para ambos os sexos
# Colunas em df_male e df_female
colunas_male_female <- c( 
                         names(df_conso_norm)[startsWith(names(df_conso_norm), "dh")], 
                         names(df_conso_norm)[startsWith(names(df_conso_norm), "m")], 
                         names(df_conso_norm)[startsWith(names(df_conso_norm), "dm")], 
                         names(df_conso_norm)[startsWith(names(df_conso_norm), "f")],
                         names(df_conso_norm)[startsWith(names(df_conso_norm), "h")])

# Identificar colunas que NÃO estão em df_male e df_female
colunas_unicas <- setdiff(names(df_conso_norm), colunas_male_female)

# Criar o novo dataframe com colunas únicas
df_ambos <- df_conso_norm %>% 
  select(all_of(colunas_unicas),-na, -suic_rate_masc,-suic_rate_fema,
         -total_saldo_movimentacao_female,-total_saldo_movimentacao_male,
         -tx_desemp_caged_femini,-tx_desemp_caged_mascu)

########################## Construção da análise econométrica

# Filtrando o Dataset de ambos os sexospara as regressões
df_ambos <- df_ambos %>%
  mutate_at(vars(-uf, -estados, -deaths), as.double)
df_ambos_filter <- df_ambos %>%
  rename(
         tx_desem = tx_desemp_caged_ambos,
         hope = esperanca_de_vida_ao_nascer,
         ivs_infra = ivs_infraestrutura_urbana,
         idhm_educ = idhm_educacao,
         gini = indice_de_gini,
         ano = ano_obito)


# Filtrando o dataset feminino para as regressões

df_female <- df_female %>%
  mutate_at(vars(-uf, -estados, -female), as.double)
df_female_filter <- df_female %>% 
  rename(hope = dm_esperanca_de_vida_ao_nascer,
    gini = dm_indice_de_gini,
    idhm_educ = dm_idhm_educacao,
    ivs_infra = f_ivs_infraestrutura_urbana,
    tx_desem = tx_desemp_caged_femini,
    tx_div = f_tx_div,
    pop_65 = f_pop_65,
    )

# Filtrando o dataset Masculino para as regressões
df_male <- df_male %>%
  mutate_at(vars(-uf, -estados, -male), as.double)
df_male_filter <- df_male %>%
  rename(
    hope = dh_esperanca_de_vida_ao_nascer,
    gini = dh_indice_de_gini,
    idhm_educ = dh_idhm_educacao,
    ivs_infra = h_ivs_infraestrutura_urbana,
    tx_desem =  tx_desemp_caged_mascu,
    tx_div = h_tx_div,
    pop_65 = h_pop_65)

## Calculando Análise descritiva das variáveis explicativas
# Para ambos os sexos
data_describ_variable_ambos <- df_ambos_filter %>%
  select(hope, gini, idhm_educ, ivs_infra, tx_desem, tx_div, pop_65) %>%
  summarise_all(~list(Média = round(mean(., na.rm = TRUE), 2),
                      Máximo = round(max(., na.rm = TRUE), 2),
                      Mínimo = round(min(., na.rm = TRUE), 2),
                      Mediana = round(median(., na.rm = TRUE), 2),
                      DesvioPadrao = round(sd(., na.rm = TRUE), 2))) %>%
  unnest(cols = everything())

# Formatar a tabela em LaTeX usando knitr
kable(data_describ_variable_ambos, format = "latex", booktabs = TRUE,
      caption = "Estatísticas Descritivas das Variáveis Selecionadas")

# Para o sexo masculino 
data_describ_variable_male <- df_male_filter %>%
  select(hope, gini, idhm_educ, ivs_infra, tx_desem, tx_div, pop_65) %>%
  summarise_all(~list(Média = round(mean(., na.rm = TRUE), 2),
                      Máximo = round(max(., na.rm = TRUE), 2),
                      Mínimo = round(min(., na.rm = TRUE), 2),
                      Mediana = round(median(., na.rm = TRUE), 2),
                      DesvioPadrao = round(sd(., na.rm = TRUE), 2))) %>%
  unnest(cols = everything())
# Formatar a tabela em LaTeX usando knitr
kable(data_describ_variable_male, format = "latex", booktabs = TRUE,
      caption = "Estatísticas Descritivas das Variáveis Selecionadas")

# Para o sexo feminino 
data_describ_variable_female <- df_female_filter %>%
  select(hope, gini, idhm_educ, ivs_infra, tx_desem, tx_div, pop_65) %>%
  summarise_all(~list(Média = round(mean(., na.rm = TRUE), 2),
                      Máximo = round(max(., na.rm = TRUE), 2),
                      Mínimo = round(min(., na.rm = TRUE), 2),
                      Mediana = round(median(., na.rm = TRUE), 2),
                      DesvioPadrao = round(sd(., na.rm = TRUE), 2))) %>%
  unnest(cols = everything())
# Formatar a tabela em LaTeX usando knitr
kable(data_describ_variable_female, format = "latex", booktabs = TRUE,
      caption = "Estatísticas Descritivas das Variáveis Selecionadas")

##################
# Para análise da taxa de divórcio 
div_ds_mask <- df_conso_norm %>% 
  select(uf, ano_obito, tx_div, h_tx_div, f_tx_div , div, populacao_de_18_anos_ou_mais, 
         h_populacao_de_18_anos_ou_mais, f_populacao_de_18_anos_ou_mais)



#######

#Matriz de corelação Ambos os sexos
cor_matrix_ambos <- cor(df_ambos_filter[, c("suic_rate_total",  "hope", "gini", "idh_educ", "ivs_infra", "tx_desem", "tx_div","pop_65")])
print(cor_matrix_ambos)
latex_table <- xtable(cor_matrix_ambos)
print(latex_table, type = "latex", floating = FALSE)
corrplot(cor_matrix_ambos, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", # Adicionar o coeficiente de correlação
         tl.col = "black", tl.srt = 45, # Ajustar a cor e a rotação do texto dos rótulos
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Gradiente de cores
         #title = "Matriz de Correlação dos Indicadores Sociais", # Adicionando um título
         cl.lim = c(-1, 1), # Definindo os limites para a escala de cores
         cl.cex = 0.75, # Ajustando o tamanho da fonte da legenda de cores
         cl.ratio = 0.1 # Ajustando a proporção da legenda de cores
         #mar = c(0, 0, 1, 0) # Ajustando as margens (abaixo, esquerda, acima, direita)
)

#Matriz de corelação masuclina
cor_matrix_mascu <- cor(df_male_filter[, c("suic_rate_masc",  "hope", "gini", "idh_educ", "ivs_infra", "tx_desem", "tx_div","pop_65")])
print(cor_matrix_mascu)
latex_table <- xtable(cor_matrix_mascu)
print(latex_table, type = "latex", floating = FALSE)

#Matriz de corelação Feminina
cor_matrix_female <- cor(df_female_filter[, c("suic_rate_fema",  "hope", "gini", "idh_educ", "ivs_infra", "tx_desem", "tx_div","pop_65")])
print(cor_matrix_female)
latex_table <- xtable(cor_matrix_female)
print(latex_table, type = "latex", floating = FALSE)

################################## Regressões #################################
# Regrão dos 3 modelos em MQO
suic_ambos_lm_mod <- lm(suic_rate_total ~ hope + gini + idh_educ + ivs_infra 
                        + tx_desem +tx_div, 
                        data = df_ambos_filter)
suic_female_lm_mod <- lm(suic_rate_fema ~ hope + gini + idh_educ + ivs_infra 
                         + tx_desem +tx_div, data = df_female_filter)
suic_male_lm_mod <- lm(suic_rate_masc ~ hope + gini + idh_educ + ivs_infra 
                       + tx_desem +tx_div, data = df_male_filter)

# Regressão usando plm ambos os sexos

# MQO usando plm
pooled_ambos <- plm(suic_rate_total ~ hope + gini + idh_educ + ivs_infra 
                    + tx_desem +tx_div, 
                    data = df_ambos_filter,
                    model = "pooling",
                    index = c("uf","ano"))

summary(pooled_ambos)

# Random Effect Model 

re_ambos <- plm(suic_rate_total ~ hope + gini + idh_educ + ivs_infra 
                + tx_desem +tx_div, 
                data = df_ambos_filter, 
                model = "random"
                )

summary(re_ambos)

# Fixed Effect Model por uf
fe_ambos <- plm(suic_rate_total ~ hope + gini + idh_educ + ivs_infra 
                + tx_desem +tx_div, 
                data = df_ambos_filter, 
                model = "within",
                index = c("uf")
                )

summary(fe_ambos)

# Fixed Effect Model por uf Incluindo a variável pop_65+

fe_65_ambos <- plm(suic_rate_total ~ hope + gini + idh_educ + ivs_infra 
                   + tx_desem +tx_div + pop_65, 
                data = df_ambos_filter, 
                model = "within",
                index = c("uf")
                )

summary(fe_65_ambos)

# Fixed Effect Model twoways por uf Incluindo a variável pop_65+

fe_65_tw_ambos <- plm(suic_rate_total ~ hope + gini + idh_educ + ivs_infra 
                      + tx_desem +tx_div + pop_65, 
                   data = df_ambos_filter, 
                   model = "within",
                   index = c("uf","ano"),
                   effect = "twoways")

summary(fe_65_tw_ambos)

# pf test ambos
pFtest(pooled_ambos,fe_ambos)

# Ph test
phtest(fe_ambos,re_ambos)

# Check to see if Panel Effects exist in data
plmtest(pooled_ambos, type=c("bp"))

# Test for heteroscedasticity
# bptest(suic_rate_total~hope+renda_per_capita+gini+idh_educ+idhm+subindice+
         # ivs_infra+salario_mini+t_desc_18+taxa_analf_18+perc_ocup_18+lpib_last_5+
         # pop_65+tx_div+pop_jov, data = df_ambos_filter, studentize = F)

##### Regressão usando plm o sexo masculino  ######


# MQO usando plm
pooled_mascu <- plm(suic_rate_masc ~ hope + gini + idh_educ + ivs_infra 
                    + tx_desem +tx_div, 
                    data = df_male_filter, 
                    model = "pooling",
                    index = c("uf","ano")
                    )
summary(pooled_mascu)

# Random Effect Model 

re_mascu <- plm(suic_rate_masc ~ hope + gini + idh_educ + ivs_infra 
                + tx_desem + tx_div, 
                data = df_male_filter, 
                model = "random")

summary(re_mascu)


# Fixed Effect Model
fe_mascu <- plm(suic_rate_masc ~ hope + gini + idh_educ + ivs_infra 
                + tx_desem + tx_div, 
                 data = df_male_filter, 
                 model = "within",
                index = c("uf")
                )
summary(fe_mascu)

# Fixed Effect Model por uf Incluindo a variável pop_65+

fe_65_mascu <- plm(suic_rate_masc ~ hope + gini + idh_educ + ivs_infra 
                   + tx_desem + tx_div + pop_65, 
                   data = df_male_filter, 
                   model = "within",
                   index = c("uf")
    )

summary(fe_65_mascu)

# Fixed Effect Model twoways por uf Incluindo a variável pop_65+

fe_65_tw_mascu <- plm(suic_rate_masc ~ hope + gini + idh_educ + ivs_infra 
                      + tx_desem + tx_div + pop_65, 
                      data = df_male_filter, 
                      model = "within",
                      index = c("uf","ano"),
                      effect = "twoways")

summary(fe_65_tw_mascu)


# pf test ambos
pFtest(pooled_mascu,fe_mascu)


# Ph test
phtest(fe_mascu,re_mascu)

# Check to see if Panel Effects exist in data
plmtest(pooled_mascu, type=c("bp"))

# Test for heteroscedasticity
# bptest(suic_rate_masc~hope+renda_per_capita+gini+idh_educ+idhm+subindice+
#          ivs_infra+salario_mini+t_desc_18+taxa_analf_18+perc_ocup_18+lpib_last_5+
#          pop_65+tx_div+pop_jov, data = df_male_filter, studentize = F)

####  Regressão usando plm o sexo feminino #############

# MQO usando plm
pooled_femi <- plm(suic_rate_fema ~ hope + gini + idh_educ + ivs_infra 
                   + tx_desem + tx_div, 
                   data = df_female_filter, 
                   model = "pooling",
                   index = c("uf","ano"))

summary(pooled_femi)


# Random Effect Model 

re_femi <- plm(suic_rate_fema ~ hope + gini + idh_educ + ivs_infra 
               + tx_desem + tx_div, 
               data = df_female_filter, 
               model = "random")

summary(re_femi)

# Fixed Effect Model
fe_femi <- plm(suic_rate_fema ~ hope + gini + idh_educ + ivs_infra 
               + tx_desem + tx_div, 
               data = df_female_filter, 
               model = "within",
               index = c("uf")
               )

summary(fe_femi)

# Fixed Effect Model por uf Incluindo a variável pop_65+

fe_65_femi <- plm(suic_rate_fema  ~ hope + gini + idh_educ + ivs_infra 
                   + tx_desem + tx_div + pop_65, 
                   data = df_female_filter, 
                   model = "within",
                   index = c("uf")
)

summary(fe_65_femi)

# Fixed Effect Model twoways por uf Incluindo a variável pop_65+

fe_65_tw_femi <- plm(suic_rate_fema ~ hope + gini + idh_educ + ivs_infra 
                     + tx_desem + tx_div + pop_65, 
                      data = df_female_filter, 
                      model = "within",
                      index = c("uf","ano"),
                      effect = "twoways")

summary(fe_65_tw_femi)



# Ph test
phtest(fe_femi,re_femi)

# Check to see if Panel Effects exist in data
plmtest(pooled_femi, type=c("bp"))

# Test for heteroscedasticity
# bptest(suic_rate_fema~hope+renda_per_capita+gini+idh_educ+idhm+subindice+
         # ivs_infra+salario_mini+t_desc_18+taxa_analf_18+perc_ocup_18+lpib_last_5+
         # pop_65+tx_div+pop_jov, data = df_female_filter, studentize = F)


# Estruturando Erros Clusterizados para todos os modelos
rob_se_ambos <- list(sqrt(diag(vcovHC(pooled_ambos, type = "HC1"))),
                     sqrt(diag(vcovHC(re_ambos, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_ambos, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_65_ambos, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_65_tw_ambos, type = "HC1")))
                     )
rob_se_mascu <- list(sqrt(diag(vcovHC(pooled_mascu, type = "HC1"))),
                     sqrt(diag(vcovHC(re_ambos, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_mascu, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_65_mascu, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_65_tw_mascu, type = "HC1")))
                     )
rob_se_femi <- list(sqrt(diag(vcovHC(pooled_femi, type = "HC1"))),
                     sqrt(diag(vcovHC(re_femi, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_femi, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_65_femi, type = "HC1"))),
                     sqrt(diag(vcovHC(fe_65_tw_femi, type = "HC1")))
                    )


### Criação da tabela para as tabelas

# Ambos os sexos com erros robusto em html para test
stargazer(pooled_ambos,re_ambos,fe_ambos,fe_65_ambos,fe_65_tw_ambos, 
          #title= "",
          #dep.var.caption = "",
          #covariate.labels = c("",""),
          #notes.label = "",
          type = "html",
          se = rob_se_ambos,
          out="C:/Users/João Perna/Desktop/Backlog TCC/Scripts TCC/Script atual de progresso/asg.htm")


# Ambos os sexos com erros robustos
stargazer(pooled_ambos,re_ambos,fe_ambos,fe_65_ambos,fe_65_tw_ambos,
          #title= "",
          #dep.var.caption = "",
          #covariate.labels = c("",""),
          #notes.label = "",
          se = rob_se_ambos,
          type = "latex")


# Masculino com erros robustos
stargazer(pooled_mascu,re_mascu,fe_mascu,fe_65_mascu,fe_65_tw_mascu,
          #title= "",
          #dep.var.caption = "",
          #covariate.labels = c("",""),
          #notes.label = "",
          se = rob_se_mascu,
          type = "latex")
# Femininosem erros robustos
stargazer(pooled_femi,re_femi,fe_femi,fe_65_femi,fe_65_tw_femi,
          #title= "",
          #dep.var.caption = "",
          #covariate.labels = c("",""),
          #notes.label = "",
          se = rob_se_femi,
          type = "latex")
