library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggrepel)

df <- readRDS("nomes_lista")

# 1. Tratamento Inicial e Limpeza de Datas
df_bruto <- bind_rows(df) %>% 
  unnest_wider(res)

# 2. TRATAMENTO DE DATAS
df_arrumado <- df_bruto %>%
  mutate(
    frequencia = as.numeric(frequencia),
    
    #Extrai apemas os primeiros 4 dígitos que encontrar
    primeiro_ano_encontrado = as.numeric(str_extract(periodo, "\\d{4}")),
    
    #Identifica se é o caso especial "1930[" (sem vírgula antes)
    eh_caso_especial = str_detect(periodo, "^\\d"),

    ano_inicio = case_when(
      eh_caso_especial ~ NA_real_,           # Caso "1930[": Início indefinido
      TRUE ~ primeiro_ano_encontrado         # Caso "[1930,1940[": O início é 1930
    ),
    
    ano_fim = case_when(
      eh_caso_especial ~ primeiro_ano_encontrado - 1, # Caso "1930[": Fim é 1929
      TRUE ~ primeiro_ano_encontrado + 9          # Caso "[1930,1940[": Fim é 1939
    )
  ) %>%
  select(-primeiro_ano_encontrado, -eh_caso_especial) # Remove colunas de lógica booleana

# 2. Criação das Métricas de Análise
df_final <- df_arrumado %>%
  group_by(nome) %>%
  arrange(ano_fim) %>%
  mutate(
    dif_abs = frequencia - lag(frequencia, default = 0),
    
    # Proteção contra divisão por zero ou NA no primeiro registro
    var_pct = (frequencia - lag(frequencia, default = 0)) / lag(frequencia),
    var_pct = scales::number(var_pct, accuracy = 0.0001, big.mark = ".", decimal.mark = ",")
  ) %>%
  ungroup() %>%
  
  # 3. Ranking por Década
  group_by(ano_fim) %>%
  mutate(
    rank_decada = dense_rank(desc(frequencia))
  ) %>%
  ungroup()

df_final <- df_final %>% 
  mutate(frequencia = as.numeric(frequencia))

# Proporção do nome no data frame
df_final <- df_final %>% 
  group_by(nome) %>% 
  mutate(prop = frequencia / sum(frequencia),
         prop = scales::number(prop, accuracy = 0.0001)   
  )

# Proporção do nome por decada
df_final <- df_final %>% 
  group_by(ano_inicio) %>% 
  mutate(prop_decada = frequencia / sum(frequencia),
         prop_decada = scales::number(prop_decada, accuracy = 0.0001)   
  )

saveRDS(df_final, "bdTratado.rds")
