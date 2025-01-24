library(googlesheets4)
library(googledrive)
library(dplyr)
library(purrr)

source("config.R")

criar_aba <- function(aluno, planilha) {
  
  # Carregar template
  template = data.frame(Aluno = aluno)
  
  # Escrever template da orientação
  sheet_write(template, planilha, sheet = aluno)
}

criar_planilha <- function(orientador, dados, pasta_id) {
  
  # Criar nova planilha para o orientador
  planilha <- gs4_create(paste(orientador, " - Avaliação orientações", collapse = ""), sheets = NULL)
  
  # Escrever os dados dos orientandos na aba Alunos
  sheet_write(dados, planilha, sheet = "Alunos")
  
  # Remover aba default
  sheet_delete(planilha, sheet = "Página1")
  
  # Mover planilha para diretório
  drive_mv(planilha, path = as_id(pasta_id))
  
  # Criar abas para os orientandos
  dados %>% group_walk(~ {walk2(.x$`Aluno(a)`, planilha, criar_aba)}) 
}

# Autenticar no Google Sheets
gs4_deauth()
gs4_auth(scopes = scope)

# Ler planilha
dados <- read_sheet(planilha_id, sheet = 1)  

# Selecionar orientador principal
dados <- dados %>% rowwise() %>%
  select(`Matrícula`, `Aluno(a)`, `Nível`, `Tipo de Defesa`, `Orientador(es)`) %>% 
  filter(`Tipo de Defesa` %in% c("DISSERTAÇÃO", "TESE")) %>%
  mutate(orientador_principal = strsplit(`Orientador(es)`, split = ",")[[1]][1]) %>%
  arrange(orientador_principal) %>% 
  unique()

# Remover arquivos antigos
# arquivos <- drive_ls(path = as_id(pasta_id))
# drive_rm(arquivos)

# Selcionar orientadores principais
orientadores <- sort(unique(dados$orientador_principal))

# Iterar sobre os orientadores principais
for (orientador in orientadores) {
  
  # Filtrar os dados do orientador
  dados_orientador <- filter(dados, orientador_principal == orientador) %>%
    select(-`Orientador(es)`, -orientador_principal) %>% 
    arrange(`Aluno(a)`)
  
  criar_planilha(orientador, dados_orientador, pasta_id) 
}
