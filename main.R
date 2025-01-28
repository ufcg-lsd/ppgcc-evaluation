library(googlesheets4)
library(googledrive)
library(dplyr)
library(purrr)
library(httr)

source("config.R")

criar_aba <- function(aluno, dados, template, planilha) {
  
  # Editar o template
  template[1,2] <- aluno
  template[3,2] <- dados[[1]]
  template[4,2] <- dados[[2]]
  
  
  # Escrever template da orientação
  sheet_write(template, planilha, sheet = aluno)
}

criar_planilha <- function(orientador, dados, pasta_id, planilha_id, template) {
  
  # Criar nova planilha para o orientador
  planilha <- gs4_create(paste(orientador, " - Avaliação orientações", collapse = ""), sheets = NULL)
  
  # Escrever os dados dos orientandos na aba Alunos
  sheet_write(dados, planilha, sheet = "Orientações")
  
  # Remover aba default
  sheet_delete(planilha, sheet = "Página1")
  
  # Mover planilha para diretório
  drive_mv(planilha, path = as_id(pasta_id))
  
  # Criar abas para os orientandos
  dados %>% group_walk(~ {walk2(
    alunos <- .x$`Aluno(a)`, 
    niveis_datas <- list(.x$Nível, .x$`Data da Defesa`), 
    criar_aba(alunos, niveis_datas, template, planilha))}) 
}

# Autenticar no Google Sheets
gs4_deauth()
gs4_auth(scopes = scope)

# Ler planilha
dados <- read_sheet(planilha_id, sheet = 1)

# Ler planilha template
dados_template <-read_sheet(template_id, sheet = 1)
colnames(dados_template) <- c("Planilha de avaliação discente", "", "", "")

# Selecionar orientador principal
dados <- dados %>% rowwise() %>%
  select(`Matrícula`, `Aluno(a)`, `Nível`, `Data da Defesa`, `Tipo de Defesa`, `Orientador(es)`) %>% 
  filter(`Tipo de Defesa` %in% c("DISSERTAÇÃO", "TESE")) %>%
  mutate(orientador_principal = strsplit(`Orientador(es)`, split = ",")[[1]][1]) %>%
  arrange(orientador_principal) %>% 
  unique()

# Remover arquivos antigos
arquivos <- drive_ls(path = as_id(pasta_id))
drive_rm(arquivos)

# Selcionar orientadores principais
orientadores <- sort(unique(dados$orientador_principal))

# Iterar sobre os orientadores principais
for (orientador in orientadores) {
  
  # Filtrar os dados do orientador
  dados_orientador <- filter(dados, orientador_principal == orientador) %>%
    select(-`Orientador(es)`, -orientador_principal) %>% 
    arrange(`Aluno(a)`)
  
  criar_planilha(orientador, dados_orientador, pasta_id, planilha_id, dados_template) 
}