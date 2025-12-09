library(dplyr)
library(shiny)
library(DT)

# Define o diretório base -------------------------------------------------
base_dir <- "C:/Users/6044/Desktop/sigtap"

# Basta colocar o arquivo ZIP com a base atualizada na subpasta -----------
# Encontra a subpasta que começa com "TabelaUnificada_" -------------------
path <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
path <- path[grepl("^TabelaUnificada_", basename(path))]

# Define o diretório de trabalho e exibe mensagem -------------------------
setwd(path)
message("Diretório de trabalho definido como: ", path)

# Leitura e conversão -----------------------------------------------------
tb_grupo <- iconv(readLines("tb_grupo.txt", warn = FALSE), "latin1", "UTF-8")
tb_sub_grupo <- iconv(readLines("tb_sub_grupo.txt", warn = FALSE), "latin1", "UTF-8")
tb_forma_organizacao <- iconv(readLines("tb_forma_organizacao.txt", warn = FALSE), "latin1", "UTF-8")
tb_procedimento <- iconv(readLines("tb_procedimento.txt", warn = FALSE), "latin1", "UTF-8")
tb_financiamento <- iconv(readLines("tb_financiamento.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_compativel <- iconv(readLines("rl_procedimento_compativel.txt", warn = FALSE), "latin1", "UTF-8")
rl_excecao_compatibilidade <- iconv(readLines("rl_excecao_compatibilidade.txt", warn = FALSE), "latin1", "UTF-8")
tb_cid <- iconv(readLines("tb_cid.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_cid <- iconv(readLines("rl_procedimento_cid.txt", warn = FALSE), "latin1", "UTF-8")
tb_detalhe <- iconv(readLines("tb_detalhe.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_detalhe <- iconv(readLines("rl_procedimento_detalhe.txt", warn = FALSE), "latin1", "UTF-8")
tb_habilitacao <- iconv(readLines("tb_habilitacao.txt", warn = FALSE), "latin1", "UTF-8")
tb_grupo_habilitacao <- iconv(readLines("tb_grupo_habilitacao.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_habilitacao <- iconv(readLines("rl_procedimento_habilitacao.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_incremento <- iconv(readLines("rl_procedimento_incremento.txt", warn = FALSE), "latin1", "UTF-8")
tb_tipo_leito <- iconv(readLines("tb_tipo_leito.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_leito <- iconv(readLines("rl_procedimento_leito.txt", warn = FALSE), "latin1", "UTF-8")
tb_modalidade <- iconv(readLines("tb_modalidade.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_modalidade <- iconv(readLines("rl_procedimento_modalidade.txt", warn = FALSE), "latin1", "UTF-8")
tb_registro <- iconv(readLines("tb_registro.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_registro <- iconv(readLines("rl_procedimento_registro.txt", warn = FALSE), "latin1", "UTF-8")
tb_servico <- iconv(readLines("tb_servico.txt", warn = FALSE), "latin1", "UTF-8")
tb_servico_classificacao <- iconv(readLines("tb_servico_classificacao.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_servico <- iconv(readLines("rl_procedimento_servico.txt", warn = FALSE), "latin1", "UTF-8")
tb_ocupacao <- iconv(readLines("tb_ocupacao.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_ocupacao <- iconv(readLines("rl_procedimento_ocupacao.txt", warn = FALSE), "latin1", "UTF-8")
tb_rubrica <- iconv(readLines("tb_rubrica.txt", warn = FALSE), "latin1", "UTF-8")
tb_sia_sih <- iconv(readLines("tb_sia_sih.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_sia_sih <- iconv(readLines("rl_procedimento_sia_sih.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_origem <- iconv(readLines("rl_procedimento_origem.txt", warn = FALSE), "latin1", "UTF-8")
tb_descricao <- iconv(readLines("tb_descricao.txt", warn = FALSE), "latin1", "UTF-8")
tb_descricao_detalhe <- iconv(readLines("tb_descricao_detalhe.txt", warn = FALSE), "latin1", "UTF-8")
tb_regra_condicionada <- iconv(readLines("tb_regra_condicionada.txt", warn = FALSE), "latin1", "UTF-8")
rl_procedimento_regra_cond <- iconv(readLines("rl_procedimento_regra_cond.txt", warn = FALSE), "latin1", "UTF-8")

# Data frames -------------------------------------------------------------
tb_grupo.ds <- data.frame(
  co_grupo       = substr(tb_grupo, 1, 2),
  no_grupo       = substr(tb_grupo, 3, 102),
  dt_competencia = substr(tb_grupo, 103, 108)
)

tb_sub_grupo.ds <- data.frame(
  co_grupo       = substr(tb_sub_grupo, 1, 2),
  co_sub_grupo   = substr(tb_sub_grupo, 3, 4),
  no_sub_grupo   = substr(tb_sub_grupo, 5, 104),
  dt_competencia = substr(tb_sub_grupo, 105, 110)
)

tb_forma_organizacao.ds <- data.frame(
  co_grupo             = substr(tb_forma_organizacao, 1, 2),
  co_sub_grupo         = substr(tb_forma_organizacao, 3, 4),
  co_forma_organizacao = substr(tb_forma_organizacao, 5, 6),
  no_forma_organizacao = substr(tb_forma_organizacao, 7, 106),
  dt_competencia       = substr(tb_forma_organizacao, 107, 112)
)

tb_procedimento.ds <- data.frame(
  co_procedimento      = substr(tb_procedimento, 1, 10),
  no_procedimento      = substr(tb_procedimento, 11, 260),
  tp_complexidade      = substr(tb_procedimento, 261, 261),
  tp_sexo              = substr(tb_procedimento, 262, 262),
  qt_maxima_execucao   = as.numeric(substr(tb_procedimento, 263, 266)),
  qt_dias_permanencia  = as.numeric(substr(tb_procedimento, 267, 270)),
  qt_pontos            = as.numeric(substr(tb_procedimento, 271, 274)),
  vl_idade_minima      = as.numeric(substr(tb_procedimento, 275, 278)),
  vl_idade_maxima      = as.numeric(substr(tb_procedimento, 279, 282)),
  vl_sh                = as.numeric(substr(tb_procedimento, 285, 294)) / 100,
  vl_sa                = as.numeric(substr(tb_procedimento, 297, 306)) / 100,
  vl_sp                = as.numeric(substr(tb_procedimento, 309, 318)) / 100,
  co_financiamento     = substr(tb_procedimento, 319, 320),
  co_rubrica           = substr(tb_procedimento, 321, 326),
  dt_competencia       = substr(tb_procedimento, 331, 336)
)

tb_financiamento.ds <- data.frame(
  co_financiamento = substr(tb_financiamento, 1, 2),
  no_financiamento = substr(tb_financiamento, 3, 102),
  dt_competencia   = substr(tb_financiamento, 103, 108)
)

rl_procedimento_compativel.ds <- data.frame(
  co_procedimento_principal  = substr(rl_procedimento_compativel, 1, 10),
  co_registro_principal      = substr(rl_procedimento_compativel, 11, 12),
  co_procedimento_compativel = substr(rl_procedimento_compativel, 13, 22),
  co_registro_compativel     = substr(rl_procedimento_compativel, 23, 24),
  tp_compatibilidade         = substr(rl_procedimento_compativel, 25, 25),
  qt_permitida               = as.numeric(substr(rl_procedimento_compativel, 26, 29)),
  dt_competencia             = substr(rl_procedimento_compativel, 30, 35)
)

rl_excecao_compatibilidade.ds <- data.frame(
  co_procedimento_restricao  = substr(rl_excecao_compatibilidade, 1, 10),
  co_procedimento_principal  = substr(rl_excecao_compatibilidade, 11, 20),
  co_registro_principal      = substr(rl_excecao_compatibilidade, 21, 22),
  co_procedimento_compativel = substr(rl_excecao_compatibilidade, 23, 32),
  co_registro_compativel     = substr(rl_excecao_compatibilidade, 33, 34),
  tp_compatibilidade         = substr(rl_excecao_compatibilidade, 35, 35),
  dt_competencia             = substr(rl_excecao_compatibilidade, 36, 41)
)

tb_cid.ds <- data.frame(
  co_cid               = substr(tb_cid, 1, 4),
  no_cid               = substr(tb_cid, 5, 104),
  tp_agravo            = substr(tb_cid, 105, 105),
  tp_sexo              = substr(tb_cid, 106, 106),
  tp_estadio           = substr(tb_cid, 107, 107),
  vl_campos_irradiados = as.numeric(substr(tb_cid, 108, 111))
)

rl_procedimento_cid.ds <- data.frame(
  co_procedimento = substr(rl_procedimento_cid, 1, 10),
  co_cid          = substr(rl_procedimento_cid, 11, 14),
  st_principal    = substr(rl_procedimento_cid, 15, 15),
  dt_competencia  = substr(rl_procedimento_cid, 16, 21)
)

tb_detalhe.ds <- data.frame(
  co_detalhe     = substr(tb_detalhe, 1, 3),
  no_detalhe     = substr(tb_detalhe, 4, 103),
  dt_competencia = substr(tb_detalhe, 104, 109)
)

rl_procedimento_detalhe.ds <- data.frame(
  co_procedimento = substr(rl_procedimento_detalhe, 1, 10),
  co_detalhe      = substr(rl_procedimento_detalhe, 11, 13),
  dt_competencia  = substr(rl_procedimento_detalhe, 14, 19)
)

tb_habilitacao.ds <- data.frame(
  co_habilitacao = substr(tb_habilitacao, 1, 4),
  no_habilitacao = substr(tb_habilitacao, 5, 154),
  dt_competencia = substr(tb_habilitacao, 155, 160)
)

tb_grupo_habilitacao.ds <- data.frame(
  nu_grupo_habilitacao = substr(tb_grupo_habilitacao, 1, 4),
  no_grupo_habilitacao = substr(tb_grupo_habilitacao, 5, 24),
  ds_grupo_habilitacao = substr(tb_grupo_habilitacao, 25, 274)
)

rl_procedimento_habilitacao.ds <- data.frame(
  co_procedimento      = substr(rl_procedimento_habilitacao, 1, 10),
  co_habilitacao       = substr(rl_procedimento_habilitacao, 11, 14),
  nu_grupo_habilitacao = substr(rl_procedimento_habilitacao, 15, 18),
  dt_competencia       = substr(rl_procedimento_habilitacao, 19, 24)
)

rl_procedimento_incremento.ds <- data.frame(
  co_procedimento  = substr(rl_procedimento_incremento, 1, 10),
  co_habilitacao   = substr(rl_procedimento_incremento, 11, 14),
  vl_percentual_sh = as.numeric(substr(rl_procedimento_incremento, 15, 21)) / 100,
  vl_percentual_sa = as.numeric(substr(rl_procedimento_incremento, 22, 28)) / 100,
  vl_percentual_sp = as.numeric(substr(rl_procedimento_incremento, 29, 35)) / 100,
  dt_competencia   = substr(rl_procedimento_incremento, 36, 41)
)

tb_tipo_leito.ds <- data.frame(
  co_tipo_leito  = substr(tb_tipo_leito, 1, 2),
  no_tipo_leito  = substr(tb_tipo_leito, 3, 62),
  dt_competencia = substr(tb_tipo_leito, 63, 68)
)

rl_procedimento_leito.ds <- data.frame(
  co_procedimento = substr(rl_procedimento_leito, 1, 10),
  co_tipo_leito   = substr(rl_procedimento_leito, 11, 12),
  dt_competencia  = substr(rl_procedimento_leito, 13, 18)
)

tb_modalidade.ds <- data.frame(
  co_modalidade  = substr(tb_modalidade, 1, 2),
  no_modalidade  = substr(tb_modalidade, 3, 102),
  dt_competencia = substr(tb_modalidade, 103, 108)
)

rl_procedimento_modalidade.ds <- data.frame(
  co_procedimento = substr(rl_procedimento_modalidade, 1, 10),
  co_modalidade   = substr(rl_procedimento_modalidade, 11, 12),
  dt_competencia  = substr(rl_procedimento_modalidade, 13, 18)
)

tb_registro.ds <- data.frame(
  co_registro    = substr(tb_registro, 1, 2),
  no_registro    = substr(tb_registro, 3, 52),
  dt_competencia = substr(tb_registro, 53, 58)
)

rl_procedimento_registro.ds <- data.frame(
  co_procedimento = substr(rl_procedimento_registro, 1, 10),
  co_registro     = substr(rl_procedimento_registro, 11, 12),
  dt_competencia  = substr(rl_procedimento_registro, 13, 18)
)

tb_servico.ds <- data.frame(
  co_servico     = substr(tb_servico, 1, 3),
  no_servico     = substr(tb_servico, 4, 123),
  dt_competencia = substr(tb_servico, 124, 129)
)

tb_servico_classificacao.ds <- data.frame(
  co_servico       = substr(tb_servico_classificacao, 1, 3),
  co_classificacao = substr(tb_servico_classificacao, 4, 6),
  no_classificacao = substr(tb_servico_classificacao, 7, 156),
  dt_competencia   = substr(tb_servico_classificacao, 157, 162)
)

rl_procedimento_servico.ds <- data.frame(
  co_procedimento  = substr(rl_procedimento_servico, 1, 10),
  co_servico       = substr(rl_procedimento_servico, 11, 13),
  co_classificacao = substr(rl_procedimento_servico, 14, 16),
  dt_competencia   = substr(rl_procedimento_servico, 17, 22)
)

tb_ocupacao.ds <- data.frame(
  co_ocupacao = substr(tb_ocupacao, 1, 6),
  no_ocupacao = substr(tb_ocupacao, 7, 156)
)

rl_procedimento_ocupacao.ds <- data.frame(
  co_procedimento = substr(rl_procedimento_ocupacao, 1, 10),
  co_ocupacao     = substr(rl_procedimento_ocupacao, 11, 16),
  dt_competencia  = substr(rl_procedimento_ocupacao, 17, 22)
)

tb_rubrica.ds <- data.frame(
  co_rubrica     = substr(tb_rubrica, 1, 6),
  no_rubrica     = substr(tb_rubrica, 7, 106),
  dt_competencia = substr(tb_rubrica, 107, 112)
)

tb_sia_sih.ds <- data.frame(
  co_procedimento_sia_sih = substr(tb_sia_sih, 1, 10),
  no_procedimento_sia_sih = substr(tb_sia_sih, 11, 110),
  tp_procedimento         = substr(tb_sia_sih, 111, 111)
)

rl_procedimento_sia_sih.ds <- data.frame(
  co_procedimento         = substr(rl_procedimento_sia_sih, 1, 10),
  co_procedimento_sia_sih = substr(rl_procedimento_sia_sih, 11, 20),
  tp_procedimento         = substr(rl_procedimento_sia_sih, 21, 21)
)

rl_procedimento_origem.ds <- data.frame(
  co_procedimento        = substr(rl_procedimento_origem, 1, 10),
  co_procedimento_origem = substr(rl_procedimento_origem, 11, 20),
  dt_competencia         = substr(rl_procedimento_origem, 21, 26)
)

tb_descricao.ds <- data.frame(
  co_descricao   = substr(tb_descricao, 1, 10),
  ds_descricao   = substr(tb_descricao, 11, 4010),
  dt_competencia = substr(tb_descricao, 4011, 4016)
)

tb_descricao_detalhe.ds <- data.frame(
  co_detalhe     = substr(tb_descricao_detalhe, 1, 3),
  ds_detalhe     = substr(tb_descricao_detalhe, 4, 4003),
  dt_competencia = substr(tb_descricao_detalhe, 4004, 4009)
)

tb_regra_condicionada.ds <- data.frame(
  co_regra_condicionada = substr(tb_regra_condicionada, 1, 4),
  no_regra_condicionada = substr(tb_regra_condicionada, 5, 154),
  ds_regra_condicionada = substr(tb_regra_condicionada, 155, 4154)
)

rl_procedimento_regra_cond.ds <- data.frame(
  co_procedimento       = substr(rl_procedimento_regra_cond, 1, 10),
  co_regra_condicionada = substr(rl_procedimento_regra_cond, 11, 14)
)

# Recodes -----------------------------------------------------------------

# tb_procedimento
tb_procedimento.ds <- tb_procedimento.ds %>%
  mutate(
    tp_complexidade = recode(tp_complexidade,
                             "0" = "Não se aplica",
                             "1" = "Atenção Básica Complexidade",
                             "2" = "Média Complexidade",
                             "3" = "Alta Complexidade"),
    tp_sexo = recode(tp_sexo,
                     "M" = "Masculino",
                     "F" = "Feminino",
                     "I" = "Indiferente/Ambos",
                     "N" = "Não se aplica"),
    qt_maxima_execucao = ifelse(qt_maxima_execucao == 9999,
                                "Não se aplica",
                                as.character(qt_maxima_execucao)), 
    qt_dias_permanencia = ifelse(qt_dias_permanencia == 9999,
                                 "Não se aplica",
                                 as.character(qt_dias_permanencia)),
    qt_pontos = ifelse(qt_pontos == 9999,
                       "Não se aplica",
                       as.character(qt_pontos)),
    vl_idade_minima = ifelse(vl_idade_minima == 9999,
                             "Não se aplica",
                             paste0(round(vl_idade_minima / 12, 1), " anos")),
    vl_idade_maxima = ifelse(vl_idade_maxima == 9999,
                             "Não se aplica",
                             paste0(round(vl_idade_maxima / 12, 1), " anos")),
    vl_sh = ifelse(is.na(vl_sh), "Não informado",
                   paste0("R$ ", formatC(vl_sh, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))),
    vl_sa = ifelse(is.na(vl_sa), "Não informado",
                   paste0("R$ ", formatC(vl_sa, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))),
    vl_sp = ifelse(is.na(vl_sp), "Não informado",
                   paste0("R$ ", formatC(vl_sp, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")))
  )

# tb_financiamento
tb_financiamento.ds <- tb_financiamento.ds %>%
  mutate(
    no_financiamento = recode(no_financiamento,
                              "01" = "Atenção Básica (PAB)",
                              "02" = "Média e Alta complecidade (MAC)",
                              "03" = "Fundo de Ações Estratégicas e Compensações (FAEC)",
                              "04" = "Viligância em Saúde",
                              "05" = "Assistência Farmacêutica",
                              "06" = "Assistência Farmacêutica e MAC",
                              "07" = "Incentivo MAC")
  )

# rl_procedimento_compativel
rl_procedimento_compativel.ds <- rl_procedimento_compativel.ds %>%
  mutate(
    tp_compatibilidade = recode(tp_compatibilidade,
                                "1" = "Compatível",
                                "2" = "Incompatível/Excludente",
                                "3" = "Concomitante")
  )

# rl_excecao_compatibilidade
rl_excecao_compatibilidade.ds <- rl_excecao_compatibilidade.ds %>%
  mutate(
    tp_compatibilidade = recode(tp_compatibilidade,
                                "1" = "Compatível",
                                "2" = "Incompatível/Excludente",
                                "3" = "Concomitante")
  )

# tb_cid
tb_cid.ds <- tb_cid.ds %>%
  mutate(
    tp_agravo = recode(tp_agravo,
                       "0" = "Sem agravo",
                       "1" = "Agravo de notificação",
                       "2" = "Agravo de bloqueio"),
    tp_sexo = recode(tp_sexo,
                     "M" = "Masculino",
                     "F" = "Feminino",
                     "I" = "Indiferente/Ambos",
                     "N" = "Não se aplica"),
    tp_estadio = recode(tp_estadio,
                        "S" = "Sim",
                        "N" = "Não")
  )

# rl_procedimento_cid
rl_procedimento_cid.ds <- rl_procedimento_cid.ds %>%
  mutate(
    st_principal = recode(st_principal,
                          "S" = "Sim",
                          "N" = "Não")
  )

# tb_tipo_leito
tb_tipo_leito.ds <- tb_tipo_leito.ds %>%
  mutate(
    no_tipo_leito = recode(no_tipo_leito,
                           "01" = "Cirúrgico",
                           "02" = "Obstétricos",
                           "03" = "Clínico",
                           "04" = "Crônicos",
                           "05" = "Psiquiatria",
                           "06" = "Pneumologia Sanitária (Tisiologia)",
                           "07" = "Pediátricos",
                           "08" = "Reabilitação",
                           "09" = "Leito Dia/Cirúrgicos",
                           "10" = "Leito Dia/Aids",
                           "11" = "Leito Dia/Fibrose Cística",
                           "12" = "Leito Dia/Intercorrência Pós-Transplante",
                           "13" = "Leito Dia/Geriatria",
                           "14" = "Leito Dia/Saúde Mental",
                           "64" = "Unidade Intermediária",
                           "65" = "Unidade Intermediária Neonatal",
                           "74" = "UTI I",
                           "75" = "UTI Adulto II",
                           "76" = "UTI Adulto III",
                           "77" = "UTI Infantil I",
                           "78" = "UTI Infantil II",
                           "79" = "UTI Infantil III",
                           "80" = "UTI Neonatal I",
                           "81" = "UTI Neonatal II",
                           "82" = "UTI Neonatal III",
                           "83" = "UTI Queimados")
  )

# tb_sia_sih
tb_sia_sih.ds <- tb_sia_sih.ds %>%
  mutate(
    tp_procedimento = recode(tp_procedimento,
                             "A" = "Ambulatorial",
                             "H" = "Hospitalar")
  )

# rl_procedimento_sia_sih
rl_procedimento_sia_sih.ds <- rl_procedimento_sia_sih.ds %>%
  mutate(
    tp_procedimento = recode(tp_procedimento,
                             "A" = "Ambulatorial",
                             "H" = "Hospitalar")
  )

# Interface do usuário ----------------------------------------------------
ui <- fluidPage(
  titlePanel("Consulta - SIGTAP"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput("proc_busca", "Código do procedimento:", ""),
      textInput("desc_busca", "Nome do procedimento:", ""),
      actionButton("buscar", "Pesquisar")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Resumo", DTOutput("resumo_view")),
        tabPanel("CID", DTOutput("cid_view")),
        tabPanel("Ocupação", DTOutput("ocupacao_view")),
        tabPanel("Modalidade", DTOutput("modalidade_view")),
        tabPanel("Detalhe", DTOutput("detalhe_view")),
        tabPanel("Habilitação", DTOutput("habilitacao_view")),
        tabPanel("SIA/SIH", DTOutput("siasih_view")),
        tabPanel("Registro", DTOutput("registro_view")),
        tabPanel("Leito", DTOutput("leito_view")),
        tabPanel("Servico", DTOutput("servico_view")),
        tabPanel("Rubrica", DTOutput("rubrica_view"))
      )
    )
  )
)

# Servidor ------------------------------------------------------

# Os "joins" foram feitos nessa etapa por questão de performance

server <- function(input, output, session) {
  
  # Pesquisa: CÓDIGO x NOME
  resultados <- eventReactive(input$buscar, {
    if (input$proc_busca != "") {
      tb_procedimento.ds %>% filter(co_procedimento == input$proc_busca)
    } else if (input$desc_busca != "") {
      tb_procedimento.ds %>% filter(grepl(input$desc_busca, no_procedimento, ignore.case = TRUE))
    } else {
      NULL
    }
  })
  
  # Aba Resumo também mostra todos os resultados encontrados caso pesquise por nome do procedimento
  output$resumo_view <- renderDT({
    req(resultados())
    df <- resultados() %>%
      select(co_procedimento, no_procedimento, tp_complexidade, tp_sexo,
             qt_maxima_execucao, qt_dias_permanencia,
             vl_idade_minima, vl_idade_maxima,
             vl_sh, vl_sa, vl_sp)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba CID
  output$cid_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_cid.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_cid.ds, by = "co_cid") %>%
      select(co_cid, no_cid, st_principal, tp_agravo, tp_estadio)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Ocupação
  output$ocupacao_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_ocupacao.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_ocupacao.ds, by = "co_ocupacao") %>%
      select(co_ocupacao, no_ocupacao)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Modalidade
  output$modalidade_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_modalidade.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_modalidade.ds, by = "co_modalidade") %>%
      select(co_modalidade, no_modalidade)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Detalhe
  output$detalhe_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_detalhe.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_detalhe.ds, by = "co_detalhe") %>%
      left_join(tb_descricao_detalhe.ds, by = "co_detalhe") %>%
      select(co_detalhe, no_detalhe, ds_detalhe)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Habilitação
  output$habilitacao_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_habilitacao.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_habilitacao.ds, by = "co_habilitacao") %>%
      select(co_habilitacao, no_habilitacao)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba SIA/SIH
  output$siasih_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_sia_sih.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      select(co_procedimento, co_procedimento_sia_sih) %>%  # remove tp_procedimento para evitar duplicacao nas tabelas
      left_join(tb_sia_sih.ds, by = "co_procedimento_sia_sih") %>%
      select(co_procedimento, co_procedimento_sia_sih,no_procedimento_sia_sih, tp_procedimento)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Registro
  output$registro_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_registro.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_registro.ds, by = "co_registro") %>%
      select(co_registro, no_registro)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Leito
  output$leito_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_leito.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_tipo_leito.ds, by = "co_tipo_leito") %>%
      select(co_tipo_leito, no_tipo_leito)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Serviço
  output$servico_view <- renderDT({
    req(input$proc_busca)
    df <- rl_procedimento_servico.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_servico.ds, by = "co_servico") %>%
      left_join(tb_servico_classificacao.ds, by = c("co_servico", "co_classificacao")) %>%
      select(co_servico, no_servico, co_classificacao, no_classificacao)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Aba Rubrica
  output$rubrica_view <- renderDT({
    req(input$proc_busca)
    df <- tb_procedimento.ds %>%
      filter(co_procedimento == input$proc_busca) %>%
      left_join(tb_rubrica.ds, by = "co_rubrica") %>%
      select(co_rubrica, no_rubrica)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
}

# Executa o app -----------------------------------------------------------
shinyApp(ui, server)
