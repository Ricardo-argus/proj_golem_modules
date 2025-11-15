#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import pool
#' @import RMariaDB
#' @import DBI
#' @noRd
app_server <- function( input, output, session ) {

  #Database connection
  tryCatch(
    {
      db_config <- app_config("database")
    },
    error = function(e) {
      stop(paste("Erro fatal ao carregar golem-config.yml:", e$message))
    }
  )

  con <- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    host = db_config$host,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password,
    dbname = db_config$dbname
  )

  session$onSessionEnded(function() {
    pool::poolClose(con)
  })

  #MODULES

  # Chama o módulo de filtros e armazena os filtros reativos que ele RETORNA
  filtros_selecionados <- mod_filtros_server("filtros_1", con = con)


  # --- 3. DADOS REATIVOS (Permanece aqui) ---

  dadosCompletos <- reactive({
    # A query para carregar TODOS os dados
    query <- "
       SELECT
         d.id_bolsista, d.ANO_CONCESSAO_BOLSA, d.SEXO_BENEFICIARIO, d.RACA_BENEFICIARIO,
         u.UF_BENEFICIARIO, bu.UNIVERSIDADE_BOLSA, tb.TIPO_BOLSA, mb.MODALIDADE_ENSINO_BOLSA,
         trb.TURNO_BOLSA, cb.CURSO_BOLSISTA
       FROM
         dados_bolsistas d
       LEFT JOIN UF_bolsas u ON d.id_bolsista = u.id_bolsista
       LEFT JOIN bolsas_universidades bu ON d.id_bolsista = bu.id_bolsista
       LEFT JOIN tipo_bolsa tb ON d.id_bolsista = tb.id_bolsista
       LEFT JOIN modalidade_bolsa mb ON d.id_bolsista = mb.id_bolsista
       LEFT JOIN turno_bolsa trb ON d.id_bolsista = trb.id_bolsista
       LEFT JOIN curso_bolsista cb ON d.id_bolsista = cb.id_bolsista
     "
    dbGetQuery(con, query)
  })

  # Este reativo agora depende do reativo retornado pelo módulo de filtros
  dadosFiltrados <- reactive({
    data <- dadosCompletos()
    filtros <- filtros_selecionados() # Pega os valores (ex: filtros$ano)

    # Validação para garantir que os filtros estejam prontos
    req(filtros)

    if (filtros$ano != "Todos") {
      data <- data %>% filter(ANO_CONCESSAO_BOLSA == filtros$ano)
    }

    if (filtros$uf != "Todos") {
      data <- data %>% filter(UF_BENEFICIARIO == filtros$uf)
    }

    if (filtros$universidade != "Todos") {
      data <- data %>% filter(UNIVERSIDADE_BOLSA == filtros$universidade)
    }

    data
  })

  # --- 4. CHAMADA DOS MÓDULOS DE CONTEÚDO ---

  # Passa os dados filtrados E os filtros selecionados para o módulo de overview
  mod_overview_server("overview_1",
                      dados_filtrados = dadosFiltrados,
                      filtros_selecionados = filtros_selecionados)

  # Passa (apenas) os dados filtrados para o módulo de tabela
  mod_dados_brutos_server("dados_brutos_1",
                          dados_filtrados = dadosFiltrados)


}
