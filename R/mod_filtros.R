#' mod_filtros UI Function
#'
#' @description UI para os filtros da sidebar.
#' @param id O ID interno do Shiny para este módulo.
#' @import shiny
#' @noRd
 mod_filtros_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("ano_filter"), "Ano de Concessão:",
                choices = c("Carregando..." = "")),

    selectInput(ns("uf_filter"), "UF do Beneficiário:",
                choices = c("Carregando..." = "")),

    selectInput(ns("universidade_filter"), "Universidade:",
                choices = c("Carregando..." = ""))
  )
}

#' mod_filtros Server Function
#'
#' @description Server para popular os filtros e retornar seus valores.
#' @param id O ID interno do Shiny para este módulo.
#' @param con O pool de conexão com o banco de dados (passado do app_server).
#' @import shiny
#' @import DBI
#' @noRd
mod_filtros_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- Lógica para popular os filtros (movida do app_server) ---
    observeEvent(session, {
      isolate({
        tryCatch({
          anos_disponiveis <- dbGetQuery(con, "SELECT DISTINCT ANO_CONCESSAO_BOLSA FROM dados_bolsistas ORDER BY ANO_CONCESSAO_BOLSA DESC;")
          ufs_disponiveis <- dbGetQuery(con, "SELECT DISTINCT UF_BENEFICIARIO FROM UF_bolsas ORDER BY UF_BENEFICIARIO;")
          universidades_disponiveis <- dbGetQuery(con, "SELECT DISTINCT UNIVERSIDADE_BOLSA FROM bolsas_universidades ORDER BY UNIVERSIDADE_BOLSA;")

          # IMPORTANTE: Usar a 'session' do módulo aqui
          updateSelectInput(session, "ano_filter",
                            choices = c("Todos", sort(unique(anos_disponiveis$ANO_CONCESSAO_BOLSA))),
                            selected = "Todos")

          updateSelectInput(session, "uf_filter",
                            choices = c("Todos", ufs_disponiveis$UF_BENEFICIARIO),
                            selected = "Todos")

          updateSelectInput(session, "universidade_filter",
                            choices = c("Todos", universidades_disponiveis$UNIVERSIDADE_BOLSA),
                            selected = "Todos")

        }, error = function(e) {
          showNotification(paste("Erro ao carregar filtros:", e$message), type = "error", duration = 10)
        })
      })
    }, once = TRUE)

    # --- RETORNO DO MÓDULO ---
    # Retorna um reativo contendo uma lista dos valores de input
    # O app_server vai "escutar" este reativo
    return(
      reactive({
        # Garante que o primeiro filtro foi populado antes de retornar
        req(input$ano_filter != "")
        list(
          ano = input$ano_filter,
          uf = input$uf_filter,
          universidade = input$universidade_filter
        )
      })
    )

  })
}
