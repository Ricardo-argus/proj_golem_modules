#' mod_dados_prouni UI Function
#'
#' @description UI for PROUNI tab
#' @param id Intern Id
#' @import shiny
#' @import DT
#' @noRd
mod_dados_prouni_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("uf"), "Selecione a UF:",
                            choices = c("Todos","AC","AL","AP","AM","BA","CE","DF","ES","GO",
                                        "MA","MT","MS","MG","PA","PB","PR","PE","PI",
                                        "RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
                            selected="Todos")),
      column(4, selectInput(ns("raca"), "Raça do Beneficiário:",
                            choices = c("Todas","Branca","Parda","Preta","Amarela","Nao Informada","Indigena"),
                            selected="Todas")),
      column(4, selectInput(ns("ano"), "Ano da Bolsa:",
                            choices = c("Todos",2018,2019,2020), selected="Todos"))
    ),
    DT::dataTableOutput(ns("PROUNI")),
    fluidRow(
      column(4, selectizeInput(ns("id_bolsista"), "Id do Bolsista:", choices=NULL)),
      column(4, selectizeInput(ns("coluna_alvo"), "Coluna:", choices=NULL)),
      column(4, selectizeInput(ns("valor_novo"), "Novo Valor:", choices=NULL))
    ),
    actionButton(ns("atualizar_valor"), "Atualizar Valores", icon=icon("sync"), class="btn-primary")
  )
}

#' mod_dados_prouni Server Function
#'
#' @description Server for PROUNI tab
#' @param id Intern Id from Shiny
#' @param dados_filtrados Reactive with filtered data
#' @param con Database Connection
#' @import shiny
#' @import DT
#' @import dplyr
#' @import DBI
#' @noRd
mod_dados_prouni_server <- function(id, dados_filtrados, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$PROUNI <- DT::renderDataTable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      if(input$uf != "Todos") df <- df %>% filter(UF_BENEFICIARIO == input$uf)
      if(input$raca != "Todas") df <- df %>% filter(RACA_BENEFICIARIO == input$raca)
      if(input$ano != "Todos") df <- df %>% filter(ANO_CONCESSAO_BOLSA == input$ano)
      DT::datatable(df, filter='top', options=list(pageLength=10, scrollX=TRUE,
                                                   language=list(url='//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')))
    })

    #update table  prouni

    coluna_tabela_map <- list(
      id_bolsista            = "dados_bolsistas",
      ANO_CONCESSAO_BOLSA    = "dados_bolsistas",
      SEXO_BENEFICIARIO      = "dados_bolsistas",
      RACA_BENEFICIARIO      = "dados_bolsistas",
      UF_BENEFICIARIO        = "UF_bolsas",
      UNIVERSIDADE_BOLSA     = "bolsas_universidades",
      TIPO_BOLSA             = "tipo_bolsa",
      MODALIDADE_ENSINO_BOLSA= "modalidade_bolsa",
      TURNO_BOLSA            = "turno_bolsa",
      CURSO_BOLSISTA         = "curso_bolsista"
    )

    observe({
      req(nrow(dados_filtrados()) > 0)
      ids <- dados_filtrados()$id_bolsista
      ids<- unique(na.omit(ids))
      updateSelectizeInput(session, "id_bolsista", choices = ids, server=TRUE)
    })

    observe({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      updateSelectizeInput(session, "coluna_alvo", choices = names(coluna_tabela_map), server = TRUE)
    })

    observeEvent(input$coluna_alvo, {
      df <- dados_filtrados()
      req(input$coluna_alvo %in% names(df))
      valores <- unique(na.omit(df[[input$coluna_alvo]]))
      updateSelectizeInput(session, "valor_novo", choices = valores, server = TRUE)

    })


    observeEvent(input$atualizar_valor, {
      req(input$id_bolsista, input$coluna_alvo, input$valor_novo)

      tabela <- coluna_tabela_map[[input$coluna_alvo]]

      tabela_sql <- DBI::dbQuoteIdentifier(con,tabela)
      coluna_sql <- DBI::dbQuoteIdentifier(con, input$coluna_alvo)


      query <- paste0("UPDATE prouni_bolsas.", tabela_sql,
                      "SET ", coluna_sql, " = '", input$valor_novo,
                      "'WHERE id_bolsista = ", input$id_bolsista)

      DBI::dbExecute(con,query)

      showNotification(
        paste0("Tabela ",tabela," coluna ", input$coluna_alvo, " atualizada para ", input$valor_novo, " no bolsista ",input$id_bolsista,"!"),
        type = 'message')
    })
  })
}
