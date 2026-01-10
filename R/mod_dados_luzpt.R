#' mod_dados_luzpt UI Function
#'
#' @description UI for Luz para todos Tab
#' @param id Intern Id from Shiny
#' @import shiny
#' @import DT
#' @noRd
mod_dados_luzpt_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("estado_luzpt"), "Estado:",
                            choices=c("Todos","Acre","Amapá","Amazonas","Bahia","Mato Grosso","Mato Grosso do Sul",
                                      "Pará","Rondonia","Roraima","Tocantins"), selected="Todos")),
      column(4, selectInput(ns("mes_atendimento"), "Mês:",
                            choices=c("Todos","Janeiro","Fevereiro","Marco","Abril","Maio","Junho",
                                      "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"), selected="Todos")),
      column(4, selectInput(ns("programa_luzpt"), "Programa:",
                            choices=c("Todos","LPT - Rural","LPT - Regioes Remotas da Amazonia Legal"), selected="Todos"))
    ),
    DT::dataTableOutput(ns("luzpt")),
    fluidRow(
      column(4, selectizeInput(ns("id_beneficiarios"), "Id da família:", choices=NULL)),
      column(4, selectizeInput(ns("coluna_alvo_luzpt"), "Coluna:", choices=NULL)),
      column(4, selectizeInput(ns("valor_novo_luzpt"), "Novo Valor:", choices=NULL))
    ),
    actionButton(ns("atualizar_valor_luzpt"), "Atualizar Valores", icon=icon("sync"), class="btn-primary")
  )
}

#' mod_dados_luzpt Server Function
#'
#' @description Server for Luz para todos Tab
#' @param id Intern Id from Shiny
#' @param dados_luz Reactive data for Luz para todos
#' @param con Database Connection
#' @import shiny
#' @import DT
#' @import dplyr
#' @import DBI
#' @noRd
mod_dados_luzpt_server <- function(id, dados_luz, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$luzpt <- DT::renderDataTable({
      df <- dados_luz()
      req(nrow(df) > 0)
      if(input$estado_luzpt != "Todos") df <- df %>% filter(estado == input$estado_luzpt)
      if(input$mes_atendimento != "Todos") df <- df %>% filter(mes_atendimento == input$mes_atendimento)
      if(input$programa_luzpt != "Todos") df <- df %>% filter(programa == input$programa_luzpt)
      DT::datatable(df, filter='top', options=list(pageLength=10, scrollX=TRUE,
                                                   language=list(url='//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')))
    })

      # update table luz para todos

    coluna_tabela_map_luzpt <- list(
      id_beneficiarios   = "luz_ano",
      mes_atendimento    = "luz_ano",
      ano_atendimento    = "luz_ano",
      ano_homologacao    = "luz_ano",
      qtd_domicilios     = "luz_domicilios_beneficiarios",
      estado             = "luz_estado_beneficiarios",
      programa           = "luz_programa_beneficiarios"
    )

    observe({
      req(nrow(dados_luz()) > 0)
      ids <- dados_luz()$id_beneficiarios
      ids<- unique(na.omit(ids))
      updateSelectizeInput(session, "id_beneficiarios", choices = ids, server=TRUE)
    })

    observe({
      df <- dados_luz()
      req(nrow(df) > 0)
      updateSelectizeInput(session, "coluna_alvo_luzpt", choices = names(coluna_tabela_map_luzpt), server = TRUE)
    })

    observeEvent(input$coluna_alvo_luzpt, {
      df <- dados_luz()
      req(input$coluna_alvo_luzpt %in% names(df))
      valores <- unique(na.omit(df[[input$coluna_alvo_luzpt]]))
      updateSelectizeInput(session, "valor_novo_luzpt", choices = valores, server = TRUE)

    })


    observeEvent(input$atualizar_valor_luzpt, {
      req(input$id_beneficiarios, input$coluna_alvo_luzpt, input$valor_novo_luzpt)

      tabela_luzpt <- coluna_tabela_map_luzpt[[input$coluna_alvo_luzpt]]

      tabela_sql_luzpt <- DBI::dbQuoteIdentifier(con,tabela_luzpt)
      coluna_sql_luzpt <- DBI::dbQuoteIdentifier(con, input$coluna_alvo_luzpt)


      query <- paste0("UPDATE prouni_bolsas.", tabela_sql_luzpt,
                      "SET ", coluna_sql_luzpt, " = '", input$valor_novo_luzpt,
                      "' WHERE id_beneficiarios = ", input$id_beneficiarios)

      DBI::dbExecute(con,query)

      showNotification(
        paste0("Tabela ",tabela_luzpt," coluna ", input$coluna_alvo_luzpt, " atualizada para ", input$valor_novo_luzpt, " no cadastro da familia ",input$id_beneficiarios,"!"),
        type = 'message')
    })

  })
}
