#' mod_dados_bolsafamilia UI Function
#'
#' @description UI for bolsa familia tab
#' @param id Intern Id from Shiny
#' @import shiny
#' @import DT
#' @noRd
mod_dados_bolsafamilia_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("estado_bf"), "Estado:",
                            choices = c("Todos","Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal",
                                        "Espirito Santo","Goias","Maranhao","Mato Grosso","Mato Grosso do Sul",
                                        "Minas Gerais","Pará","Paraiba","Parana","Pernambuco","Piaui",
                                        "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul",
                                        "Rondonia","Roraima","Santa Catarina","Sao Paulo","Sergipe","Tocantins"),
                            selected="Todos")),
      column(4, selectInput(ns("ano_bf"), "Ano:", choices=c("Todos",2023,2024,2025), selected="Todos"))
    ),
    DT::dataTableOutput(ns("bolsafamilia")),
    fluidRow(
      column(4, selectizeInput(ns("id_familias"), "Id da família:", choices=NULL)),
      column(4, selectizeInput(ns("coluna_alvo_bolsafm"), "Coluna:", choices=NULL)),
      column(4, selectizeInput(ns("valor_novo_bolsafm"), "Novo Valor:", choices=NULL))
    ),
    actionButton(ns("atualizar_valor_bolsafm"), "Atualizar Valores", icon=icon("sync"), class="btn-primary")
  )
}

#' mod_dados_bolsafamilia Server Function
#'
#' @description Server for bolsa familia tab
#' @param id Intern Id from Shiny
#' @param dados_bf Reactive data for Bolsa Família
#' @param con Database Connection
#' @import shiny
#' @import DT
#' @import dplyr
#' @import DBI
#' @noRd
mod_dados_bolsafamilia_server <- function(id, dados_bf, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$bolsafamilia <- DT::renderDataTable({
      df <- dados_bf()
      req(nrow(df) > 0)
      if(input$estado_bf != "Todos") df <- df %>% filter(estado == input$estado_bf)
      if(input$ano_bf != "Todos") df <- df %>% filter(ano_beneficio == input$ano_bf)
      DT::datatable(df, filter='top', options=list(pageLength=10, scrollX=TRUE,
                                                   language=list(url='//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')))
    })

    ##update table bolsa familia

    coluna_tabela_map_bolsafm <- list(
      id_familias         = "benef_primeirainfancia",
      ano_beneficio       = "beneficio_ano",
      estado              = "bolsa_familia_ibge",
      codigo_ibge         = "bolsa_familia_ibge",
      qtd_ben_bpi         = "benef_primeirainfancia",
      qtd_ben_brc         = "benef_renda_complementar",
      qtd_ben_bco         = "benef_renda_complementar",
      qtd_ben_bvg         = "benef_somafamiliares",
      qtd_ben_bvn         = "benef_somafamiliares",
      qtd_ben_bva         = "benef_somafamiliares",
      qtd_ben_bv          = "benef_somafamiliares",
      qtd_ben_bf          = "benef_somafamiliares",
      qtd_ben_bv_bvs      = "benef_variaveis_soma",
      qtd_ben_bva_bvs     = "benef_variaveis_soma",
      qtd_ben_bvbva       = "benef_variaveis_soma"
    )





    observe({
      req(nrow(dados_bf()) > 0)
      ids <- dados_bf()$id_familias
      ids<- unique(na.omit(ids))
      updateSelectizeInput(session, "id_familias", choices = ids, server=TRUE)
    })

    observe({
      df <- dados_bf()
      req(nrow(df) > 0)
      updateSelectizeInput(session, "coluna_alvo_bolsafm", choices = names(coluna_tabela_map_bolsafm), server = TRUE)
    })

    observeEvent(input$coluna_alvo_bolsafm, {
      df <- dados_bf()
      req(input$coluna_alvo_bolsafm %in% names(df))
      valores <- unique(na.omit(df[[input$coluna_alvo_bolsafm]]))
      updateSelectizeInput(session, "valor_novo_bolsafm", choices = valores, server = TRUE)

    })


    observeEvent(input$atualizar_valor_bolsafm, {
      req(input$id_familias, input$coluna_alvo_bolsafm, input$valor_novo_bolsafm)

      tabela_bolsafm <- coluna_tabela_map_bolsafm[[input$coluna_alvo_bolsafm]]

      tabela_sql_bolsafm <- DBI::dbQuoteIdentifier(con,tabela_bolsafm)
      coluna_sql_bolsafm <- DBI::dbQuoteIdentifier(con, input$coluna_alvo_bolsafm)


      query <- paste0("UPDATE prouni_bolsas.", tabela_sql_bolsafm,
                      "SET ", coluna_sql_bolsafm, " = '", input$valor_novo_bolsafm,
                      "' WHERE id_familias = ", input$id_familias)

      DBI::dbExecute(con,query)

      showNotification(
        paste0("Tabela ",tabela_bolsafm," coluna ", input$coluna_alvo_bolsafm, " atualizada para ", input$valor_novo_bolsafm, " no cadastro da familia ",input$id_familias,"!"),
        type = 'message')
    })
  })
}
