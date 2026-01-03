#' mod_dados_brutos UI Function
#'
#' @description UI for tab Dados Completos.
#' @param id Intern Id from Shiny for this Module
#' @import shiny
#' @import DT
#' @noRd
mod_dados_brutos_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Dados Consolidados dos Beneficiários", status = "primary", solidHeader = TRUE,
        width = 12,

        tabsetPanel(
          id = ns("tabset_dados"),

          # PROUNI
          tabPanel(
            title = "PROUNI",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("uf"), "Selecione a UF: ",
                                 choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("raca"), "Raça do Beneficiário: ",
                                 choices = c("Todas","Branca", "Parda", "Preta", "Amarela", "Nao Informada", "Indigena"),
                                 selected = "Todas")
              ),
              column(4,
                     selectInput(ns("ano"), "Ano da Bolsa: ",
                                 choices = c("Todos",2018,2019,2020),
                                 selected = "Todos")
              )
            ),

            DT::dataTableOutput(ns("PROUNI")),

            fluidRow(
              column(4,
                  div(style = "max-height: 300px; overflow-y: auto;",
                     selectizeInput(ns("id_bolsista"), "Selecione o Id do Bolsista:", choices = NULL)
                  )
              ),
              column(4,
                  div(style = "max-height: 300px; overflow-y: auto;",
                     selectizeInput(ns("coluna_alvo"), "Coluna a ser alterada:", choices = NULL, multiple = FALSE,
                                    options = list(selectOnTab = TRUE, openOnFocus = TRUE, closeAfterSelect = TRUE))
                     )
              ),
              column(4,
                  div(style = "max-height: 300px; overflow-y: auto;",
                     selectizeInput(ns("valor_novo"), "Novo Valor para Coluna Selecionada:", choices = NULL, multiple = FALSE)
                  )
              )
            ),

            actionButton(ns("atualizar_valor"), "Atualizar Valores", icon = icon("sync"), class = "btn-primary"),

            tags$style(HTML("
              .selectize-dropdown-content {
                max-height: 300px;
                overflow-y: auto;
                background-color: white;
                color: black;
              }
            "))
          ),

          # BOLSA FAMÍLIA
          tabPanel(
            title = "BOLSA FAMÍLIA",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("estado_bf"), "Selecione o Estado:",
                                 choices = c("Todos",
                                             "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal",
                                             "Espirito Santo", "Goias", "Maranhao", "Mato Grosso", "Mato Grosso do Sul",
                                             "Minas Gerais", "Pará", "Paraiba", "Parana", "Pernambuco", "Piaui",
                                             "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul",
                                             "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", "Tocantins"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("ano_bf"), "Ano de concessão do benefício:",
                                 choices = c("Todos",2023,2024,2025),
                                 selected = "Todos")
              )
            ),

            DT::dataTableOutput(ns("bolsafamilia")),

            fluidRow(
            column(4,
                   div(style = "max-height: 300px; overflow-y: auto;",
                       selectizeInput(ns("id_familias"), "Selecione o Id da familia atendida:", choices = NULL)
                   )
            ),
            column(4,
                   div(style = "max-height: 300px; overflow-y: auto;",
                       selectizeInput(ns("coluna_alvo_bolsafm"), "Coluna a ser alterada:", choices = NULL, multiple = FALSE,
                                      options = list(selectOnTab = TRUE, openOnFocus = TRUE, closeAfterSelect = TRUE))
                   )
            ),
            column(4,
                   div(style = "max-height: 300px; overflow-y: auto;",
                       selectizeInput(ns("valor_novo_bolsafm"), "Novo Valor para Coluna Selecionada:", choices = NULL, multiple = FALSE)
                   )
            )
          ),

          actionButton(ns("atualizar_valor_bolsafm"), "Atualizar Valores", icon = icon("sync"), class = "btn-primary"),

          tags$style(HTML("
              .selectize-dropdown-content {
                max-height: 300px;
                overflow-y: auto;
                background-color: white;
                color: black;
              }
            "))
          ),

          # LUZ PARA TODOS
          tabPanel(
            title = "Luz Para Todos",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("estado_luzpt"), "Estado:",
                                 choices = c("Todos",
                                             "Acre", "Amapá", "Amazonas", "Bahia",
                                             "Mato Grosso", "Mato Grosso do Sul",
                                             "Pará",
                                             "Rondonia", "Roraima", "Tocantins"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("mes_atendimento"), "Mês do atendimento:",
                                 choices = c("Todos","Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
                                             "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("programa_luzpt"),"Tipo de Programa:",
                                 choices = c("Todos","LPT - Rural", "LPT - Regioes Remotas da Amazonia Legal"),
                                 selected = "Todos")

              )
            ),

            DT::dataTableOutput(ns("luzpt")),

            fluidRow(
              column(4,
                     div(style = "max-height: 300px; overflow-y: auto;",
                         selectizeInput(ns("id_beneficiarios"), "Selecione o Id da familia atendida:", choices = NULL)
                     )
              ),
              column(4,
                     div(style = "max-height: 300px; overflow-y: auto;",
                         selectizeInput(ns("coluna_alvo_luzpt"), "Coluna a ser alterada:", choices = NULL, multiple = FALSE,
                                        options = list(selectOnTab = TRUE, openOnFocus = TRUE, closeAfterSelect = TRUE))
                     )
              ),
              column(4,
                     div(style = "max-height: 300px; overflow-y: auto;",
                         selectizeInput(ns("valor_novo_luzpt"), "Novo Valor para Coluna Selecionada:", choices = NULL, multiple = FALSE)
                     )
              )
            ),

            actionButton(ns("atualizar_valor_luzpt"), "Atualizar Valores", icon = icon("sync"), class = "btn-primary"),

            tags$style(HTML("
              .selectize-dropdown-content {
                max-height: 300px;
                overflow-y: auto;
                background-color: white;
                color: black;
              }
            "))
            )
          )
        )
      )
    )
}
#' mod_dados_brutos Server Function
#'
#' @description Server for tab Dados Completos.
#' @param id Intern Id from Shiny for this Module
#' @param dados_filtrados a Reactive Filter already applied
#' @import shiny
#' @import DT
#' @import dplyr
#' @noRd
mod_dados_brutos_server <- function(id, dados_filtrados, dados_luz, dados_bf, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Filters logic for PROUNI tab
    output$PROUNI <- DT::renderDataTable({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      df_prouni_filtrado <- df

      if(input$uf != "Todos"){
        df_prouni_filtrado <- df_prouni_filtrado %>% filter(UF_BENEFICIARIO == input$uf)
      }

      if(input$raca != "Todas"){
        df_prouni_filtrado <- df_prouni_filtrado %>% filter(RACA_BENEFICIARIO == input$raca)
      }
      if(input$ano != "Todos"){
        df_prouni_filtrado <- df_prouni_filtrado %>% filter(ANO_CONCESSAO_BOLSA == input$ano)
      }

      #hover when click on selected option
      DT::datatable(
        df_prouni_filtrado,
        filter = 'top',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
        ),
        rownames = FALSE,
        class = "stripe hover"
      )
    })

    #Filters logic for bolsafamilia tab
    output$bolsafamilia <- DT::renderDataTable({
      df <- dados_bf()
      req(nrow(df) > 0)

      df_bolsafamilia_filtrado <- df

      if(input$estado_bf != "Todos"){
        df_bolsafamilia_filtrado <- df_bolsafamilia_filtrado %>% filter(estado == input$estado_bf)
      }

      if(input$ano_bf != "Todos"){
        df_bolsafamilia_filtrado <- df_bolsafamilia_filtrado %>% filter(ano_beneficio == input$ano_bf)
      }

      #hover when click on selected option
      DT::datatable(
        df_bolsafamilia_filtrado,
        filter = 'top',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
        ),
        rownames = FALSE,
        class = "stripe hover"
      )
    })


  #Filters logic for luzpt tab
  output$luzpt <- DT::renderDataTable({
    df <- dados_luz()
    req(nrow(df) > 0)

    df_luzpt_filtrado <- df

    if(input$estado_luzpt != "Todos"){
      df_luzpt_filtrado <- df_luzpt_filtrado %>% filter(estado == input$estado_luzpt)
    }

    if(input$mes_atendimento != "Todos"){
      df_luzpt_filtrado <- df_luzpt_filtrado %>% filter(mes_atendimento == input$mes_atendimento)
    }
    if(input$programa_luzpt != "Todos"){
      df_luzpt_filtrado <- df_luzpt_filtrado %>% filter(programa == input$programa_luzpt)
    }

    #hover when click on selected option
    DT::datatable(
      df_luzpt_filtrado,
      filter = 'top',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
  })

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



    #update table  prouni
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

    coluna_tabela_map_luzpt <- list(
      id_beneficiarios   = "luz_ano",
      mes_atendimento    = "luz_ano",
      ano_atendimento    = "luz_ano",
      ano_homologacao    = "luz_ano",
      qtd_domicilios     = "luz_domicilios_beneficiarios",
      estado             = "luz_estado_beneficiarios",
      programa           = "luz_programa_beneficiarios"
    )



    # update table luz para todos
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




    #update table bolsa familia
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
