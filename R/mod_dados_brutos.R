#' mod_dados_brutos UI Function
#'
#' @description UI para a aba de Dados Completos (Tabela DT).
#' @param id O ID interno do Shiny para este módulo.
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

          tabPanel(
            title = "PROUNI",
            icon = icon("table"),

            fluidRow(
              column(4,
            selectInput(ns("uf"), "Selecione a UF: ", choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                                                  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                                                  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                        selected = "Todos"
              )
            ),

              column(4,
                     selectInput(ns("raca"), "Raça do Beneficiário: ", choices = c("Todas","Branca", "Parda", "Preta", "Amarela", "Nao Informada", "Indigena"),
                                 selected = "Todas"
                    )
                  ),
              column(4,
                   selectInput(ns("ano"), "Ano da Bolsa: ", choices = c(2018,2019,2020),
                               selected = "2020"
                   )
                  )
                ),

            DT::dataTableOutput(ns("PROUNI")),

            fluidRow(
              column(6,
                     selectizeInput(ns("id_bolsista"), "Selecione o Id do Bolsista:", choices = NULL)
                     ),
              column(6,
                     selectizeInput(ns("novo_turno"), "Novo Turno:", choices = c("MATUTINO", "VESPERTINO", "NOTURNO", "CURSO A DISTANCIA"))
                     )
            ),
            actionButton(ns("atualizar_turno"), "Atualizar Turno", icon = icon("sync"), class = "btn-primary")

          ),

            tabPanel(
              title = "BOLSA FAMÍLIA",
              icon = icon("table"),

              fluidRow(
                column(4,
                       selectInput(ns("estado"), "Selecione o Estado: ", choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                                                             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                                                             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                                   selected = "Todos"
                  )
                )
              ),

              DT::dataTableOutput(ns("bolsafamilia")),
            ),

            tabPanel(
            title = "Luz Para Todos",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("estado"), "Selecione o Estado: ", choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                                                               "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                                                               "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                                 selected = "Todos"
                )
              ),

              column(4,
                     selectInput(ns("mes_atendimento"), "Selecione o mês de atendimento: ", choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                                                                   "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                                                                   "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

                  )
              )
            ),

            DT::dataTableOutput(ns("luzpt"))

          ),

          )

        )
    )
  )
}
#' mod_dados_brutos Server Function
#'
#' @description Server para a aba de Dados Completos.
#' @param id O ID interno do Shiny para este módulo.
#' @param dados_filtrados Um reativo com os dados já filtrados.
#' @import shiny
#' @import DT
#' @import dplyr
#' @noRd
mod_dados_brutos_server <- function(id, dados_filtrados, dados_luz, dados_bf, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$PROUNI <- DT::renderDataTable({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      df_filtrado <- df

      if(input$uf != "Todos"){
        df_filtrado <- df_filtrado %>% filter(UF_BENEFICIARIO == input$uf)
      }

      if(input$raca != "Todas"){
        df_filtrado <- df_filtrado %>% filter(RACA_BENEFICIARIO == input$raca)
      }


      DT::datatable(
        df_filtrado,
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

    #atualiza tabela prouni
    observe({
      ids <- dados_filtrados()$id_bolsista
      updateSelectizeInput(session, "id_bolsista", choices = ids, server=TRUE)
    })

    observeEvent(input$atualizar_turno, {
      req(input$id_bolsista, input$novo_turno)


          query <- sprintf("UPDATE prouni_bolsas.turno_bolsa SET TURNO_BOLSA = '%s' WHERE id_bolsista = %s",
                           input$novo_turno, input$id_bolsista
          )

          DBI::dbExecute(con,query)

          showNotification("Turno atualizado com sucesso!", type = 'message')
    })


    output$luzpt <- DT::renderDataTable({
      df <- dados_luz()
      req(nrow(df) > 0)
      DT::datatable(
        df,
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


    output$bolsafamilia <- DT::renderDataTable({
      df <- dados_bf()
      req(nrow(df) > 0)

      DT::datatable(
        df,
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

  })
}
