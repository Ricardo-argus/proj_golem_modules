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

            DT::dataTableOutput(ns("PROUNI"))

          ),
            # Aba 2: Exemplo de outra aba
            tabPanel(
              title = "BOLSA FAMÍLIA", # Título da segunda aba
              icon = icon("table"), # Ícone opcional

              # Você pode adicionar outros outputs aqui, como gráficos ou resumos
              h4("Tabela a ser Preenchida."),
              p("Adicionar dados dos Beneficiários do Bolsa Família."),
              verbatimTextOutput(ns("resumo_exemplo")) # Exemplo de novo output
            ),

          tabPanel(
            title = "Luz Para Todos",
            icon = icon("table"),

            h4("Tabela a ser preenchida."),
            p("Adicionar dados dos Domicílios atendidos pelo Luz para Todos."),
            verbatimTextOutput(ns("exemplo_resumo"))
          )

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
mod_dados_brutos_server <- function(id, dados_filtrados){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$PROUNI <- DT::renderDataTable({
      df <- dados_filtrados()
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
