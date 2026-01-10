#' mod_dados_brutos UI Function
#'
#' @description UI for tab Dados Completos.
#' @param id Intern Id from Shiny for this Module
#' @import shiny
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
          tabPanel("PROUNI", icon=icon("table"), mod_dados_prouni_ui(ns("prouni"))),
          tabPanel("Bolsa Família", icon=icon("table"), mod_dados_bolsafamilia_ui(ns("bolsafamilia"))),
          tabPanel("Luz Para Todos", icon=icon("table"), mod_dados_luzpt_ui(ns("luzpt")))
        )
      )
    ),

    # adjust Selectizeinput dropdown view
    tags$style(HTML("
      .selectize-dropdown-content {
        max-height: 400px !important;
        overflow-y: auto !important;
        background-color: white !important;
        color: black !important;
        z-index: 9999 !important;
      }
      .selectize-input {
        background-color: white !important;
        color: black !important;
      }
      .selectize-dropdown [data-selectable] {
        color: black !important;
      }
    ")),

    tags$style(HTML("
  body {
    margin-bottom: 50px;
  }
"))
  )
}
#' mod_dados_brutos Server Function
#'
#' @description Server For Dados Brutos tab
#' @param id Intern Id from Shiny
#' @param dados_filtrados Reactive data FROM PROUNI
#' @param dados_luz Reactive data FROM Luz Para Todos
#' @param dados_bf Reactive data FROM Bolsa Família
#' @param con Database Connection
#' @import shiny
#' @noRd
mod_dados_brutos_server <- function(id, dados_filtrados, dados_luz, dados_bf, con){
  moduleServer(id, function(input, output, session){
    mod_dados_prouni_server("prouni", dados_filtrados, con)
    mod_dados_bolsafamilia_server("bolsafamilia", dados_bf, con)
    mod_dados_luzpt_server("luzpt", dados_luz, con)
  })
}
