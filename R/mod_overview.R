#' mod_overview UI Function
#'
#' @description UI for tab VISAO Geral
#' @param id Intern Id for this module
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @noRd
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$head(
        tags$style(HTML("
          .nav-tabs > li > a {
            background-color: #FFFFF0;
            color: #1C1C1C;
          }
          .nav-tabs > li.active > a {
            background-color: #000000 !important;
            color: white !important;
          }
        "))
      ),
      tabsetPanel(
        tabPanel(
          title = "PROUNI",
          icon = icon("chart-line"),
          mod_bolsas_overview_ui(ns("bolsas")),
          div(style = "overflow-x: auto;",
              DT::dataTableOutput(ns("PROUNI"))
          )
        ),
        tabPanel(
          title = "BOLSA FAMÍLIA",
          icon = icon("chart-line"),
          mod_bolsafamilia_overview_ui(ns("bolsafamilia")),
          div(style = "overflow-x: auto;",
              DT::dataTableOutput(ns("BOLSA_FAMILIA"))
          )
        ),
        tabPanel(
          title = "LUZ PARA TODOS",
          icon = icon("chart-line"),
          mod_luzpt_overview_ui(ns("luzpt")),
          div(style = "overflow-x: auto;",
              DT::dataTableOutput(ns("LUZ_PARA_TODOS"))
          )
        )
      )
    )
  )
}



#' mod_overview Server Function
#'
#' @description Server for tab Visao Geral
#' @param id Intern Id for this module
#' @param dados_filtrados a Reactive with filtered data
#' @param filtros_selecionados a reactive for filters
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @noRd
mod_overview_server <- function(id, dados_filtrados, filtros_selecionados, con, dados_luz ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # SubModules for Each Social Programs
    mod_bolsas_overview_server("bolsas", dados_filtrados, filtros_selecionados)
    mod_bolsafamilia_overview_server("bolsafamilia", con)
    mod_luzpt_overview_server("luzpt", con)

  })
}
