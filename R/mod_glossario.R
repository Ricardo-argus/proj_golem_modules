#' glossario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_glossario_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tabsetPanel(
        tabPanel(
          title = "PROUNI",
          icon = icon("book"),
          h4("Programa Universidade para Todos"),
          p("Oferece bolsas de estudo em instituições privadas para estudantes de baixa renda.")
        ),

        tabPanel(
          title = "BOLSA FAMILIA",
          icon = icon("book"),
          h4("Programa Bolsa Família"),
          p("Transferência de renda para famílias em situação de pobreza e extrema pobreza.")
        ),

        tabPanel(
          title = "LUZ PARA TODOS",
          icon = icon("book"),
          h4("Programa Luz Para Todos"),
          p("Leva energia elétrica a comunidades rurais e regiões isoladas.")
        )
      )
    )
  )
}

#' glossario Server Functions
#'
#' @noRd
mod_glossario_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

