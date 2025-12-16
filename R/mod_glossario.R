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
          icon = icon("book")
        ),

        tabPanel(
          title = "BOLSA FAMILIA",
          icon = icon("book")
        ),

        tabPanel(
          title = "LUZ PARA TODOS",
          icon = icon("book")
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

