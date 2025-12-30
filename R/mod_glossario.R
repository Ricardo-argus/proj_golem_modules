#' glossario UI Function
#'
#' @description A shiny Module com menu lateral customizado.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_glossario_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .glossario-wrapper {
        display: flex;
        height: 85vh;
        align-items: center;
        justify-content: flex-start;
        gap: 40px;
        color: white;
      }
      .glossario-menu {
        display: flex;
        flex-direction: column;
        padding: 15px;
        border: 1px solid white;
        border-radius: 8px;
        background-color: rgba(255,255,255,0.05);
        width: 220px;
        height: fit-content;
      }
      .glossario-item {
        padding: 10px;
        margin: 5px 0;
        cursor: pointer;
        border-radius: 6px;
        transition: background-color 0.3s;
      }
      .glossario-item:hover {
        background-color: rgba(255,255,255,0.15);
      }
      .glossario-item.active {
        background-color: rgba(0,123,255,0.6);
        font-weight: bold;
      }
      .glossario-conteudo {
        flex-grow: 1;
        padding: 30px;
        background-color: rgba(255,255,255,0.05);
        border-radius: 8px;
        height: 100%;
        overflow-y: auto;
      }
    ")),
    div(class = "glossario-wrapper",
        div(class = "glossario-menu",
            actionLink(ns("prouni"), "PROUNI", class = "glossario-item"),
            actionLink(ns("bolsa"), "BOLSA FAMILIA", class = "glossario-item"),
            actionLink(ns("luz"), "LUZ PARA TODOS", class = "glossario-item")
        ),
        div(class = "glossario-conteudo",
            uiOutput(ns("conteudo_glossario"))
        )
    )
  )
}

mod_glossario_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected <- reactiveVal("PROUNI")

    observeEvent(input$prouni, { selected("PROUNI") })
    observeEvent(input$bolsa,  { selected("BOLSA FAMILIA") })
    observeEvent(input$luz,    { selected("LUZ PARA TODOS") })

    output$conteudo_glossario <- renderUI({
      switch(selected(),
             "PROUNI" = tagList(
               h3("Programa Universidade para Todos"),
               p("Oferece bolsas de estudo em instituições privadas para estudantes de baixa renda.")
             ),
             "BOLSA FAMILIA" = tagList(
               h3("Programa Bolsa Família"),
               p("Transferência de renda para famílias em situação de pobreza e extrema pobreza.")
             ),
             "LUZ PARA TODOS" = tagList(
               h3("Programa Luz Para Todos"),
               p("Leva energia elétrica a comunidades rurais e regiões isoladas.")
             )
      )
    })
  })
}
