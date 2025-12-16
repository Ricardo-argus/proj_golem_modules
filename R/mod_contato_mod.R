#' contato_mod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contato_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("#%s .well { color: black !important; }", ns("info_adicional")))),
    fluidRow(
      column(12, align = "center",
             h2("Fale Conosco")
      ),
      column(12, offset = 2,
             p("Este dashboard foi desenvolvido com o objetivo de promover transparência e facilitar o acesso a dados sobre programas sociais brasileiros, como PROUNI, Bolsa Família e Luz Para Todos."),
             actionButton(ns("mostrar_info"), "Mostrar mais informações"),
             uiOutput(ns("info_adicional")),
             br(),
             p("Telefone? "),
             p("E-mail: ", a("contato@programassociais.gov.br", href = "mailto:contato@programassociais.gov.br")),
             p("Site PROUNI: ", a("www.programassociais.gov.br", href = "https://www.programassociais.gov.br", target = "_blank")),
             p("Site BOLSA FAMILIA: ", a("www.programassociais.gov.br", href = "https://www.programassociais.gov.br", target = "_blank")),
             p("Site LUZ PARA TODOS: ", a("www.programassociais.gov.br", href = "https://www.programassociais.gov.br", target = "_blank"))
      )
    ),

    br(), br(),

    fluidRow(
      column(6, offset = 2,
             textInput(ns("nome"), "Nome"),
             textInput(ns("email"), "Email"),
             textAreaInput(ns("mensagem"), "Mensagem", "", rows = 5),
             actionButton(ns("enviar"), "Enviar")
      )
    )
  )
}
#' contato_mod Server Functions
#'
#' @noRd
mod_contato_mod_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Estado reativo para controlar visibilidade
    estado <- reactiveValues(mostrar = FALSE)

    observeEvent(input$mostrar_info, {
      estado$mostrar <- !estado$mostrar  # alterna entre TRUE e FALSE
    })

    output$info_adicional <- renderUI({
      if (estado$mostrar) {
        wellPanel(
          h4("Informações adicionais"),
          p("Se você gostou desta iniciativa e tem interesse em colaborações, novas ideias de visualizações, ou deseja desenvolver dashboards personalizados, será um prazer conversar!"),
          p("Conecte-se comigo pelo LinkedIn: ",
            a("linkedin.com/in/ricardohernandes", href = "https://linkedin.com/in/ricardohernandes", target = "_blank")),
          p("Ou se preferir, envie um texto no inbox abaixo!")
        )
      } else {
        NULL
      }
    })

    observeEvent(input$enviar, {
      showModal(modalDialog(
        title = "Mensagem Enviada!",
        "Obrigado pelo contato, retornaremos em breve.",
        easyClose = TRUE
      ))
    })
  })
}
