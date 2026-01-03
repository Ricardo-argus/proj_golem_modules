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
             h2("Saiba mais")
      ),
      column(10, offset = 1,
             p("Este dashboard é parte de um projeto pessoal que desenvolvi com o objetivo de promover a transparência e facilitar o acesso a dados sobre programas sociais brasileiros, como PROUNI, Bolsa Família e Luz Para Todos."),
             p("A ideia surgiu da minha paixão por visualização de dados e pelo potencial que a tecnologia tem de aproximar as pessoas das políticas públicas que impactam suas vidas. Este trabalho também faz parte do meu portfólio e será divulgado no LinkedIn como uma forma de compartilhar conhecimento e abrir portas para novas conexões."),
             actionButton(ns("mostrar_info"), "Mostrar mais informações"),
             uiOutput(ns("info_adicional")),
             br(),
             h3("Envie sua mensagem"),
             p("Use o formulário abaixo para me mandar uma mensagem diretamente. Fico à disposição para conversar!")
    ),

    br(), br(),

    fluidRow(
      column(10, offset = 1,
             textInput(ns("nome"), "Nome", width = "50%"),
             textInput(ns("email"), "Email", width = "50%"),
             textAreaInput(ns("mensagem"), "Mensagem", "", rows = 5, width = "40%"),
             actionButton(ns("enviar"), "Enviar")
        )
      )
    )
  )
}
#' contato_mod Server Functions
#'
#' @noRd
mod_contato_mod_server <- function(id,con){
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
            a("linkedin.com/in/ricardohernandes", href = "https://www.linkedin.com/in/ricardo-hernandes-05b93218a/", target = "_blank")),
          p("Ou se preferir, envie um texto no inbox abaixo!")
        )
      } else {
        NULL
      }
    })

    observeEvent(input$enviar, {

      if(input$nome == ""|| input$email == "" || input$mensagem == ""){
        showModal(modalDialog(
          title = "Erro",
          'Por favor, preencha todos os campos antes de enviar.',
          easyClose = TRUE
        ))
      }else{

      dbExecute(con,
                "INSERT INTO visitantes (nome,email,mensagem) VALUES (?,?,?)",
                params = list(input$nome, input$email, input$mensagem)
                )


      showModal(modalDialog(
        title = "Mensagem Enviada!",
        "Obrigado pelo contato, retornaremos em breve.",
        easyClose = TRUE
      ))
      }
    })
  })
}
