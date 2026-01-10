#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),

    dashboardPage(
      skin = "black",

      dashboardHeader(
        title = tags$span(
          tags$img(
            src = "www/logo_gov.png",
            height = "40px",
            style = "vertical-align: middle; margin-right: 10px;"
          ),
          "PROGRAMAS"
        ),
        titleWidth = 400
      ),

      dashboardSidebar(
        width = 400,
        sidebarMenu(
          menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
          menuItem("Dados Completos", tabName = "raw_data", icon = icon("table")),
          menuItem("Contato", tabName = "Contatos", icon = icon("envelope")),
          menuItem("Glossário", tabName = "gloss", icon = icon("book"))
        ),

        hr(),

        mod_filtros_ui("filtros_1"),

        div(
          style = "display: flex; justify-content: center; margin-top: 10px;",
          actionButton("refresh_app",
                     label = NULL,
                     icon = icon("sync-alt"),
                     style = "
                      background-color: #343a40;
                          color: white;
                          border: none;
                          border-radius: 6px;
                          padding: 6px 12px;
                          font-size: 14px;
                        ",
          )
        ),

        hr(),

        br(), br(),

        tags$div(
          style = "color: white; background-color: #A9A9A9; padding: 20px; border-radius: 10px; text-align: center;",
          tags$h4("Conecte-se comigo"),
          tags$a(
            href = "https://github.com/ricardo-argus", target = "_blank",
            title = "Acesse meu repositorio no Github",
            icon("github", class = "fa-2x social-icon github-icon"),
            style = "margin-right: 15px; color: white;"
          ),
          tags$a(
            href = "https://www.linkedin.com/in/ricardo-hernandes-05b93218a/", target = "_blank",
            title = "Veja meu perfil no linkedIn",
            icon("linkedin", class = "fa-2x social-icon linkedin-icon"),
            style = "color: white;"
          )
        )
      ),

      dashboardBody(
        tags$head(
          tags$style(HTML("
      .box.box-solid.box-primary > .box-header {
        background-color: #778899 !important;
        color: black !important;
      }
      .box.box-solid.box-primary {
        border: 1px solid #4682B4 !important;
      }
    ")),
          tags$script(HTML("Shiny.addCustomMessageHandler('refreshApp', function(message) {
                           location.reload();
                           });
                           "))
        ),
        tagList(
          tabItems(
            tabItem(tabName = "overview", mod_overview_ui("overview_1")),
            tabItem(tabName = "raw_data", mod_dados_brutos_ui("dados_brutos_1")),
            tabItem(tabName = "Contatos", mod_contato_mod_ui("contatos_1")),
            tabItem(tabName = "gloss", mod_glossario_ui("glossario_1"))
          ),
          fluidRow(
            column(width = 12, utils_footer())
          )
        )
      )
    )
  )
}


# -----------------------------------------------------------------
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'GOVBR: Projetos'
    ),

    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")

  )
}
