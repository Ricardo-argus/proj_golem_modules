#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # gerenciar os recursos externos
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
          "PROJETOS"
        ),
        titleWidth = 400
      ),

      dashboardSidebar(
        width = 400,
        sidebarMenu(
          menuItem("Visão Geral", tabName = "overview", icon = icon("dashboard")),
          menuItem("Dados Completos", tabName = "raw_data", icon = icon("table")),
          menuItem("Contato", tabName = "reports", icon = icon("envelope")),
          menuItem("Glossário", tabName = "gloss", icon = icon("book"))
        ),

        hr(),

        #Call module for filters
        mod_filtros_ui("filtros_1"),

        hr(),

        br(), br(),

        tags$div(
          style = "color: white; background-color: #A9A9A9; padding: 15px; border-radius: 10px; text-align: center;",
          icon("project-diagram"),
          tags$h4("RShiny Golem Application Project"),
          tags$p("Developed by Hernandes"),
          tags$p("Guarulhos 2025")
        )
      ),

      dashboardBody(
        tabItems(
          # Aba 1: Visão Geral
          tabItem(tabName = "overview",

                 #Call module overview ui
                  mod_overview_ui("overview_1")
          ),

          # Aba 2: Tabela de Dados Brutos
          tabItem(tabName = "raw_data",

                  # 3. CHAMADA DA UI DO MÓDULO DE DADOS BRUTOS
                  # Damos um ID único: "dados_brutos_1"
                  mod_dados_brutos_ui("dados_brutos_1")
          )
        )
      )
    )
  )
}

# -----------------------------------------------------------------
# O ARQUIVO golem_add_external_resources() NÃO MUDA
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
