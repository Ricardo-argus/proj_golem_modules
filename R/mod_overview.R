#' mod_overview UI Function
#'
#' @description UI para a aba de Visão Geral (KPIs e Gráficos).
#' @param id O ID interno do Shiny para este módulo.
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

          fluidRow(
            valueBoxOutput(ns("total_bolsas"), width = 4),
            valueBoxOutput(ns("total_universidades"), width = 4),
            valueBoxOutput(ns("percentual_integral"), width = 4)
          ),

          div(style = "overflow-x: auto;",
              fluidRow(
                box(
                  title = "Distribuição por Raça/Cor", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotly::plotlyOutput(ns("bolsas_por_raca"))
                ),
                box(
                  title = "Distribuição por Modalidade", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotly::plotlyOutput(ns("bolsas_por_modalidade"))
                )
              )
          ),

          DT::dataTableOutput(ns("PROUNI"))

        ),

        tabPanel(
          title = "BOLSA FAMÍLIA",
          icon = icon("chart-line"),

          fluidRow(
            valueBoxOutput(ns("total_familias"), width = 4), #quantidade de municipios analisados
            valueBoxOutput(ns("total_brc"), width = 4),   # estado com maior quantidade total de beneficios renda e cidadania
            valueBoxOutput(ns("min_municipio"), width = 4) # municipio com menor quantidade de beneficios recebidos
          ),

          div(style = "overflow-x: auto;",
              fluidRow(
                box(
                  title = "Top 5 Municipios com mais Beneficiários do BPI", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotly::plotlyOutput(ns("bpi_topfive"))
                ),
                box(
                  title = "10 Estados com mais Beneficiários do BVG", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotly::plotlyOutput(ns("bvg_topten"))
                )
              )
          ),

          DT::dataTableOutput(ns("BOLSA FAMILIA"))

        ),

        tabPanel(
          title = "LUZ PARA TODOS",
          icon = icon("chart-line"),

          fluidRow(
            valueBoxOutput(ns("total_domicilios"), width = 4), #quantidade de domicilios analisados
            valueBoxOutput(ns("total_estadoluzpt"), width = 4),   # estado com maior quantidade total de beneficiarios
            valueBoxOutput(ns("maior_anoluzpt"), width = 4) # Ano com mais beneficiarios atendidos
          ),

          div(style = "overflow-x: auto;",
              fluidRow(
                box(
                  title = "Top 5 Estados com menos Beneficiários", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotly::plotlyOutput(ns("luzpt_topfive"))
                ),
                box(
                  title = "Distribuição por Tipo de Programa", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotly::plotlyOutput(ns("tipo_programa_luzpt"))
                )
              )
          ),

          DT::dataTableOutput(ns("LUZ PARA TODOS"))

        )
      )
    )
  )
}

#' mod_overview Server Function
#'
#' @description Server para a aba de Visão Geral.
#' @param id O ID interno do Shiny para este módulo.
#' @param dados_filtrados Um reativo com os dados já filtrados.
#' @param filtros_selecionados Um reativo com os filtros (para o subtítulo).
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @noRd
mod_overview_server <- function(id, dados_filtrados, filtros_selecionados, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # --- KPIs (ValueBoxes) ---
    output$total_bolsas <- renderValueBox({

      num <- nrow(dados_filtrados())


      filtros <- filtros_selecionados()

      filtros_ativos <- c()
      if (filtros$ano != "Todos") { filtros_ativos <- c(filtros_ativos, filtros$ano) }
      if (filtros$uf != "Todos") { filtros_ativos <- c(filtros_ativos, filtros$uf) }
      if (filtros$universidade != "Todos") { filtros_ativos <- c(filtros_ativos, filtros$universidade) }

      if (length(filtros_ativos) > 0) {
        texto_filtro <- paste(filtros_ativos, collapse = " / ")
        subtitulo_final <- paste("Total de Bolsas:", texto_filtro)
      } else {
        subtitulo_final <- "Total de Bolsas (Sem Filtro)"
      }

      valueBox(
        value = format(num, big.mark = "."),
        subtitle = subtitulo_final,
        icon = icon("graduation-cap"),
        color = "purple"
      )
    })

    output$total_universidades <- renderValueBox({
      num <- n_distinct(dados_filtrados()$UNIVERSIDADE_BOLSA)
      valueBox(
        value = num,
        subtitle = "Universidades Envolvidas",
        icon = icon("university"),
        color = "blue"
      )
    })

    output$percentual_integral <- renderValueBox({
      df <- dados_filtrados()
      if (nrow(df) > 0) {
        perc <- sum(df$TIPO_BOLSA == "INTEGRAL", na.rm = TRUE) / nrow(df) * 100
        val <- paste0(format(perc, digits = 2, nsmall = 1), "%")
      } else {
        val <- "N/A"
      }
      valueBox(
        value = val,
        subtitle = "Percentual de Bolsas Integrais",
        icon = icon("percent"),
        color = "green"
      )
    })

    # --- Gráficos (Plotly) ---
    output$bolsas_por_raca <- plotly::renderPlotly({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      raca_counts <- df %>%
        count(RACA_BENEFICIARIO, sort = TRUE) %>%
        na.omit()

      p <- ggplot(raca_counts, aes(x = reorder(RACA_BENEFICIARIO, n), y = n, fill = RACA_BENEFICIARIO)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(title = "Distribuição por Raça/Cor\ndo Beneficiário",
             x = "", y = "Número de Bolsas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "#555555"), # Linhas de grade suaves
          panel.grid.minor = element_blank()
        )

      # ATUALIZAÇÃO: Aplicar template escuro do Plotly
      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)", # Fundo do papel transparente
          plot_bgcolor = "rgba(0,0,0,0)",  # Fundo do gráfico transparente
          legend = list(font = list(color = "white")) # Cor do texto da legenda
        )
    })

    output$bolsas_por_modalidade <- plotly::renderPlotly({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      modalidade_counts <- df %>%
        count(MODALIDADE_ENSINO_BOLSA) %>%
        na.omit()

      p <- ggplot(modalidade_counts, aes(x = MODALIDADE_ENSINO_BOLSA, y = n, fill = MODALIDADE_ENSINO_BOLSA)) +
        geom_col(show.legend = FALSE) +
        labs(title = "Distribuição por Modalidade\nde Ensino",
             x = "Modalidade", y = "Número de Bolsas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.x = element_blank(), # Remover grades verticais
          panel.grid.major.y = element_line(color = "#555555"), # Grades horizontais suaves
          panel.grid.minor = element_blank()
        )


      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)", # Fundo do papel transparente
          plot_bgcolor = "rgba(0,0,0,0)",  # Fundo do gráfico transparente
          legend = list(font = list(color = "white")) # Cor do texto da legenda
        )
    })

    # KPI Bolsa Familia

    #VALUEBOXES

    output$total_familias <- renderValueBox({
      total <- fct_get_bolsafm(con)$total_familias
      valueBox(
        value = total[[1]],
        subtitle = "Municipios Analisados",
        icon = icon("home"),
        color = "purple"
      )
    })

    output$total_brc <- renderValueBox({
      total <- fct_get_bolsafm(con)$total_brc
      valueBox(
        value = total[[1]],
        subtitle = "Estado com Mais Beneficio Complementar",
        icon = icon("home"),
        color = "blue"
      )
    })



    #inserir graficos get_luz_pt e gráficos bolsafm

    output$total_domicilios <- renderValueBox({
      total <- fct_get_luzpt(con)$total_domicilios
      valueBox(
        value = total[[1]],
        subtitle = "Total de Domicilios Analisados",
        icon = icon("home"),
        color = "purple"
      )
    })

    output$total_estadoluzpt <- renderValueBox({
      total <- fct_get_luzpt(con)$total_estadoluzpt
      valueBox(
        value = total[[1]],
        subtitle = "Estado com Mais Domicilios atendidos",
        icon = icon("home"),
        color = "blue"
      )
    })

    output$maior_anoluzpt <- renderValueBox({
      total <- fct_get_luzpt(con)$maior_anoluzpt
      valueBox(
        value = total[[1]],
        subtitle = "Ano com Mais Domicilios Atendidos",
        icon = icon("home"),
        color = "yellow"
      )
    })

  })

}

