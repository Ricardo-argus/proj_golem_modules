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
            valueBoxOutput(ns("total_familias"), width = 4),
            valueBoxOutput(ns("total_brc"), width = 4),
            valueBoxOutput(ns("min_municipio"), width = 4)
          ),

          div(style = "overflow-x: auto;",
              fluidRow(
                box(
                  title = "Top 5 Estados com mais Beneficiários do BPI", status = "primary", solidHeader = TRUE,
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
            valueBoxOutput(ns("total_domicilios"), width = 4),
            valueBoxOutput(ns("total_estadoluzpt"), width = 4),
            valueBoxOutput(ns("maior_anoluzpt"), width = 4)
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

    #plots in plotly
    output$bolsas_por_raca <- plotly::renderPlotly({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      raca_counts <- df %>%
        count(RACA_BENEFICIARIO, sort = TRUE) %>%
        na.omit()

      p <- ggplot(raca_counts, aes(x = reorder(RACA_BENEFICIARIO, n), y = n, fill = RACA_BENEFICIARIO)) +
        geom_col(width = 1.0, show.legend = FALSE) +
        geom_text(aes(label = format(n, big.mark = ".")),
                  hjust = -0.1, color = "white", size = 4)+
        coord_flip() +
        labs(title = "Distribuição por Raça/Cor\ndo Beneficiário",
             x = "", y = "Número de Bolsas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.text.x = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "#555555"),
          panel.grid.minor = element_blank()
        )


      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(font = list(color = "white"))
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
        geom_text(aes(label = format(n, big.mark = ".")),
                  vjust = -0.5, color = "white", size = 4) +
        labs(title = "Distribuição por Modalidade\nde Ensino",
             x = "Modalidade", y = "Número de Bolsas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.minor = element_blank()
        )


      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(font = list(color = "white"))
        )
    })

    # KPI Bolsa Familia

    #VALUEBOXES

    output$total_familias <- renderValueBox({
      total <- fct_get_bolsafm(con)$total_familias
      valueBox(
        value = format(total[[1]], big.mark = ".", decimal.mark = ","),
        subtitle = "Quantidade de Municipios Analisados",
        icon = icon("home"),
        color = "purple"
      )
    })

    output$total_brc <- renderValueBox({
      total <- fct_get_bolsafm(con)$total_brc
      valueBox(
        value = total[[1]],
        subtitle = "Estado com Mais Beneficio Complementar",
        icon = icon("arrow-up"),
        color = "green"
      )
    })

    #kpi min_estado

    output$min_municipio <- shinydashboard::renderValueBox({
      query <- "
    SELECT  minimo.estado FROM
    (SELECT
        bfi.estado,
            (COALESCE(SUM(bp.qtd_ben_bpi), 0)
            + COALESCE(SUM(brc.qtd_ben_brc), 0)
            + COALESCE(SUM(brc.qtd_ben_bco), 0)
            + COALESCE(SUM(bsf.qtd_ben_bvg), 0)
            + COALESCE(SUM(bsf.qtd_ben_bvn), 0)
            + COALESCE(SUM(bsf.qtd_ben_bva), 0)
            + COALESCE(SUM(bsf.qtd_ben_bv), 0)
            + COALESCE(SUM(bsf.qtd_ben_bf), 0)
            + COALESCE(SUM(bvs.qtd_ben_bv), 0)
            + COALESCE(SUM(bvs.qtd_ben_bva), 0)
            + COALESCE(SUM(bvs.qtd_ben_bvbva), 0))
            AS total_benef
    FROM benef_primeirainfancia bp
    LEFT JOIN beneficio_ano ba ON ba.id_familias = bp.id_familias
    LEFT JOIN bolsa_familia_ibge bfi ON bfi.id_familias = bp.id_familias
    LEFT JOIN benef_renda_complementar brc ON brc.id_familias = bp.id_familias
    LEFT JOIN benef_somafamiliares bsf ON bsf.id_familias = bp.id_familias
    LEFT JOIN benef_variaveis_soma bvs ON bvs.id_familias = bp.id_familias
    GROUP BY bfi.estado
    ORDER BY total_benef ASC
    LIMIT 1) AS minimo;
  "

      df <- dbGetQuery(con, query)
      req(nrow(df) > 0)

      estado_min <- df$estado[1]

      shinydashboard::valueBox(
        value = estado_min,
        subtitle = "Estado com Menor Número de Beneficiários",
        icon = shiny::icon("arrow-down"),
        color = "red"
      )
    })


    output$bpi_topfive <- plotly::renderPlotly({
      query <- "
    SELECT top.estado, top.total_bpi FROM (
      SELECT SUM(bp.qtd_ben_bpi) as total_bpi, bf.estado
      FROM bolsa_familia_ibge bf
      LEFT JOIN benef_primeirainfancia bp ON bp.id_familias = bf.id_familias
      GROUP BY bf.estado
      ORDER BY total_bpi DESC
      LIMIT 5
    ) top;
  "

      df <- dbGetQuery(con, query)
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = reorder(estado, total_bpi),
                          y = total_bpi,
                          fill = estado)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = format(total_bpi, big.mark = ".")),
                  hjust = -0.1, color = "white", size = 4) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
        labs(title = "Top 5 Estados com Mais Beneficiários\nPrimeira Infância (BPI)",
             x = "Estado", y = "Total de Familias Atendidas") +  # mantém apenas eixo Y
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
          axis.title = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.text.x = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.major.x = element_line(color = "#333333"),
          panel.grid.minor = element_blank()
        )

      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 150, r = 50, t = 80, b = 60),
          showlegend = FALSE
        )
    })


    output$bvg_topten <- plotly::renderPlotly({
      query <- "
    SELECT top.estado, top.total_bvg FROM (
      SELECT SUM(bsf.qtd_ben_bvg) as total_bvg, bf.estado
      FROM bolsa_familia_ibge bf
      LEFT JOIN benef_somafamiliares bsf ON bsf.id_familias = bf.id_familias
      GROUP BY bf.estado
      ORDER BY total_bvg DESC
      LIMIT 10
    ) top;
  "

      df <- dbGetQuery(con, query)
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = reorder(estado, total_bvg),
                          y = total_bvg,
                          fill = estado)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = format(total_bvg, big.mark = ".")),
                  hjust = -0.1, color = "white", size = 4) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
        labs(title = "Top 10 Estados com Mais Beneficiários\nBenefício Variável Familiar (BVG)",
             x = "Estado", y = "Total de Familias Atendidas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.major.x = element_line(color = "#333333"),
          panel.grid.minor = element_blank()
        )

      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 150, r = 50, t = 80, b = 60),
          showlegend = FALSE
        )
    })





    # KPI luz_pt

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
        icon = icon("arrow-up"),
        color = "green"
      )
    })

    output$maior_anoluzpt <- renderValueBox({
      total <- fct_get_luzpt(con)$maior_anoluzpt
      valueBox(
        value = total[[1]],
        subtitle = "Ano com Mais Domicilios Atendidos",
        icon = icon("home"),
        color = "yellow")
    })

    output$luzpt_topfive <- plotly::renderPlotly({
      query <- "
    SELECT eb.estado, SUM(db.qtd_domicilios) as total_domicilios
    FROM luz_domicilios_beneficiarios db
    LEFT JOIN luz_estado_beneficiarios eb on db.id_beneficiarios = eb.id_beneficiarios
    GROUP BY eb.estado
    ORDER BY total_domicilios ASC
    LIMIT 5;
  "

      df <- dbGetQuery(con, query)
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = reorder(estado, total_domicilios),
                          y = total_domicilios,
                          fill = estado)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = format(total_domicilios, big.mark = ".")),
                  hjust = -0.1, color = "white", size = 4) +
        coord_flip() +
        labs(title = "Top 5 Estados com Menos Beneficiários",
             x = "Estado", y = "Total de Domicílios") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
          axis.title = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.text.x = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.major.x = element_line(color = "#333333"),
          panel.grid.minor = element_blank()
        )

      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 150, r = 50, t = 80, b = 60),
          showlegend = FALSE
        )
    })

    output$tipo_programa_luzpt <- plotly::renderPlotly({
      query <- "
    SELECT lpb.programa, SUM(db.qtd_domicilios) AS total_por_programa
    FROM luz_domicilios_beneficiarios db
    LEFT JOIN luz_programa_beneficiarios lpb
           ON db.id_beneficiarios = lpb.id_beneficiarios
    GROUP BY lpb.programa
    ORDER BY total_por_programa DESC;
  "

      df <- dbGetQuery(con, query)
      req(nrow(df) > 0)

      df$programa <- recode(df$programa,
                            "LPT - Regioes Remotas da Amazonia Legal" = "Amazonia Legal",
                            "LPT - Rural" = "Rural"
      )

      p <- ggplot(df, aes(x = programa, y = total_por_programa, fill = programa)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = format(total_por_programa, big.mark = ".")),
                  vjust = -0.5, color = "white", size = 4) +
        labs(title = "Distribuição por Tipo de Programa",
             x = "Programa", y = "Total de Domicílios",
             fill = NULL) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.minor = element_blank()
        )

      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 1,
            font = list(color = "white", size = 12)
          )
        )
    })
  })

}
