#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bolsafamilia_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
            plotlyOutput(ns("bpi_topfive"))
          ),
          box(
            title = "10 Estados com mais Beneficiários do BVG", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("bvg_topten"))
          )
        )
    )
  )
}

#' bolsafamilia_overview Server Functions
#'
#' @noRd
mod_bolsafamilia_overview_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # KPI Bolsa Familia

    #VALUEBOXES

    output$total_familias <- renderValueBox({
      total <- fct_get_bolsafm(con)$total_familias
      valueBox(
        value = formatC(as.numeric(total[[1]]), format = "d", big.mark = "."),
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


  })
}

