#' luzpt_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_luzpt_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
            plotlyOutput(ns("luzpt_topfive"))
          ),
          box(
            title = "Distribuição por Tipo de Programa", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("tipo_programa_luzpt"))
          )
        )
    )
  )
}

#' luzpt_overview Server Functions
#'
#' @noRd
mod_luzpt_overview_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # KPI luz_pt

    output$total_domicilios <- renderValueBox({
      total <- fct_get_luzpt(con)$total_domicilios
      valueBox(
        value = formatC(as.numeric(total[[1]]), format = "d", big.mark = "."),
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


