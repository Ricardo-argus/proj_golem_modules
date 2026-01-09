#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import pool
#' @import RMariaDB
#' @import DBI
#' @noRd

#Database Connection Module

app_server <- function( input, output, session ) {
  con <- mod_db_pool_init()

  session$onSessionEnded(function() {
    mod_db_pool_teardown(con)
  })


  # Chama o módulo de filtros e armazena os filtros reativos que ele RETORNA
  filtros_selecionados <- mod_filtros_server("filtros_1", con = con)

  observeEvent(input$refresh_app, {
    session$sendCustomMessage("refreshApp", "reload")
  })


  # --- 3. Reactive Data

  dadosCompletos <- reactive({
    query <- "
       SELECT
         d.id_bolsista, d.ANO_CONCESSAO_BOLSA, d.SEXO_BENEFICIARIO, d.RACA_BENEFICIARIO,
         u.UF_BENEFICIARIO, bu.UNIVERSIDADE_BOLSA, tb.TIPO_BOLSA, mb.MODALIDADE_ENSINO_BOLSA,
         trb.TURNO_BOLSA, cb.CURSO_BOLSISTA
       FROM
         dados_bolsistas d
       LEFT JOIN UF_bolsas u ON d.id_bolsista = u.id_bolsista
       LEFT JOIN bolsas_universidades bu ON d.id_bolsista = bu.id_bolsista
       LEFT JOIN tipo_bolsa tb ON d.id_bolsista = tb.id_bolsista
       LEFT JOIN modalidade_bolsa mb ON d.id_bolsista = mb.id_bolsista
       LEFT JOIN turno_bolsa trb ON d.id_bolsista = trb.id_bolsista
       LEFT JOIN curso_bolsista cb ON d.id_bolsista = cb.id_bolsista
     "
    dbGetQuery(con, query)
  })

  dadosbolsafamilia <- reactive({

    query <- "SELECT
                bp.id_familias,
                ba.ano_beneficio,
                bfi.estado,
                bfi.codigo_ibge,
                bp.qtd_ben_bpi,
                brc.qtd_ben_brc,
                brc.qtd_ben_bco,
                bsf.qtd_ben_bvg,
                bsf.qtd_ben_bvn,
                bsf.qtd_ben_bva,
                bsf.qtd_ben_bv,
                bsf.qtd_ben_bf,
                bvs.qtd_ben_bvbva
              FROM
                benef_primeirainfancia bp
              LEFT JOIN beneficio_ano ba ON ba.id_familias = bp.id_familias
              LEFT JOIN bolsa_familia_ibge bfi ON bfi.id_familias = bp.id_familias
              LEFT JOIN benef_renda_complementar brc ON brc.id_familias = bp.id_familias
              LEFT JOIN benef_somafamiliares bsf ON bsf.id_familias = bp.id_familias
              LEFT JOIN benef_variaveis_soma bvs ON bvs.id_familias = bp.id_familias"

    dbGetQuery(con,query)
  })

  dadosluzpt <- reactive({

    query <- "SELECT
                a.id_beneficiarios,
                a.mes_atendimento,
                a.ano_atendimento,
                a.ano_homologacao,
                db.qtd_domicilios,
                eb.estado,
                pb.programa
              FROM
                luz_ano a
              LEFT JOIN luz_domicilios_beneficiarios db on a.id_beneficiarios = db.id_beneficiarios
              LEFT JOIN luz_estado_beneficiarios eb on a.id_beneficiarios = eb.id_beneficiarios
              LEFT JOIN luz_programa_beneficiarios pb on a.id_beneficiarios = pb.id_beneficiarios
"
    dbGetQuery(con,query)
  })





  dadosFiltrados <- reactive({
    data <- dadosCompletos()
    filtros <- filtros_selecionados()

    # Validação para garantir que os filtros estejam prontos
    req(filtros)

    if (filtros$ano != "Todos") {
      data <- data %>% filter(ANO_CONCESSAO_BOLSA == filtros$ano)
    }

    if (filtros$uf != "Todos") {
      data <- data %>% filter(UF_BENEFICIARIO == filtros$uf)
    }

    if (filtros$universidade != "Todos") {
      data <- data %>% filter(UNIVERSIDADE_BOLSA == filtros$universidade)
    }

    data
  })

  #  CHAMADA DOS MÓDULOS DE CONTEÚDO

  # Passa os dados filtrados E os filtros selecionados para o módulo de overview
  mod_overview_server("overview_1",
                      dados_filtrados = dadosFiltrados,
                      filtros_selecionados = filtros_selecionados,
                      con = con,
                      dados_luz = dadosluzpt)

  # Passa (apenas) os dados filtrados para o módulo de tabela
  mod_dados_brutos_server("dados_brutos_1",
                          dados_filtrados = dadosFiltrados,
                          dados_luz = dadosluzpt,
                          dados_bf = dadosbolsafamilia,
                          con = con)

  mod_contato_mod_server("contatos_1", con = con)


  mod_glossario_server("glossario_1")


}
