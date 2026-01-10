#' fct_get_luzpt: query from Db LUZ para todos
#'
#' @param pool Pool already activated
#' @return df with query results
#' @import DBI dplyr
#' @noRd
fct_get_luzpt <- function(pool, ano = NULL, uf = NULL) {

  quantidade_domicilios <- "SELECT COUNT(qtd_domicilios) FROM luz_domicilios_beneficiarios;"
    estado_benef <-  "SELECT eb.estado, SUM(db.qtd_domicilios) as total_domicilios FROM luz_domicilios_beneficiarios db
                      LEFT JOIN luz_estado_beneficiarios eb on db.id_beneficiarios = eb.id_beneficiarios
                      GROUP BY eb.estado
                      ORDER BY total_domicilios DESC
                      LIMIT 1;"
    ano_maior_benef <- "SELECT a.ano_atendimento, SUM(db.qtd_domicilios) AS total_atendidos FROM luz_ano a
                        LEFT JOIN luz_domicilios_beneficiarios db ON a.id_beneficiarios = db.id_beneficiarios
                        GROUP BY a.ano_atendimento
                        ORDER BY total_atendidos DESC
                        LIMIT 1;"
    topfive_min_benef <- "SELECT eb.estado, SUM(db.qtd_domicilios) as total_domicilios FROM luz_domicilios_beneficiarios db
                          LEFT JOIN luz_estado_beneficiarios eb on db.id_beneficiarios = eb.id_beneficiarios
                          GROUP BY eb.estado
                          ORDER BY total_domicilios ASC
                          LIMIT 5;"
    program_type <- "SELECT lpb.programa, SUM(db.qtd_domicilios) AS total_por_programa FROM luz_domicilios_beneficiarios db
                          LEFT JOIN luz_programa_beneficiarios lpb ON db.id_beneficiarios = lpb.id_beneficiarios
                          GROUP BY lpb.programa
                          ORDER BY total_por_programa  DESC"

    list(

      total_domicilios= dbGetQuery(pool, quantidade_domicilios ),
      total_estadoluzpt  = dbGetQuery(pool, estado_benef ),
      maior_anoluzpt = dbGetQuery(pool, ano_maior_benef),
      luzpt_topfive = dbGetQuery(pool, topfive_min_benef ),
      tipo_programa_luzpt = dbGetQuery(pool, program_type)

    )
}
