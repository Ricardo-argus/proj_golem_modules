#' fct_get_bolsafm: query from DB Bolsa Família
#'
#' @param pool Pool already activated
#' @return df with Queries result
#' @import DBI dplyr
#' @noRd
fct_get_bolsafm <- function(pool, ano = NULL, uf = NULL) {

  quantidade_municipio <- "SELECT COUNT(*) FROM (
SELECT DISTINCT(codigo_ibge)FROM bolsa_familia_ibge) total_municipios;
"
  quantidade_totalbrc <-  "SELECT
    maximo.estado
FROM
    (SELECT
        SUM(brc.qtd_ben_brc) AS total_brc, bf.estado
    FROM
        bolsa_familia_ibge bf
    LEFT JOIN benef_renda_complementar brc ON brc.id_familias = bf.id_familias
    GROUP BY bf.estado
    ORDER BY total_brc DESC
    LIMIT 1) maximo;"

  min_estado_benef <- "SELECT  minimo.estado FROM
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
    LIMIT 1) AS minimo; "

  topfive_bpi <- "SELECT top.estado, top.total_bpi FROM (
                  SELECT SUM(bp.qtd_ben_bpi) as total_bpi, bf.estado  FROM bolsa_familia_ibge bf
                  LEFT JOIN benef_primeirainfancia bp ON bp.id_familias = bf.id_familias
                  GROUP BY bf.estado
                  ORDER BY total_bpi DESC
                  LIMIT 5) top;"

  topten_bvg <- "SELECT top.estado, top.total_bvg FROM (
                  SELECT SUM(bsf.qtd_ben_bvg) as total_bvg, bf.estado  FROM bolsa_familia_ibge bf
                  LEFT JOIN benef_somafamiliares bsf ON bsf.id_familias = bf.id_familias
                  GROUP BY bf.estado
                  ORDER BY total_bvg DESC
                  LIMIT 10) top;"

    list(

      total_familias = dbGetQuery(pool, quantidade_municipio ),
      total_brc  = dbGetQuery(pool, quantidade_totalbrc ),
      min_estado = dbGetQuery(pool, min_estado_benef),
      bpi_topfive = dbGetQuery(pool, topfive_bpi ),
      bvg_topten = dbGetQuery(pool, topten_bvg)

    )
}
