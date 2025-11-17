#' mod_db_pool_init: Inicializa o pool de conexão com o banco
#'
#' @param config_path Caminho para o arquivo golem-config.yml
#' @return Objeto pool::Pool

mod_db_pool_init <- function(config_path = "inst/golem-config.yml"){
  tryCatch({
    db_config <- config::get("db", file = config_path)

    pool::dbPool(
      drv = RMariaDB::MariaDB(),
      host = db_config$host,
      port = as.integer(db_config$port),
      user = db_config$user,
      password = db_config$password,
      dbname = db_config$dbname
    )
  }, error = function(e) {
    stop(paste("Erro ao criar conexão com banco de dados:", e$message))
  })
}


#' mod_db_pool_teardown: Encerra o pool de conexão
#'
#' @param pool Objeto pool::Pool
mod_db_pool_teardown <- function(pool) {
  if (!is.null(pool)) {
    pool::poolClose(pool)
  }
}
