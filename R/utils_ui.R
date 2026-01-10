# footer config

utils_footer <- function(){
  tags$footer(
    style = "
    position: fixed;
    bottom: 0;
    left: 0;
    width: 100%;
    background-color: #F8F9FA;
    padding: 10px;
    text-align: center;
    font-size: 14px;
    color: #6c757d;
    border-top: 1px solid #dee2e6;
    z-index: 9999;
    ",
    tags$b("₢ 2026 Ricardo Hernandes - Shiny Application Brazilian Social Programs")
  )
}
