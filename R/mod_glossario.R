#' glossario UI Function
#'
#' @description A shiny Module com menu lateral customizado.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_glossario_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .glossario-wrapper {
        display: flex;
        height: 85vh;
        align-items: center;
        justify-content: flex-start;
        gap: 40px;
        color: white;
      }
      .glossario-menu {
        display: flex;
        flex-direction: column;
        padding: 15px;
        border: 1px solid white;
        border-radius: 8px;
        background-color: rgba(255,255,255,0.05);
        width: 220px;
        height: fit-content;
      }
      .glossario-item {
        padding: 10px;
        margin: 5px 0;
        cursor: pointer;
        border-radius: 6px;
        transition: background-color 0.3s;
      }
      .glossario-item:hover {
        background-color: rgba(255,255,255,0.15);
      }
      .glossario-item.active {
        background-color: rgba(0,123,255,0.6);
        font-weight: bold;
      }
      .glossario-conteudo {
        flex-grow: 1;
        padding: 30px;
        background-color: rgba(255,255,255,0.05);
        border-radius: 8px;
        height: 100%;
        overflow-y: auto;
      }
      .white-box {
        background-color: white;
        border: 1px solid #ccc;
        border-radius: 8px;
        padding: 15px;
        color: black;
        width: 100%;
        margin-top: 20px;
      }
      .texto-centralizado {
        text-align: center;
      }
    ")),
    div(class = "glossario-wrapper",
        div(class = "glossario-menu",
            actionLink(ns("prouni"), "PROUNI", class = "glossario-item"),
            actionLink(ns("bolsa"), "BOLSA FAMILIA", class = "glossario-item"),
            actionLink(ns("luz"), "LUZ PARA TODOS", class = "glossario-item")
        ),
        div(class = "glossario-conteudo",
            uiOutput(ns("conteudo_glossario"))
        )
    )
  )
}

mod_glossario_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected <- reactiveVal("PROUNI")

    observeEvent(input$prouni, { selected("PROUNI") })
    observeEvent(input$bolsa,  { selected("BOLSA FAMILIA") })
    observeEvent(input$luz,    { selected("LUZ PARA TODOS") })

    output$conteudo_glossario <- renderUI({
      switch(selected(),
             "PROUNI" = tagList(
               h3("Programa Universidade para Todos"),
               p("Oferece bolsas de estudo em instituições privadas para estudantes de baixa renda."),
               div(class = 'white-box texto-centralizado',
                   p(strong("MODALIDADE DE ENSINO")),   # título em negrito
                   p("Modalidade de ensino em uma faculdade é o formato pelo qual o curso é oferecido — presencial, semipresencial ou a distância (EaD). Cada modalidade define como as aulas acontecem, a carga horária presencial e online, e o nível de flexibilidade para o estudante."),
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("TIPO DE BOLSA")),          # título em negrito
                   p("O PROUNI oferece bolsas integrais e parciais (50%) para estudantes de baixa renda em instituições privadas de ensino superior.")
               )
             ),
             "BOLSA FAMILIA" = tagList(
               h3("Programa Bolsa Família"),
               p("Transferência de renda para famílias em situação de pobreza e extrema pobreza."),
               div(class = 'white-box texto-centralizado',
                   p(strong("CODIGO IBGE")),   # título em negrito
                   p("é um número único atribuído pelo Instituto Brasileiro de Geografia e Estatística a cada município do Brasil.
Ele serve para identificar oficialmente cidades e localidades em cadastros, pesquisas e sistemas governamentais.
Facilita a organização de dados estatísticos e administrativos em nível nacional.
É usado em programas sociais, censos e registros oficiais para garantir padronização
"),
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BPI")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bpi (Benefício Primeira Infância)
Indica a Quantidade de benefícios pagos a crianças de 0 a 6 anos.
Oferece apoio adicional para o desenvolvimento infantil,
Ajuda nas despesas básicas da primeira infância,
Valoriza a fase mais crítica da formação da criança e
Promove inclusão e cuidado desde os primeiros anos.
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BRC")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_brc (Benefício de Renda de Cidadania)
Indica a quantidade de benefícios básicos pagos por pessoa da família.
É o valor fixo que compõe a renda mínima garantida,
Cada integrante da família recebe esse benefício,
Funciona como base do programa Bolsa Família e
Assegura que todas as famílias tenham renda mínima.
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BCO")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bco (Benefício Complementar)
Mostra o número de famílias que receberam complemento de renda.
É pago quando a soma dos benefícios não atinge o valor mínimo,
Funciona como ajuste para garantir dignidade financeira,
Evita que famílias fiquem abaixo da linha de pobreza e
Reforça a segurança social do programa.
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BVG")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bvg (Benefício Variável Gestante)
informa o Número de gestantes atendidas com benefício extra.
Auxilia nos custos da gravidez e saúde da mãe,
Estimula o acompanhamento pré-natal obrigatório,
Contribui para reduzir riscos na gestação e
Promove proteção social às famílias em expansão.
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BVN")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bvn (Benefício Variável Nutriz)
Informa a Quantidade de mães em fase de amamentação atendidas.
Apoia a saúde da mãe e do bebê nos primeiros meses,
Complementa a renda familiar nesse período delicado,
Valoriza o aleitamento materno como política pública e
Garante suporte financeiro durante a nutrição inicial
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BVA")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bva (Benefício Variável Adolescente)
Representa Número de adolescentes de 12 a 18 anos beneficiados.
Incentiva a permanência escolar e reduz evasão,
Oferece apoio financeiro para continuidade dos estudos assim como o Pé-de-Meia,
Valoriza a formação educacional na adolescência e
Promove inclusão social e oportunidades futuras.
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BV")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bv (Benefício Variável Criança)
Indica a Quantidade de benefícios pagos a crianças de 7 a 12 anos.
Complementa a renda e apoia a educação infantil,
Ajuda nas despesas básicas da fase escolar,
Contribui para reduzir desigualdades sociais e
Garante apoio contínuo às famílias com filhos pequenos
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BF")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bf (Benefícios Familiares)
Informa o Total de benefícios recebidos pela família.
Mostra o impacto direto do programa no núcleo familiar,
Facilita análise da abrangência do Bolsa Família e
É indicador central da proteção social oferecida.
")
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("BENEFICIO BVBVA")),          # título em negrito
                   p("Observado na aba Dados Completos - qtd_ben_bvbva (BV + BVA somados)
Indica a Quantidade de benefícios de crianças e adolescentes juntos.
Consolida dados das faixas etárias escolares,
Facilita análises integradas sobre educação básica,
Mostra o alcance do programa entre jovens e crianças e
Permite avaliar políticas voltadas à permanência escolar.
")
               )
             ),
             "LUZ PARA TODOS" = tagList(
               h3("Programa Luz Para Todos"),
               p("Leva energia elétrica a comunidades rurais e regiões isoladas."),
               div(class = 'white-box texto-centralizado',
                   p(strong("ANO DE HOMOLOGAÇÃO")),   # título em negrito
                   p("É o ano em que o atendimento do programa foi oficialmente validado e registrado,
Marca o momento em que a execução do serviço foi reconhecida pelo governo,
Serve como referência para controle, auditoria e prestação de contas e
Garante que o atendimento foi concluído conforme os critérios do programa.
"),
               ),
               br(),
               div(class = 'white-box texto-centralizado',
                   p(strong("TIPO DE PROGRAMA")),          # título em negrito
                   p("Refere-se à categoria do serviço prestado: regiões remotas, áreas rurais ou via distribuidora,
Define o contexto geográfico e operacional da instalação elétrica,
Ajuda a identificar os desafios logísticos e técnicos do atendimento e
Permite segmentar os dados por tipo de localidade atendida.
.")
               )
             )
      )
    })
  })
}
