library(DT)
library(rpart)
library(heatmaply)
library(xlsx)
library(flextable)
library(kableExtra)
library(rattle)
library(tidyverse)
library(pool)
library(shiny)
library(stringi)
library(datamods)
library(rpart.plot)
library(gridlayout)
library(forcats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(DBI)
library(esquisse)
library(RPostgreSQL)
library(plotly)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(shinybusy)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(party)
library(stringr)

corpus <- data.frame()%>%mutate(cualquiera = NA,sexo=NA,ciudad=NA,nivel=NA,edad=NA,spk=NA,sexociudad=NA,edadciudad=NA,nivelciudad=NA,sexoniveledadciudad=NA,pitch_mean_spk=NA,range_mean_spk=NA,inflexion_mean_spk=NA,dur_mean_spk=NA,velocidad_spk=NA,intensity_mean_spk=NA)

invento <- data.frame()%>%mutate(pulsaEjecute = NA)
# mapscities <- readRDS("mapscities.rds")

ui <- navbarPage(
  title = "Ameresco",
  id = "tabset-default-id",
  selected = "Inicio",
  footer = tags$p(
    align = "center",
    "Cabedo,
A. y Carcelén,
A. 2022,
Oralstats Aroca. V.1.2"
  ),
  collapsible = TRUE,
  theme = bs_theme(
    version = 5,
    bootswatch = "journal",
  ),
  useShinydashboard(),
  tabPanel(
    title = "Inicio",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "areacredits area2 arealicencia",
        "credits2    area3 area4       "
      ),
      row_sizes = c(
        "0.73fr",
        "1.27fr"
      ),
      col_sizes = c(
        "0.58fr",
        "1.91fr",
        "0.58fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area2",
        has_border = FALSE,
        item_gap = "10px",
        tags$div(tags$img(
          src = "aroca2.gif",
          width = "100%",
          height = "400px"
        ))
      ),
      grid_card(
        area = "area3",
        actionLink(
          inputId = "gocorpus",
          "Pulse para acceder al buscador"
        ),
        includeHTML(path = "www/map.html")
      ),
      grid_card(
        area = "areacredits",
        title = "Director del proyecto Ameresco",
        tags$img(
          src = "briz.png",
          width = "150px"
        ),
        tags$strong("Antonio Briz")
      ),
      grid_card(
        area = "credits2",
        title = "Coordinadoras académicas del proyecto Ameresco",
        tags$img(
          src = "albelda.png",
          width = "150px"
        ),
        tags$strong("Marta Albelda"),
        tags$img(
          src = "estelles.png",
          width = "150px"
        ),
        tags$strong("Maria Estellés")
      ),
      grid_card(
        area = "area4",
        title = "Creadores Oralstats Aroca",
        tags$strong("Adrián Cabedo"),
        tags$img(
          src = "cabedo.png",
          width = "150px"
        ),
        tags$strong("Andrea Carcelén"),
        tags$img(
          src = "carcelen.png",
          width = "150px"
        )
      ),
      grid_card(
        area = "arealicencia",
        title = "Licencia y copyright",
        scrollable = TRUE,
        actionBttn(
          inputId = "generalinfo",
          "Más info",
          icon = icon("info"),
          color = "success",
          style = "material-flat",
          size = "xs"
        ),
        tags$div(
          style = "font-size:9px;",
          tags$img(
            src = "copyright.png",
            width = "60",
            height = "20"
          ),
          tags$p(
            "Oralstats Aroca by Cabedo Nebot,
Adrián & Carcelén Guerrero,
Andrea is licensed under a",
            tags$a(
              href = "http://creativecommons.org/licenses/by/4.0/",
              "Creative Commons Reconocimiento 4.0 Internacional License."
            ),
            "Disponible en:",
            tags$a(
              href = "https://github.com/acabedo/oralstats/",
              "https://github.com/acabedo/oralstats/"
            )
          ),
          tags$p("Subvención actual: ESPRINT,
Estrategias pragmático-retóricas en la interacción conversacional conflictiva entre íntimos y conocidos: intensificación,
atenuación y gestión interaccional.Ministerio de Ciencia e Innovación (PID2020-114805GB-100)")
        )
      )
    )
  ),
  tabPanel(
    title = "Equipo",
    grid_container(
      layout = "area0",
      row_sizes = "2fr",
      col_sizes = "6fr",
      gap_size = "10px",
      grid_card(
        area = "area0",
        scrollable = TRUE,
        grid_container(
          layout = "area0",
          row_sizes = "2fr",
          col_sizes = "2fr",
          gap_size = "10px",
          grid_card(
            area = "area0",
            scrollable = TRUE,
            has_border = TRUE,
            tags$div(
              width = "100%",
              align = "center",
              includeHTML("www/equipo.html")
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Búsqueda",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "area0 area2 area2",
        "area0 area2 area2"
      ),
      row_sizes = c(
        "1.79fr",
        "3.45fr"
      ),
      col_sizes = c(
        "1fr",
        "2.51fr",
        "1.94fr"
      ),
      gap_size = "11px",
      grid_card(
        area = "area0",
        title = "Buscar",
        scrollable = TRUE,
        has_border = TRUE,
        useShinyjs(),
        tags$div(
          id = "bloque",
          actionBttn(
            inputId = "tutorialinfo",
            "Tutorial",
            icon = icon("info"),
            color = "success",
            style = "material-flat",
            size = "xs"
          ),          textInput(
            inputId = "filter1",
            label = "",
            value = "",
            width = "100%",
            placeholder = "Escriba una palabra"
          ),
          tags$br(),
          splitLayout(tags$div(
            id = "busquedas",
            aling = "center",
            actionButton(
              "buscarbttn",
              "Buscar"
            ),
            actionButton(
              "resetbttn",
              "Limpiar"
            )
          )),
          checkboxInput(
            inputId = "avanzadas",
            label = "Opciones",
            value = FALSE,
            width = "100%"
          ),
          conditionalPanel(
            condition = "input.avanzadas == true",
            tags$div(
              id = "opciones",
              checkboxInput(
                inputId = "regexsel",
                label = "Regex",
                value = FALSE,
                width = "100%"
              ),
              selectInput(
                inputId = "ciudad",
                label = "Ciudad",
                width = "100%",
                choices = c(
                  "cualquiera",
                  "Barranquilla",
                  "Medellín",
                  "Ciudad de México",
                  "La Habana",
                  "Iquique",
                  "Querétaro",
                  "Santa Cruz",
                  "Ciudad de Panamá",
                  "Santiago de Chile",
                  "Las Palmas",
                  "Tucumán",
                  "Temuco",
                  "Tegucigalpa",
                  "Buenos Aires",
                  "Monterrey"
                ),
                selected = "cualquiera",
                multiple = FALSE
              ),
              selectInput(
                inputId = "sexo",
                label = "Sexo",
                width = "100%",
                choices = c(
                  "cualquiera",
                  "Hombre",
                  "Mujer"
                ),
                selected = "cualquiera",
                multiple = FALSE
              ),
              selectInput(
                inputId = "edad",
                label = "Edad",
                width = "100%",
                choices = c(
                  cualquiera = "cualquiera",
                  `18-25` = "18-25",
                  `26-55` = "26-55",
                  `Más de 56` = "Más de 56"
                )
              ),
              selectInput(
                inputId = "nivel",
                label = "Nivel",
                width = "100%",
                choices = list(
                  cualquiera = "cualquiera",
                  Alto = "Alto",
                  Medio = "Medio",
                  Bajo = "Bajo"
                )
              ),
              selectInput(
                inputId = "filter4",
                label = "Cita",
                width = "100%",
                choices = c(
                  "cualquiera",
                  "yes",
                  "no"
                ),
                selected = "cualquiera",
                multiple = FALSE
              ),
              selectInput(
                inputId = "filter5",
                label = "Interrogación",
                width = "100%",
                choices = c(
                  "cualquiera",
                  "yes",
                  "no"
                ),
                selected = "cualquiera",
                multiple = FALSE
              ),
              selectInput(
                inputId = "filter3",
                label = "Solapamiento",
                width = "100%",
                choices = list(
                  "cualquiera",
                  "yes",
                  "no"
                ),
                selected = "cualquiera",
                multiple = FALSE
              ),
              selectInput(
                inputId = "filter2",
                label = "Alargamiento",
                width = "100%",
                choices = list(
                  "cualquiera",
                  "yes",
                  "no"
                ),
                selected = "cualquiera",
                multiple = FALSE
              ),
              sliderInput(
                inputId = "filter6",
                label = "Distancia",
                min = -2L,
                max = 2L,
                value = 0L,
                step = 1L,
                width = "100%"
              ),
              textInput(
                inputId = "filter7",
                width = "100%",
                label = "Distancia palabra",
                value = ""
              ),
              sliderInput(
                inputId = "filter8",
                label = "Distancia pos",
                min = -2L,
                max = 2L,
                value = 0L,
                step = 1L,
                width = "100%"
              ),
              textInput(
                inputId = "filter9",
                label = "Distancia upos",
                value = "",
                width = "100%"
              ),
              selectInput("posicion_grupo",
                "Posición grupo",
                choices = c(
                  "cualquiera",
                  "primera",
                  "intermedia",
                  "última",
                  "única"
                ),
                width = "100%"
              ),
              selectInput("posicion_intervencion",
                "Posición intervención",
                choices = c(
                  "cualquiera",
                  "primera",
                  "intermedia",
                  "última",
                  "única"
                ),
                width = "100%"
              )
            )
          )
        )
      ),
      grid_card(
        area = "area2",
        width = "100%",
        align = "center",
        scrollable = TRUE,
        has_border = TRUE,
        checkboxInput(
          inputId = "relative",
          label = "Frecuencia relativa por millón",
          value = FALSE,
          width = "100%"
        ),
        shinydashboard::valueBoxOutput("totales"),
        plotlyOutput(
          outputId = "ciudadplot",
          width = "100%",
          height = "400px"
        ),
        tags$p(),
        actionLink(
          "linkconcord",
          "Ver concordancias"
        ),
        varSelectInput(
          inputId = "selectfactor",
          label = "Variable de agrupación",
          data = corpus %>%
            select(
              ciudad,
              sexo,
              edad,
              nivel,
              spk
            ),
          selected = "ciudad",
          multiple = TRUE
        )
      )
    )
  ),
  tabPanel(
    title = "Concordancias",
    grid_container(
      layout = c(
        "area0",
        "area1"
      ),
      row_sizes = c(
        "2fr",
        "2fr"
      ),
      col_sizes = "1.73fr",
      gap_size = "10px",
      grid_card(
        area = "area0",
        scrollable = TRUE,
        downloadButton(
          "downloadconcord",
          "Descargue concordancias"
        ),
        DTOutput(
          outputId = "table1",
          width = "100%"
        )
      ),
      grid_card(
        area = "area1",
        scrollable = TRUE,
        tabsetPanel(
          tabPanel(
            title = "Contexto",
            uiOutput(outputId = "table2")
          ),
          tabPanel(
            title = "Conversación completa",
            uiOutput(outputId = "audioconv"),
            DTOutput(
              outputId = "conversacion",
              width = "100%",
              height = "200px"
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Ngrams",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "area0 area1",
        "area0 area1"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.53fr",
        "1.47fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        varSelectInput("filterngrams1",
          "Seleccione variable",
          data = corpus %>% select(
            spk,
            ciudad,
            sexo,
            edad,
            nivel
          ),
          multiple = TRUE,
          selected = "ciudad"
        ),
        selectInput(
          inputId = "filterngrams2",
          label = "Seleccione tipo de ngrama",
          choices = list(
            "ngrama1",
            "ngrama2",
            "ngrama3",
            "ngrama4"
          ),
          selected = "ngrama1"
        ),
        actionButton(
          inputId = "ngrambttn",
          label = "Obtener ngramas",
          width = "100%"
        )
      ),
      grid_card(
        area = "area1",
        DTOutput(
          outputId = "ngrams",
          width = "100%"
        )
      )
    )
  ),
  tabPanel(
    title = "Prosodia",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "area0 area3",
        "area0 area3"
      ),
      row_sizes = c(
        "1.52fr",
        "0.48fr"
      ),
      col_sizes = c(
        "0.5fr",
        "2.5fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        actionBttn(
          inputId = "prosodiainfo",
          "Más info",
          icon = icon("info"),
          color = "success",
          style = "material-flat",
          size = "xs"
        ),
        varSelectInput("filterprosodia1",
          "Seleccione variable numérica",
          data = corpus %>%
            select(
              pitch_mean_spk,
              range_mean_spk,
              inflexion_mean_spk,
              dur_mean_spk,
              velocidad_spk,
              intensity_mean_spk
            ),
          selected = "range_mean_spk"
        ),
        varSelectInput("filterprosodia2",
          "Seleccione variable",
          data = corpus %>% select(
            ciudad,
            sexo,
            edad,
            nivel
          ),
          selected = "sexo"
        ),
        varSelectInput("filterprosodia4",
          "Seleccione variable",
          data = corpus %>% select(
            ciudad,
            sexo,
            edad,
            nivel
          ),
          selected = "ciudad"
        ),
        selectInput(
          inputId = "filterprosodia3",
          label = "Seleccione visualización",
          choices = list(
            "diagrama de caja",
            "grafico lolipop"
          ),
          selected = "diagrama de caja"
        ),
        actionButton(
          inputId = "prosodiabttn",
          label = "Obtener datos prosódicos",
          width = "100%"
        )
      ),
      
      grid_card(
        area = "area3",
        scrollable = TRUE,
        
        plotlyOutput(
          outputId = "prosodiaplot",
          width = "100%",
          height = "2000px"
        ),
        tabsetPanel(
          tabPanel(title = "Por variable",
                   verbatimTextOutput("prosodiastats1")),
          tabPanel(title = "Por combinación",
                   verbatimTextOutput("prosodiastats2"))
        )
      )
    )
  ),
  tabPanel(
    title = "Descargar",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "area0 area3",
        "area0 area3"
      ),
      row_sizes = c(
        "1.52fr",
        "0.48fr"
      ),
      col_sizes = c(
        "0.5fr",
        "2.5fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        title = "Descargar transcripción",
        actionBttn(
          inputId = "descargarinfo",
          "Más info",
          icon = icon("info"),
          color = "success",
          style = "material-flat",
          size = "xs"
        ),
        selectInput('conversaciondescargar', 'Seleccione conversación para descargar:',
                    choices = NULL),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        downloadButton('downloadReport',"Descargar")
      ),
      
      grid_card(
        area = "area3",
        scrollable = TRUE,
        
        DTOutput('conve')
      )
    )
  ),
  tabPanel(
    title = "Estadísticas corpus",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "area4 area1 area1 area1",
        "area4 area1 area1 area1",
        "area4 area1 area1 area1"
      ),
      row_sizes = c(
        "0.87fr",
        "0.8fr",
        "1.33fr"
      ),
      col_sizes = c(
        "0.7fr",
        "1.34fr",
        "1fr",
        "0.96fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area1",
        DTOutput(
          outputId = "completestats",
          width = "100%"
        ),
        plotlyOutput(
          outputId = "plotcorpus",
          width = "100%",
          height = "400px"
        )
      ),
      grid_card(
        area = "area4",
        title = "Panel de selección",
        scrollable = TRUE,
        tags$div(
          selectInput(
            inputId = "selstats",
            width = "100%",
            label = "Seleccione palabras o hablantes",
            choices = list(
              "palabras",
              "hablantes"
            )
          ),
          varSelectInput(
            inputId = "groupcorpus",
            label = "Agrupar corpus por",
            multiple = TRUE,
            data = corpus %>%
              select(
                -cualquiera,
                -sexociudad,
                -nivelciudad,
                -edadciudad,
                -sexoniveledadciudad
              ),
            width = "100%",
            selected = "ciudad"
          ),
          varSelectInput(
            inputId = "filtrovariable",
            label = "Primer filtro (opcional)",
            data = corpus %>%
              select(
                -cualquiera,
                -sexociudad,
                -nivelciudad,
                -edadciudad,
                -sexoniveledadciudad
              ),
            width = "100%"
          ),
          selectInput(
            inputId = "filtroselect",
            label = "Seleccione categoría Filtro 1",
            choices = "",
            multiple = TRUE,
            width = "100%"
          ),
          varSelectInput(
            inputId = "filtrovariable2",
            label = "Segundo filtro  (opcional)",
            data = corpus %>% select(
              -cualquiera,
              -sexociudad,
              -nivelciudad,
              -edadciudad,
              -sexoniveledadciudad
            ),
            width = "100%"
          ),
          selectInput(
            inputId = "filtroselect2",
            label = "Seleccione categoría Filtro 2",
            choices = "",
            multiple = TRUE,
            width = "100%"
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Furious",
    add_busy_spinner(
      spin = "circle",
      margins = c(
        10,
        20
      ),
      position = "bottom-right",
      color = "green",
      width = "100px",
      height = "100px"
    ),
    grid_container(
      layout = c(
        "area3 area0 area0",
        "area3 area0 area0",
        "area  area2 area2"
      ),
      row_sizes = c(
        "1fr",
        "0.77fr",
        "2.27fr"
      ),
      col_sizes = c(
        "0.44fr",
        "1.89fr",
        "0.67fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        scrollable = TRUE,
        actionBttn(
          inputId = "furiousinfo",
          "Más info",
          icon = icon("info"),
          color = "success",
          style = "material-flat",
          size = "xs"
        ),
        plotlyOutput(
          outputId = "furiousplot",
          width = "100%",
          height = "400px"
        ),
        splitLayout(
          verbatimTextOutput("mediasspk1"),
          verbatimTextOutput("mediasspk2")
        )
      ),
      grid_card(
        area = "area2",
        scrollable = TRUE,
        downloadButton(
          "downloadfurious",
          "Descargue furioso"
        ),
        uiOutput(outputId = "audio2"),
        DTOutput(
          outputId = "furioustbl",
          width = "100%",
          height = "400px"
        )
      ),
      grid_card(
        area = "area",
        title = "Parámetros para furioso",
        scrollable = TRUE,
        tags$div(filter_data_ui("filtering"))
      ),
      grid_card(
        area = "area3",
        title = "Parámetros generales",
        scrollable = TRUE,
        tags$div(
          varSelectizeInput(
            inputId = "selecty",
            label = "Variable para ordenadas",
            multiple = FALSE,
            data = "",
            width = "100%"
          ),
          selectInput(
            inputId = "conversselect",
            label = "Seleccione conversación",
            choices = "",
            multiple = FALSE,
            width = "100%"
          ),
          actionButton(
            inputId = "furiousbttn",
            "Generar furioso"
          )
        ),
        tags$br()
      )
    )
  )
)



server <- function(input, output,session) {
  

  
filtrado <- eventReactive(input$buscarbttn,{
req(input$filter1)

    conexion1 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
    
    # busqueda <- paste0("select a.token,a.upos,a.lemma,a.alargamiento,b.content from palabras as a left join grupos as b on a.id = b.id where a.token ='",input$filter1,"'","AND a.alargamiento ='",input$filter2,"'")
    if(input$regexsel ==FALSE){
      if(!grepl("[ \\?]",input$filter1)){
      texto <- dbGetQuery(conexion1,paste0("WITH cte AS (SELECT lag(token,2) over(partition by palabras.id order by posicion_grupo ASC) as prev2,lag(token,1) over(partition by palabras.id order by posicion_grupo ASC) as prev1,lag(upos,2) over(partition by palabras.id order by posicion_grupo ASC) as prevpos2,lag(upos,1) over(partition by palabras.id order by posicion_grupo ASC) as prevpos1, token,lead(token,1) over(partition by palabras.id order by posicion_grupo ASC) as post1,lead(token,2) over(partition by palabras.id order by posicion_grupo ASC) as post2,upos,lead(upos,1) over(partition by palabras.id order by posicion_grupo ASC) as postpos1,lead(upos,2) over(partition by palabras.id order by posicion_grupo ASC) as postpos2,posicion_grupo_tag,posicion_intervencion,ulemma,alargamiento,solap, cita,interr, grupos.content as content, grupos.source,hablantes.sexo,hablantes.nivel,hablantes.edad,conversaciones.ciudad, grupos.time_start,grupo_id,grupos.spk,grupos.intervenciones_id FROM palabras left join grupos on palabras.id=grupo_id LEFT JOIN hablantes on hablantes.spk = grupos.spk LEFT JOIN conversaciones on hablantes.source = conversaciones.source) SELECT * from cte where token = '", input$filter1,"' OR upos ='",input$filter1,"' OR ulemma ='",input$filter1,"'"))}else {
        
        texto <- dbGetQuery(conexion1,paste0("WITH cte AS (SELECT lag(token,2) over(partition by palabras.id order by posicion_grupo ASC) as prev2,lag(token,1) over(partition by palabras.id order by posicion_grupo ASC) as prev1,lag(upos,2) over(partition by palabras.id order by posicion_grupo ASC) as prevpos2,lag(upos,1) over(partition by palabras.id order by posicion_grupo ASC) as prevpos1, token,lead(token,1) over(partition by palabras.id order by posicion_grupo ASC) as post1,lead(token,2) over(partition by palabras.id order by posicion_grupo ASC) as post2,upos,lead(upos,1) over(partition by palabras.id order by posicion_grupo ASC) as postpos1,lead(upos,2) over(partition by palabras.id order by posicion_grupo ASC) as postpos2,posicion_grupo_tag,posicion_intervencion,ulemma,alargamiento,solap, cita,interr, grupos.content as content, grupos.source,hablantes.sexo,hablantes.nivel,hablantes.edad,conversaciones.ciudad, grupos.time_start,grupo_id,grupos.spk,grupos.intervenciones_id FROM palabras left join grupos on palabras.id=grupo_id LEFT JOIN hablantes on hablantes.spk = grupos.spk LEFT JOIN conversaciones on hablantes.source = conversaciones.source) SELECT * from cte where content like '% ", input$filter1," %' OR content like '", input$filter1," %'","OR content like '% ", input$filter1,"'"))
        
      }}
    else if(input$regexsel ==TRUE & input$filter1!="")  {texto <- dbGetQuery(conexion1,paste0("WITH cte AS (SELECT lag(token,2) over(partition by palabras.id order by posicion_grupo ASC) as prev2,lag(token,1) over(partition by palabras.id order by posicion_grupo ASC) as prev1,lag(upos,2) over(partition by palabras.id order by posicion_grupo ASC) as prevpos2,lag(upos,1) over(partition by palabras.id order by posicion_grupo ASC) as prevpos1, token,lead(token,1) over(partition by palabras.id order by posicion_grupo ASC) as post1,lead(token,2) over(partition by palabras.id order by posicion_grupo ASC) as post2,upos,lead(upos,1) over(partition by palabras.id order by posicion_grupo ASC) as postpos1,lead(upos,2) over(partition by palabras.id order by posicion_grupo ASC) as postpos2,ulemma,grupos.time_start,posicion_grupo_tag,posicion_intervencion,alargamiento,solap, cita,interr, content, grupos.source,hablantes.sexo,hablantes.nivel,hablantes.edad,conversaciones.ciudad, grupo_id,grupos.spk, grupos.intervenciones_id FROM palabras left join grupos on palabras.id=grupo_id LEFT JOIN hablantes on hablantes.spk = grupos.spk LEFT JOIN conversaciones on hablantes.source = conversaciones.source)
SELECT * from cte where token like '", input$filter1,"'"))} else {NULL}
    
    dbDisconnect(conexion1)
    
    if(!grepl("[ \\?]",input$filter1)){
    texto%>%mutate(distancia=paste0("prev",input$filter6))%>%filter(if(input$filter2!="cualquiera") {alargamiento%in%!!input$filter2} else {TRUE},
                                                                                                                                                                                                            if(input$filter3!="cualquiera") {solap%in%!!input$filter3} else {TRUE},if(input$filter4!="cualquiera") {cita%in%!!input$filter4} else {TRUE},if(input$filter5!="cualquiera") {interr%in%!!input$filter5} else {TRUE},if(input$filter6<0){get(paste0("prev",abs(input$filter6)))==input$filter7}else if (input$filter6>0){get(paste0("post",input$filter6))==input$filter7}else{TRUE},
                                                                                                                                                                                                            if(input$filter8<0){get(paste0("prevpos",abs(input$filter8)))==input$filter9}else if (input$filter8>0){get(paste0("postpos",input$filter8))==input$filter9}else{TRUE},
                                                                                                                                                                                                            if(input$sexo!="cualquiera") {sexo%in%!!input$sexo} else {TRUE},
                                                                                                                                                                                                            if(input$edad!="cualquiera") {edad%in%!!input$edad} else {TRUE},
                                                                                                                                                                                                            if(input$nivel!="cualquiera") {nivel%in%!!input$nivel} else {TRUE},
                                                                    if(input$ciudad!="cualquiera") {ciudad%in%!!input$ciudad} else {TRUE},
                                                                    if(input$posicion_grupo!="cualquiera") {posicion_grupo_tag%in%!!input$posicion_grupo} else {TRUE},
                                                                    if(input$posicion_intervencion!="cualquiera") {posicion_intervencion%in%!!input$posicion_intervencion} else {TRUE})}
    
    else{
      
      texto%>%filter(token == word(input$filter1,1)|(token == word(gsub("[\\¿\\?]","",input$filter1),1)&interr=="yes"))%>%mutate(distancia=paste0("prev",input$filter6))%>%filter(if(input$filter2!="cualquiera") {alargamiento%in%!!input$filter2} else {TRUE},
                                                                                                                                                                                  if(input$filter3!="cualquiera") {solap%in%!!input$filter3} else {TRUE},if(input$filter4!="cualquiera") {cita%in%!!input$filter4} else {TRUE},if(input$filter5!="cualquiera") {interr%in%!!input$filter5} else {TRUE},if(input$filter6<0){get(paste0("prev",abs(input$filter6)))==input$filter7}else if (input$filter6>0){get(paste0("post",input$filter6))==input$filter7}else{TRUE},
                                                                                                                                                                                  if(input$filter8<0){get(paste0("prevpos",abs(input$filter8)))==input$filter9}else if (input$filter8>0){get(paste0("postpos",input$filter8))==input$filter9}else{TRUE},
                                                                                                                                                                                  if(input$sexo!="cualquiera") {sexo%in%!!input$sexo} else {TRUE},
                                                                                                                                                                                  if(input$edad!="cualquiera") {edad%in%!!input$edad} else {TRUE},
                                                                                                                                                                                  if(input$nivel!="cualquiera") {nivel%in%!!input$nivel} else {TRUE},
                                                                                                                                                                                  if(input$ciudad!="cualquiera") {ciudad%in%!!input$ciudad} else {TRUE},
                                                                                                                                                                                  if(input$posicion_grupo!="cualquiera") {posicion_grupo_tag%in%!!input$posicion_grupo} else {TRUE},
                                                                                                                                                                                  if(input$posicion_intervencion!="cualquiera") {posicion_intervencion%in%!!input$posicion_intervencion} else {TRUE})
      
    }

  })

corpuscompleto <- reactive({
 
  req(input$relative==TRUE)
  conexion1 <- dbPool(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
  
  # busqueda <- paste0("select a.token,a.upos,a.lemma,a.alargamiento,b.content from palabras as a left join grupos as b on a.id = b.id where a.token ='",input$filter1,"'","AND a.alargamiento ='",input$filter2,"'")

# corpuscompleto <- dbGetQuery(conexion1,paste0("select ",selection,", count(*) FROM palabras left join grupos on palabras.id=grupo_id LEFT JOIN hablantes on hablantes.spk = grupos.spk LEFT JOIN conversaciones on hablantes.source = conversaciones.source group by",selection))
# corpuscompleto <- corpuscompleto%>%mutate(categoria=paste(!!!input$selectfactor,sep="_"))
#  
#   dbDisconnect(conexion1)
  
  palabras <- conexion1 %>% tbl("palabras")
  hablantes <- conexion1 %>% tbl("hablantes")
  conversaciones <- conexion1 %>% tbl("conversaciones")
  corpus <- palabras%>%select(-source) %>%left_join(hablantes, by="spk")%>%left_join(conversaciones, by="source")%>%mutate(categoria=paste(!!!input$selectfactor,sep="_"))%>%group_by(categoria)%>%summarise(frecuencia_compl=n())%>%show_query()%>% collect()
  poolClose(conexion1)
  p <- filtrado()
  p<-  p%>%mutate(categoria= paste(!!!input$selectfactor,sep="_"))%>%group_by(categoria)%>%summarise(frecuencia=n())%>%left_join(corpus,by="categoria")%>%group_by(categoria)%>%mutate(frecpormillon = frecuencia/frecuencia_compl*1000000)%>%summarise(frecuencia= last(frecuencia),frecpormillon = last(frecpormillon))

  # filtrado2 <- filtrado()%>%mutate(categoria=paste(!!!input$selectfactor,sep="_"))%>%left_join(corpuscompleto, by("categoria"))%>%group_by(categoria)%>%summarise(frecuencia=n())
  
})

  # output$tablecorpus <- renderDT(corpuscompleto())
  output$table1 <- DT::renderDataTable({
    req(input$filter1!="")

    datatable(filtrado()%>%rename(inicio = time_start)%>%select(inicio,ciudad,spk,token,content,intervenciones_id)%>%drop_na(),escape=6,selection = "single", options=list(columnDefs = list(list(visible=FALSE, targets=c(6))),pageLength = 5,lengthMenu = c(5, 10, 15, 20)))})
  
contexto <- reactive({
  req(input$filter1!="")
  
    busc <- filtrado()[input$table1_rows_selected,"intervenciones_id"]
    
    conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
    
    # busqueda <- paste0("select a.token,a.upos,a.lemma,a.alargamiento,b.content from palabras as a left join grupos as b on a.id = b.id where a.token ='",input$filter1,"'","AND a.alargamiento ='",input$filter2,"'")
    texto <- dbGetQuery(conexion2,paste0("WITH cte AS (SELECT lag(time_start_int,2) over(partition by source order by source,time_start_int ASC) as timestart,lag(spk,2) over(partition by source order by source,time_start_int ASC) as spkprev2,lag(intervencion,2) over(partition by source order by source,time_start_int ASC) as prev2,lag(spk,1) over(partition by source order by source,time_start_int ASC) as spkprev1,lag(intervencion,1) over(partition by source order by source,time_start_int ASC) as prev1, spk, intervencion,source,lead(spk,1) over(partition by source order by source,time_start_int ASC) as spkpost1,lead(intervencion,1) over(partition by source order by source,time_start_int ASC) as post1,lead(spk,2) over(partition by source order by source,time_start_int ASC) as spkpost2,lead(intervencion,2) over(partition by source order by source,time_start_int ASC) as post2, lead(time_end_int,2) over(partition by source order by source,time_end_int ASC) as timeend, intervenciones_id from intervenciones order by source,time_start_int ) select * from cte where cte.intervenciones_id = '", busc,"'"))
    texto2 <- tbl(conexion2,"conversaciones")%>%select(source,citacion)%>%collect()
    texto <- texto%>% left_join(texto2, by="source")
    dbDisconnect(conexion2)
    texto
    
    })
    

conversacionselect <- reactive({
  
    req(input$table1_rows_selected)
    busc <- filtrado()[input$table1_rows_selected,"source"]

    conexion4 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
    texto <- dbGetQuery(conexion4,paste0("select * from intervenciones where source = '", busc,"'"))
    dbDisconnect(conexion4)
    texto<-texto%>%rename(inicio=time_start_int,final = time_end_int,contenido=intervencion_export)
    texto <- texto%>%arrange(inicio)
    
  })

# gruposselect <- reactive({
#   
#   req(input$furioustbl_rows_selected)
#   busc <- grupos()[input$furioustbl_rows_selected,"source"]
#   
#   conexionselgrupos <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
#   texto <- dbGetQuery(conexionselgrupos,paste0("select source,grupo_id,time_start,time_end from grupos where source = '", busc,"'"))
#   dbDisconnect(conexionselgrupos)
#   texto<-texto%>%filter(source%in%input$conversselect)%>%rename(inicio=time_start,final = time_end)
#   texto
#   
# })

output$conversacion <- renderDT(datatable(conversacionselect()%>%select(source,inicio,final,contenido),selection = "single",rownames= FALSE,options = list(scrollY = TRUE,autoWidth = TRUE,columnDefs = list(list(width = '60px', targets = c(0,1,2)))), filter = list(
  position = 'top', clear = FALSE
)))
 
   output$table2 <- renderUI({
    req(input$table1_rows_selected)
    req(input$filter1)
    link = paste0("http://esvaratenuacion.es/sites/default/files/conversaciones/",contexto()$source,".mp3#t=",contexto()$timestart/1000,",",(contexto()$timeend+250)/1000)
    tags$div(
      tags$h5(tags$strong("Contexto")),
      tags$br(),
      tags$audio(src= link,type = "audio/mp3",autoplay = NA, controls = NA),
      tags$br(),
      tags$strong(style="margin:0;font-size:80%;",paste("Tiempo de inicio:", contexto()$timestart), "ms","|| Tiempo final:",contexto()$timeend, "ms"),
      tags$br(),
      tags$p(style="margin:0;font-size:80%;",paste(contexto()$spkprev2,": ",contexto()$prev2)),
      tags$p(style="margin:0;font-size:80%;",paste(contexto()$spkprev1,": ",contexto()$prev1)),
      tags$p(style="margin:0;font-size:80%;",paste(contexto()$spk,": ",contexto()$intervencion)),
      tags$p(style="margin:0;font-size:80%;",paste(contexto()$spkpost1,": ",contexto()$post1)),
      tags$p(style="margin:0;font-size:80%;",paste(contexto()$spkpost2,": ",contexto()$post2)),
      tags$br(),
      tags$p(style="font-size:80%;",paste("CITA: (Conversación ",contexto()$source,", ",contexto()$citacion, ". Consultado el", Sys.Date() ,")",sep=" "))
    )
    
    
    
  })
  output$audioconv <- renderUI({
    req(input$conversacion_rows_selected)
    seleccion <- conversacionselect()[input$conversacion_rows_selected,c("source","inicio","final")]
    link = paste0("http://esvaratenuacion.es/sites/default/files/conversaciones/",seleccion$source,".mp3#t=",seleccion$inicio/1000,",",(seleccion$final+250)/1000)
    tags$div(
      tags$h5(tags$strong("Conversación completa")),
      tags$br(),
      tags$audio(src= link,type = "audio/mp3",autoplay = NA, controls = NA))

    })
 
  output$audio2 <- renderUI({
    req(input$furioustbl_rows_selected)
    grupos <- grupos2()
    seleccion <- grupos[input$furioustbl_rows_selected,c("source","time_start","time_end")]
    link <- paste0("http://esvaratenuacion.es/sites/default/files/conversaciones/",seleccion$source,".mp3#t=",seleccion$time_start/1000,",",((seleccion$time_end+250)/1000))
    tags$div(
      tags$h6("Audio"),
      tags$audio(src= link,type = "audio/mp3",autoplay = NA, controls = NA))
    
  })
  
 #  output$audio2text <- renderPrint({
 #    
 # req(input$furioustbl_rows_selected)
 #    grupos <- grupos2()
 #    seleccion <- grupos[input$furioustbl_rows_selected,c("source","time_start","time_end","grupo_id")]
 #    link <- paste0("http://esvaratenuacion.es/sites/default/files/conversaciones/",seleccion$source,".mp3#t=",seleccion$time_start/1000,",",seleccion$time_end/1000)
 #    link
 #    })
  
   # grupose = data.frame()


  
  



  
data_r <- reactiveValues(data = invento, name = "invento")
  
observeEvent(input$esquissebuttn,
               {conexion3 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
               corpus3 <- dbGetQuery(conexion3,"select * from hablantes left join conversaciones on hablantes.source = conversaciones.source")
               dbDisconnect(conexion3)
               corpus3<-corpus3[, !duplicated(colnames(corpus3))]
               corpus3 <- corpus3%>%select(-row.names,-spk)
                                data_r$data <- corpus3
                                data_r$name <- "corpus3"})
  # 
  # observe({
  #   # conexion3 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
  #   # corpus <- dbGetQuery(conexion3,"select * from grupos")
  #   # dbDisconnect(conexion3)
  #   #
  #   # data_r <- setDF(corpus)
  # 
  #   data_r$data <- iris
  #   data_r$name <- "iris"
  # 
  # })
  

  

  results <- esquisse_server(
    id = "esquisse",
    data_rv = data_r
    
  )
  
  output$ciudadplot <- renderPlotly({
    
   
    if(input$relative==FALSE){
    p<-  filtrado()%>%
      mutate(categoria= paste(!!!input$selectfactor,sep="_"))%>%
  group_by(!!!input$selectfactor)%>%summarise(categoria=last(categoria),frecuencia=n())%>%drop_na()
    # d<- ggplot(p, aes(x = "", y = frecuencia, fill = ciudad)) +
    #   geom_col() +    geom_label(aes(label = frecuencia),
    #                              position = position_stack(vjust = 0.5),
    #                              show.legend = FALSE) +    coord_polar(theta = "y")
     d<- ggplot(p, aes(x=categoria,y=frecuencia, fill=categoria))+ theme_bw()+   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + geom_bar(stat="identity")  
    d } else{
      
      p<- corpuscompleto()
      d<- ggplot(p, aes(x=categoria,y=frecpormillon, fill=categoria))+ theme_bw()+   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + geom_bar(stat="identity")  
      d
      
    }
    
  })
  

  # output$sexoplot <- renderPlotly({
  #   req(input$buscarbttn)
  #   p<-  filtrado()%>%group_by(sexo)%>%summarise(frecuencia=n())%>%drop_na()
  #   d<-ggplot(p, aes(x=sexo, y=frecuencia, fill=sexo)) +  theme(axis.title.x=element_blank(),
  #                                                               axis.text.x=element_blank(),
  #                                                               axis.ticks.x=element_blank())+
  #     geom_bar(stat="identity")
  #   d 
  #   
  # })
  # 
  # output$nivelplot <- renderPlotly({
  #   req(input$buscarbttn)
  #   p<-  filtrado()%>%group_by(nivel)%>%summarise(frecuencia=n())%>%drop_na()
  #   d<-ggplot(p, aes(x=nivel, y=frecuencia, fill=nivel)) +  theme(axis.title.x=element_blank(),
  #                                                                 axis.text.x=element_blank(),
  #                                                                 axis.ticks.x=element_blank())+
  #     geom_bar(stat="identity")
  #   d 
  #   
  # })
  # 
  # output$edadplot <- renderPlotly({
  #   req(input$buscarbttn)
  #   p<-  filtrado()%>%group_by(edad)%>%summarise(frecuencia=n())%>%drop_na()
  #   d<-ggplot(p, aes(x=edad, y=frecuencia, fill=edad)) +  theme(axis.title.x=element_blank(),
  #                                                               axis.text.x=element_blank(),
  #                                                               axis.ticks.x=element_blank())+
  #     geom_bar(stat="identity")
  #   d 
  #   
  # })
  
corps <- reactiveValues(prueba = NULL)

corpusstats <- reactive({
  
  conexioncorpus <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
  corpus <- dbGetQuery(conexioncorpus,"select hablantes.grupos,hablantes.intervenciones,hablantes.palabras,hablantes.spk,edad,sexo,nivel, conversaciones.source as conversacion, conversaciones.ciudad from hablantes left join  conversaciones on hablantes.source = conversaciones.source ")
  dbDisconnect(conexioncorpus)
  corpus
  
  
})

corpusstatscomplete <- reactive(
  
  corpusstats()%>%summarise(ciudades = n_distinct(ciudad,na.rm = TRUE),conversaciones = n_distinct(conversacion,na.rm = TRUE),grupos = sum(grupos,na.rm = TRUE),intervenciones = sum(intervenciones,na.rm = TRUE), palabras = sum(palabras,na.rm = TRUE), hablantes = n_distinct(spk,na.rm = TRUE))
  
)
output$completestats <- renderDT(corpusstatscomplete()) 
  
  observeEvent(input$filtrovariable!="", {
    corpus1 <- corpusstats()%>%mutate(seleccionada=!!input$filtrovariable)%>%select(seleccionada)
    
    updateSelectInput(session,"filtroselect","Selecciona",choices=unique(corpus1$seleccionada) )
    })
  
  observe(
    
    updateSelectizeInput(session,"conversselect","Selecciona",choices=unique(corpusstats()$conversacion) )
    
    
  )
  observe(
    updateVarSelectizeInput(session,"selecty","Variable para ordenadas", data=grupos()%>%select_if(is.numeric)%>%select(-time_start,-time_end),selected = "intensity_mean") )
    
  values <- reactiveValues(df_data = NULL)
  
  observeEvent(input$sahe,{
    conexionsahe <- dbPool(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )

    corpus2 <- conexionsahe%>%tbl("hablantes")

    corpus3 <- conexionsahe%>%tbl("grupos")

    corpus4 <- conexionsahe%>%tbl("conversaciones")

    corpus5 <- corpus3%>%left_join(corpus2%>%select(spk,sexo,edad,nivel), by="spk")%>%left_join(corpus4%>%select(source,ciudad),by="source")%>%show_query()%>% collect()
    # corpus2 <- dbGetQuery(conexionsahe,"select spk,sexo,edad,nivel from hablantes ")
    # corpus3 <- dbGetQuery(conexionsahe,"select * from grupos")
    # corpus4 <- dbGetQuery(conexionsahe,"select source,ciudad from conversaciones")
    
    poolClose(conexionsahe)
    # corpus2 <- corpus2%>%left_join(corpus3, by="spk")
    # corpus2 <- corpus2%>%left_join(corpus4,by="source")
    # corpus2
    values$prueba <- corpus5
  })

sahelanthropusgrupos <- eventReactive(input$sahe,{values$prueba})
# output$pruebasahe <- renderDT({sahelanthropusgrupos()})
  #dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
  # corpus2 <- dbGetQuery(conexionsahe,"select spk,sexo,nivel,edad from hablantes ")
  # corpus3 <- dbGetQuery(conexionsahe,"select * from grupos")
  # corpus4 <- dbGetQuery(conexionsahe,"select * from conversaciones")
  # 
  # dbDisconnect(conexionsahe)
  # corpus2 <- corpus2%>%left_join(corpus3, by="spk")
  # corpus2 <- corpus2%>%left_join(corpus4,by="source")
  # corpus2
  # %>%mutate(sexociudad = paste(stri_sub(sexo,1,1),stri_sub(ciudad,1,3),sep="_"),nivelciudad = paste(stri_sub(nivel,1,1),stri_sub(ciudad,1,3),sep="_"),edadciudad = paste(stri_sub(edad,1,1),stri_sub(ciudad,1,3),sep="_"),sexoniveledadciudad = paste(stri_sub(sexo,1,1),stri_sub(nivel,1,1),stri_sub(edad,1,1),stri_sub(ciudad,1,3),sep="_"))
  
  # })
  
  observe(
    updateVarSelectInput(session,"selnum","Selecciona variables numéricas",data=sahelanthropusgrupos()%>%select(where(is.numeric))) )
  observe({
    updateSelectInput(session,"selspk2","Selecciona variables numéricas",choices=unique(sahelanthropusgrupos()%>%select(!!input$selspk))) })

output$treetbl <- renderPrint(treedata())
treedata <- reactive({
  req(input$selspk2)
  p <- sahelanthropusgrupos()%>%mutate(varx = !!input$selspk)%>%filter(varx%in%!!input$selspk2)%>%select(varx,!!!input$selnum)%>%na.omit()%>%mutate(across(where(is_character),as_factor))
  if(!!input$seltreepkg=="party"){
  d <-ctree(varx~., data= p, control = ctree_control(maxdepth = 3))}else{
    d <- rpart(
      varx ~ ., 
      data = p, 
      method = "class", 
      minsplit = 2, 
      minbucket = 1
    )
    
  }
  d
})  
    
output$treeplot <- renderPlot({
  req(input$selspk2)
 if(!!input$seltreepkg == "party")
  
 { plot(treedata())}
else{fancyRpartPlot(treedata(), caption = NULL)}  
  })

anovaresult <- reactive({
  req(input$selspk2)
  p <- sahelanthropusgrupos()%>%mutate(varx = !!input$selspk,vary=unlist(!!!input$selnum))%>%filter(varx%in%!!input$selspk2)%>%select(varx,vary)
TukeyHSD(aov(vary~varx, data=p))
         }) 
anovaplot <- reactive({
  req(input$selspk)
  req(input$selspk2)
  p <- sahelanthropusgrupos()%>%mutate(varx = !!input$selspk,vary=unlist(!!!input$selnum))%>%filter(varx%in%!!input$selspk2)%>%select(varx,vary)
  d <-  ggplot(p, aes(x=varx, y=vary, fill=varx)) + 
    geom_boxplot(alpha=0.3, outlier.shape = NA) +
    theme(legend.position="none")
  d
})
output$anovaprint <- renderPrint({req(input$selspk2)
  anovaresult()})
output$anovaplot <- renderPlotly({  req(input$selspk2) 
  anovaplot()})

mapa <- reactive({
  req(input$selspk2)
  p <- sahelanthropusgrupos()%>%mutate(varx = !!input$selspk)%>%filter(varx%in%!!input$selspk2)%>%group_by(varx)%>%select(varx,!!!input$selnum)%>% summarize_if(., is.numeric, mean, na.rm = TRUE)%>%mutate_if(is.numeric, round, 2)
  p <- p %>%column_to_rownames(var="varx")
  
}
)

output$heatmap <- renderPlotly({
                               heatmaply(
  mapa(), 
  xlab = "Variables",
  ylab = "Categorías", 
  scale = "column",
  main = "Mapa de calor"
)})

output$heatmaptable <- renderDT({
  mapa()})

  observeEvent(input$filtrovariable2!="", {
    
    corpus2 <- corpusstats()%>%mutate(seleccionada=!!input$filtrovariable2)%>%select(seleccionada)
    
    
    updateSelectInput(session,"filtroselect2","Selecciona",choices=unique(corpus2$seleccionada))})
  
  output$plotcorpus <- renderPlotly({
    corpus <- corpusstats()
    req(input$groupcorpus!="")
    
# if(!!input$selstats=="palabras"){
# 
#    if(!is.null(input$filtroselect)&is.null(input$filtroselect2)){
# 
#       p<-  corpus%>%filter(!!input$filtrovariable%in%!!input$filtroselect)%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=sum(palabras))
# 
#     }else if(!is.null(input$filtroselect)&!is.null(input$filtroselect2)){p<-  corpus%>%filter(!!input$filtrovariable%in%!!input$filtroselect,!!input$filtrovariable2%in%!!input$filtroselect2)%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=sum(palabras))}else  if(!is.null(input$filtroselect)&is.null(input$filtroselect2))
#     {p<-  corpus%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=sum(palabras))}} 
#     
#     else {
#       
#       if(!is.null(input$filtroselect)&is.null(input$filtroselect2)) {p<-  corpus%>%filter(!!input$filtrovariable%in%!!input$filtroselect)%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=n_distinct(spk))}else 
#       
#       if(!is.null(input$filtroselect)&!is.null(input$filtroselect2)) {p<-  corpus%>%filter(!!input$filtrovariable%in%!!input$filtroselect,!!input$filtrovariable2%in%!!input$filtroselect2)%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=n_distinct(spk))
#       
#     } else {p<-  corpus%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=n_distinct(spk))}
#         
#       
#       }
    if(!!input$selstats=="palabras"){
      
        p<-  corpus%>%filter(if(!is.null(!!input$filtroselect)){!!input$filtrovariable%in%!!input$filtroselect}else{TRUE},if(!is.null(!!input$filtroselect2)){!!input$filtrovariable2%in%!!input$filtroselect2}else{TRUE})%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%filter(seleccionado!="NA")%>%summarise(frecuencia=sum(palabras))%>%na.omit()}
        
      else {p<-  corpus%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%filter(seleccionado!="NA")%>%summarise(frecuencia=n_distinct(spk))%>%na.omit()}
      
      
    
    d<-ggplot(p, aes(x=seleccionado, y=frecuencia, fill=seleccionado)) + theme_bw() + theme(axis.text.x=element_blank(),
axis.ticks.x=element_blank() 
    )+      geom_bar(stat="identity") 
    d 
    
  })
  
# output$pruebaDT <- renderDT({
#   corpus <-corpusstats()
#   p<-  corpus%>%filter(!!input$filtrovariable%in%!!input$filtroselect)%>%mutate(seleccionado=paste(!!!input$groupcorpus,sep="_"))%>%group_by(seleccionado)%>%summarise(frecuencia=n_distinct(spk))
#   p})

                    
  
  
  # output$hablanteplot <- renderPlotly({
  #   req(input$buscarbttn)
  #   p<-  filtrado()%>%group_by(spk)%>%summarise(frecuencia=n())%>%arrange(frecuencia)%>%head(n=10)%>%drop_na()
  #   d<-ggplot(p, aes(x=spk, y=frecuencia, fill=spk)) +
  #     geom_bar(stat="identity")+       theme(axis.title.x=element_blank(),
  #                                            axis.text.x=element_blank(),
  #                                            axis.ticks.x=element_blank())
  #   d
  #   
  # })
  # 
  observeEvent(input$linkconcord, {
    updateTabsetPanel(session, "tabset-default-id", "Concordancias")
  })
  observeEvent(input$gocorpus, {
    updateTabsetPanel(session, "tabset-default-id", "Búsqueda")
  })
  observeEvent(input$resetbttn,{reset("opciones")
    reset("bloque")})
  
output$totales <- renderValueBox({ valueBox(
 nrow(filtrado()%>%drop_na(token,intervenciones_id,ciudad)), "Total de palabras", icon = icon("list"),color = "navy"
)})
#   renderUI({
#  tags$div(tags$h3("Resultados:"),
#   tags$h3(nrow(filtrado()%>%drop_na(token,intervenciones_id,ciudad))))
# })
# }



grupos <- eventReactive(input$furiousbttn,{

conexiongrupos <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
q <- input$conversselect
grupos <- dbGetQuery(conexiongrupos,paste0("select * from grupos where source ='",input$conversselect,"'"))
dbDisconnect(conexiongrupos)
# q <- input$conversselect
# grupos <- grupos%>%filter(source==q)
grupos

})

grupos2 <- reactive({
grupos <- grupos()
grupos1 <- res_filter$filtered()%>%mutate(furious = "yes")
grupos <- grupos%>%left_join(grupos1%>%select(grupo_id,furious), by="grupo_id")%>%select(source,spk,content,time_start,time_end,furious)%>%mutate(furious = ifelse(is.na(furious),"no",furious))%>%arrange(source,time_start)})

res_filter <- filter_data_server(
  id = "filtering",
  data = reactive(grupos()%>%select(-source,-content,-row.names,-tier,-intervenciones_id,-intid)),
  name = reactive("grupos"),
  vars = reactive(names(grupos()%>%select(-spk,-time_start,-time_end,-grupo_id,-source,-content,-row.names,-tier,-intervenciones_id,-intid))),
  widget_num = "slider",
  widget_date = "slider",
  label_na = "Missing"

)


output$furioustbl <- renderDT({
  
  datatable(grupos2(),selection = "single",filter = list(
    position = 'top', clear = FALSE
  ))%>%formatStyle(
    'furious',
    target = "row",
    backgroundColor = styleEqual(c("no", "yes"), c('white', 'lightcoral')))
})
output$furiousplot <- renderPlotly({

  grupos <- grupos()
  grupos1 <- res_filter$filtered()%>%mutate(furious = "yes")
  grupos <- grupos%>%left_join(grupos1%>%select(grupo_id,furious), by="grupo_id")%>%mutate(selecty = !!input$selecty)%>%select(source,spk,content,time_start,time_end,furious,selecty)%>%mutate(furious = ifelse(is.na(furious),"no",furious),spk_furious=paste(spk,furious,sep="_"))
  
  ggplot(grupos, aes(x=time_start, y=selecty)) +    geom_segment( aes(x=time_start, xend=time_start, y=mean(selecty,na.rm=TRUE), yend=selecty), color="grey") +    geom_point( size=2,aes(colour = spk_furious)) +    ylab("Value of Y")} 
  
)


mediasspk1 <- reactive({

  grupos()%>%mutate(selecty = !!input$selecty)%>%summarise(media_general=mean(selecty,na.rm=TRUE))})
mediasspk2 <- reactive({
  
  grupos <- grupos()%>%mutate(selecty = !!input$selecty)%>%dplyr::group_by(spk)%>%summarise(media_por_hablante=round(mean(selecty,na.rm=TRUE),2))
 grupos
  })
output$mediasspk1 <- renderPrint({

  round(mediasspk1(),2)})
output$mediasspk2 <-renderPrint({

        as.data.frame(mediasspk2())

})



# output$mapaameresco <- renderLeaflet({
#   
#   leaflet(mapscities) %>%
#     addProviderTiles(providers$Esri.WorldTopoMap) %>%
#     addMarkers(~long, ~lat, 
#                label = ~city, 
#                labelOptions = labelOptions(textsize = "12px"),
#                popup = ~words)
  
  # 
  # leaflet()%>%addTiles(options=leaflet::tileOptions(noWrap = TRUE))%>% addMarkers(data=mapscities,~long, ~lat, popup = ~as.character(words), label = ~as.character(city))

  # })

# output$mapaameresco <- renderPlotly({
#   
# 
# 
#   world_map <- map_data("world")
#   ggplot(world_map, aes(x = long, y = lat, group = group)) +
# geom_polygon(fill="lightgray", colour = "white")+ geom_point(data=cities%>%filter(!is.na(words)), aes(x=long, y=lat,group=NA, size=words,colour=name))
#   # + geom_point(data=cities%>%filter(!is.na(words)), aes(group=NA,colour=name))
# })

observeEvent(input$furiousinfo, {
  showModal(modalDialog(
    title = "Sobre furioso",
    p("Como complemento al actual proyecto Esprint, dirigido por Marta Albelda y Maria Estellés, Furious (Furioso) explora las conversaciones del corpus Ameresco a partir del establecimiento por parte del investigador de una serie de criterios prosódicos y/o morfológicos que, en combinación, puedan considerarse como demarcativos de un estado de ánimo airado y que, por tanto, corresponda con la transmisión de un posible conflicto verbal."),
    p("Para más información, consúltese"),
    a(href="http://github.com/acabedo/furious","Github"),
    easyClose = TRUE,
    footer = NULL
  ))})

observeEvent(input$prosodiainfo, {
  showModal(modalDialog(
    title = "Sobre prosodia",
    p("En esta sección puede observarse el comportamiento de algunas variables fónicas según su cruce con algunas variables sociolingüísticas. Las variables utilizada son el rango tonal en semitonos, la inflexión tonal en semitonos, la F0 media en Hz, la intensidad, la velocidad de habla y la duración."),
    p("Puede visualizarse la información fónica mediante diagramas de caja para ver la mediana y los cuartiles o mediante gráficos lolipop para ver la media"),
    easyClose = TRUE,
    footer = NULL
  ))})

observeEvent(input$descargarinfo, {
  showModal(modalDialog(
    title = "Sobre descargar",
    p("En esta sección pueden encontrarse enlaces a los audios, los documentos de ELAN y las transcripciones (en PDF, Word o HTML). Debe seleccionar una conversación en el desplegable del panel de la izquierda, marcar la opción de formato deseada y pulsar el botón Descargar. La generación de la transcripción puede tardar unos 10-20 segundos."),
    easyClose = TRUE,
    footer = NULL
  ))})

observeEvent(input$saheinfo, {
  showModal(modalDialog(
    title = "Sahelanthropus",
    p("Sahelanthropus es un script muy simple que permite al investigador seleccionar variables de un corpus (Ameresco en este caso) y generar tres diferentes análisis: un árbol de decisiones, una prueba ANOVA (con diagrama de caja incluido) y un mapa de calor."),
    p("El principal objetivo es caracterizar variables idiolectales o sociolectales; se analiza, por tanto, tanto a los individuos, como usuarios con rasgos lingüísticos propios, como a los sociolectos, con comportamientos grupales generalizados en relación con el sexo, el nivel, la edad o la procedencia geográfica. Para más información:"),
    a(href="http://github.com/acabedo/sahelanthropus","Github"),
    easyClose = TRUE,
    footer = NULL
  ))})
observeEvent(input$esquisseinfo, {
  showModal(modalDialog(
    title = "Explorador con Esquisse",
    p("Esta sección utiliza el módulo Esquisse de la librería Datamod para permitir al investigador explorar de manera descriptiva las variables numéricas y categóricas correspondientes a los grupos entonativos del corpus Ameresco. Datamod-Esquisse puede encontrarse en el siguiente enlace:"),
    a(href="https://cran.r-project.org/web/packages/esquisse/vignettes/get-started.html","Enlace a Esquisse"),
    easyClose = TRUE,
    footer = NULL
  ))})

observeEvent(input$tutorialinfo, {
  showModal(modalDialog(
    title = "Tutorial",
    p("Puede consultar el tutorial de búsqueda en el siguiente enlace:"),
    a(href="https://github.com/acabedo/aroca/tree/main/tutorial","Enlace al tutorial de búsqueda"),
    easyClose = TRUE,
    footer = NULL
  ))})


observeEvent(input$generalinfo, {
  showModal(modalDialog(
    title = "Información general",
    p("Corpus Ameresco utiliza Oralstats Aroca para realizar tareas de consulta y minería de datos. Aroca consiste en una modificación del programa Oralstats, diseñado para la explotación, visualización y análisis de datos orales."),
    p("Diseñado y programado por lingüistas, Aroca está pensado para ayudar al investigador en su consulta del material real espontáneo. Se encuentra de manera gratuita en Github:"),
    a(href="https://github.com/acabedo/oralstats/","Enlace a Oralstats"),
    p(""),
    p("En cuanto al corpus Ameresco, todo material procedente de un corpus oral está siempre sujeto a posibles errores de transcripción o adscripción sociolingüística. Más allá de errores concretos, la aplicación de técnicas estadísticas sobre big data permite esbozar patrones generales de conducta en la realidad conversacional hispanohablante, sobre todo en el continente americano."),
    p("Esperamos que el uso de esta plataforma pueda ser de utilidad."),
    p("Más información sobre el corpus y el acceso gratuito a todas las conversaciones y transcripciones puede encontrarse en:"),
    a(href="https://www.corpusameresco.com","Corpus Ameresco"),
    p(""),
   p("Briz Gómez, Antonio; Albelda, Marta; Estellés, Maria (director y coordinadoras de Ameresco) y Cabedo, Adrián y Carcelén, Andrea (creadores de Oralstats Aroca)"),
    easyClose = TRUE,
    footer = NULL
  ))})

output$downloadfurious <- downloadHandler(
    filename = function() {
      paste('furious_', Sys.Date(),'_',Sys.time(), '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(x = grupos2(),file = con, sheetName = "furious")
      # write_delim(grupos2(), con,delim="\t",quote="none")
    })

output$downloadconcord <- downloadHandler(
  filename = function() {
    paste('concordancias_', Sys.time(), '.csv', sep='')
  },
  content = function(con) {
    write_excel_csv2(x = filtrado()%>%select(source,spk,content,sexo,nivel,edad,time_start),file = con, delim = "\t",quote = "none")
    # write_delim(grupos2(), con,delim="\t",quote="none")
  })

ngrams <- eventReactive(input$ngrambttn,{
  req(input$filterngrams1)
  if(input$filterngrams2=="ngrama3"){
    conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
    ngramstbl <- tbl(conexion2,"palabras")
    spktbl <- tbl(conexion2,"hablantes")
    convtbl <- tbl(conexion2,"conversaciones")
    
    ngrams <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(ngram=paste(lag(token,2),lag(token),token,sep=" "))%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado,ngram)%>%summarise(frecuencia=n())%>%arrange(desc(frecuencia))%>%collect()
    ngramstotal <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado)%>%summarise(total=n())%>%collect()
    ngrams2 <- ngrams%>%left_join(ngramstotal,by="seleccionado")%>%mutate(frecrel= round((frecuencia/total)*1000000),2)%>%arrange(desc(frecrel))
    dbDisconnect(conexion2)
    ngrams2%>%mutate(seleccionado=as.factor(seleccionado))%>%select(seleccionado,ngram,frecuencia,frecrel)%>%slice_head(n=100)} else if(input$filterngrams2=="ngrama2"){conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
    ngramstbl <- tbl(conexion2,"palabras")
    spktbl <- tbl(conexion2,"hablantes")
    convtbl <- tbl(conexion2,"conversaciones")
    
    ngrams <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(ngram=paste(lag(token),token,sep=" "))%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado,ngram)%>%summarise(frecuencia=n())%>%arrange(desc(frecuencia))%>%collect()
    ngramstotal <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado)%>%summarise(total=n())%>%collect()
    ngrams2 <- ngrams%>%left_join(ngramstotal,by="seleccionado")%>%mutate(frecrel= round((frecuencia/total)*1000000),2)%>%arrange(desc(frecrel))
    dbDisconnect(conexion2)
    ngrams2%>%mutate(seleccionado=as.factor(seleccionado))%>%select(seleccionado,ngram,frecuencia,frecrel)%>%slice_head(n=100)} else if (input$filterngrams2=="ngrama4"){
      
      conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
      ngramstbl <- tbl(conexion2,"palabras")
      spktbl <- tbl(conexion2,"hablantes")
      convtbl <- tbl(conexion2,"conversaciones")
      
      ngrams <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(ngram=paste(lag(token,3),lag(token,2),lag(token),token,sep=" "))%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado,ngram)%>%summarise(frecuencia=n())%>%arrange(desc(frecuencia))%>%collect()
      ngramstotal <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado)%>%summarise(total=n())%>%collect()
      ngrams2 <- ngrams%>%left_join(ngramstotal,by="seleccionado")%>%mutate(frecrel= round((frecuencia/total)*1000000),2)%>%arrange(desc(frecrel))
      dbDisconnect(conexion2)
      ngrams2%>%mutate(seleccionado=as.factor(seleccionado))%>%select(seleccionado,ngram,frecuencia,frecrel)%>%slice_head(n=100)
    }
  else{
    
    conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
    ngramstbl <- tbl(conexion2,"palabras")
    spktbl <- tbl(conexion2,"hablantes")
    convtbl <- tbl(conexion2,"conversaciones")
    
    ngrams <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(ngram=token)%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado,ngram)%>%summarise(frecuencia=n())%>%arrange(desc(frecuencia))%>%collect()
    ngramstotal <- ngramstbl%>%left_join(spktbl%>%select(sexo,edad,nivel,spk),by="spk")%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%mutate(seleccionado=paste(!!!input$filterngrams1,sep="_"))%>%group_by(seleccionado)%>%summarise(total=n())%>%collect()
    ngrams2 <- ngrams%>%left_join(ngramstotal,by="seleccionado")%>%mutate(frecrel= round((frecuencia/total)*1000000),2)%>%arrange(desc(frecrel))
    dbDisconnect(conexion2)
    ngrams2%>%mutate(seleccionado=as.factor(seleccionado))%>%select(seleccionado,ngram,frecuencia,frecrel)%>%slice_head(n=100)
    
  }
})

output$ngrams <- renderDT(datatable(ngrams(),filter = list(position = 'top', clear = FALSE),
                                    options = list(
                                      search = list(regex = TRUE, caseInsensitive = FALSE),
                                      pageLength = 10
                                    )))
prosodia <- eventReactive(input$prosodiabttn,{
  
  
  conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
  # grupostbl <- tbl(conexion2,"grupos")
  spktbl <- tbl(conexion2,"hablantes")
  convtbl <- tbl(conexion2,"conversaciones")
  
  grupos <- spktbl%>%left_join(convtbl%>%select(source,ciudad),by="source")%>%collect()
  
  dbDisconnect(conexion2)
  grupos 
})

output$prosodiaplot <- renderPlotly(
  {
    
    req(input$filterprosodia1)
    corpus <- prosodia()%>%mutate(x=!!input$filterprosodia2,y=!!input$filterprosodia1,z=!!input$filterprosodia4)%>%filter(!grepl("desconocido|Desconocido",x),!grepl("desconocido|Desconocido",z))%>%select(x,y,z)
    
    if(input$filterprosodia3 =="diagrama de caja")
    {   p<-ggplot(corpus, aes(x=x, y=y, fill=x)) +
      geom_boxplot() + facet_wrap(~z) +theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank() )
    
    p}
    else{
      p<-  ggplot(data = corpus%>%group_by(x,z)%>%summarise(y=mean(y,na.rm=TRUE),z=max(z)), aes(x= x,y =y,fill=x)) +geom_segment(aes(x = x, xend = x, y = 0, yend = y, color=x), lwd = 1) +
        geom_point(aes(fill = x),size = 6, pch = 21)+ facet_wrap(~z)+geom_text(aes(label = round(y,1), size = 5), color = "white", size = 2)+theme(axis.text.x=element_blank(),
                                                                                                                                                   axis.ticks.x=element_blank() )
      p}
  }
)
output$prosodiastats1 <- renderPrint({
  req(input$filterprosodia1)
  corpus <- prosodia()%>%mutate(x=!!input$filterprosodia2,y=!!input$filterprosodia1,z=!!input$filterprosodia4)%>%filter(!grepl("desconocido|Desconocido",x),!grepl("desconocido|Desconocido",z))%>%select(x,y,z)
  psych::describeBy(x =corpus$y,group = corpus$x)
  
})
output$prosodiastats2 <- renderPrint({
  req(input$filterprosodia1)
  corpus <- prosodia()%>%mutate(y=!!input$filterprosodia1,seleccionado=paste(!!input$filterprosodia2,!!input$filterprosodia4,sep="_"))%>%filter(!grepl("desconocido|Desconocido",seleccionado))%>%select(seleccionado,y)
  psych::describeBy(x =corpus$y,group = corpus$seleccionado)
  
})

conve <- reactive({
  
  conexion2 <- dbConnect(RPostgreSQL::PostgreSQL(),  host = "XXX",dbname="XXX", user = "XXX", password = "XXX" )
  # grupostbl <- tbl(conexion2,"grupos")
  spktbl <- tbl(conexion2,"hablantes")
  spk <- spktbl%>%collect()
  convtbl <- tbl(conexion2,"conversaciones")
  conv <- convtbl%>%collect()
  dbDisconnect(conexion2)
  conv%>%mutate(audio=paste("<a href='http://esvaratenuacion.es/sites/default/files/conversaciones/",source,".mp3'","target='_blank' download>audio</a>",sep=""),elan=paste("<a href='http://esvaratenuacion.es/sites/default/files/conversaciones/",source,".eaf'","target='_blank' download>elan</a>",sep=""))%>%select(source,ciudad,audio,elan,autoria,citacion)
  
})

observe( {
  # corpus1 <- conve()%>%mutate(seleccionada=!!input$conversaciondescargar)%>%select(seleccionada)
  corpus1 <- conve()%>%arrange(desc(source))
  updateSelectInput(session,"conversaciondescargar","Seleccione conversación para descargar",choices=unique(conve()$source))
})

output$conve <- renderDT(datatable(conve(),escape=FALSE))



output$downloadReport <- downloadHandler(
  filename = function() {
    paste('Corpus Ameresco', Sys.Date(), "Conversación",input$conversaciondescargar, sep = '_', switch(
      input$format, PDF = '.pdf', HTML = '.html', Word = '.docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('report.Rmd')
    src2<-normalizePath('CC-BY-NC-SA-4.0.jpg')
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
    file.copy(src2, 'CC-BY-NC-SA-4.0.jpg', overwrite = TRUE)
    
    library(rmarkdown)
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)

}


shinyApp(ui, server)
