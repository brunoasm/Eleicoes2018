#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(ggthemes)


#read data

#electoral results
load('summarized_results.Rdata') 
#map of zones
zonas = st_read('SP_ZONAS_janeiro_2018/ZONAS_FINAL.shp') %>%
  st_simplify(dTolerance = 0.001)
#map of municipality
municipios = st_read('SP-MUN/35MUE250GC_SIR.shp') %>%
  st_simplify(dTolerance = 0.001)



# Define UI 
ui <- fluidPage(
  
  pageWithSidebar(
    headerPanel('Votação dos deputados de SP por Zona Eleitoral'),
    sidebarPanel(
      selectInput('nivel', 'Nível', ''),
      selectizeInput('candidato','Candidato',''),
      selectInput('contagem','Contagem',c('Absoluta','Percentual')),
      tags$div(class="header", checked=NA, tags$p(),
               tags$p("Source code on github:", tags$a(href="https://github.com/brunoasm/Eleicoes2018", "brunoasm/Eleicoes2018", target="_blank")))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",plotOutput('plot1')),
        tabPanel("Tabela",dataTableOutput(outputId="tabela_votos"))
        )
    )
  )
)

# Define server logic 
server <- function(input, output,session) {
  
  
  updateSelectInput(session,
                    'nivel', 
                    'Nível',
                    levels(elec_filt$DS_CARGO_PERGUNTA))
  
  
  observeEvent(input$nivel, {
    req(input$nivel)
    nomes = elec_filt %>%
      dplyr::filter(DS_CARGO_PERGUNTA == input$nivel) %>%
      pull(nome_pt) %>%
      fct_drop %>%
      levels

    updateSelectizeInput(session,
                         inputId = 'candidato',
                         label = 'Candidato',
                         choices = nomes,options = list(maxOptions = 5000))
  })
  
  #ler mapa de zonas e juntar aos resultados eleitorais
  v = reactiveValues()
  observe({
    req(input$candidato)
    if (input$contagem == 'Absoluta'){
      v$map_data = elec_filt %>%
        dplyr::filter(DS_CARGO_PERGUNTA == input$nivel,
                      nome_pt == input$candidato) %>%
        select(NR_ZONA,VOTOS) %>%
        group_by(NR_ZONA) %>%
        summarise(VOTOS = sum(VOTOS)) %>%
        right_join(zonas,
                   by = c('NR_ZONA' = 'ZEFINAL')) %>%
        st_as_sf() %>%
        select(NR_ZONA,VOTOS) 
        
    } else {
      v$map_data = elec_filt %>%
        dplyr::filter(DS_CARGO_PERGUNTA == input$nivel,
                      nome_pt == input$candidato) %>%
        group_by(NR_ZONA) %>%
        summarise(porcentagem = sum(VOTOS)/sum(VOTOS/porcentagem)) %>%
        right_join(zonas,
                   by = c('NR_ZONA' = 'ZEFINAL')) %>%
        st_as_sf() %>%
        select(NR_ZONA,porcentagem) 
    }

  })
  
  #Preparar tabela de output
  observe({
    req(input$candidato)
    output$tabela_votos = renderDataTable({
      elec_filt %>%
        dplyr::filter(DS_CARGO_PERGUNTA == input$nivel,
                      nome_pt == input$candidato) %>%
        right_join(zonas,
                   by = c('NR_ZONA' = 'ZEFINAL')) %>%
        dplyr::filter(!is.na(NM_MUNICIPIO)) %>%
        transmute(Município = NM_MUNICIPIO,
                  Zona = NR_ZONA,
                  `Votos totais` = VOTOS,
                  `Votos (porcentagem)` = scales::percent(porcentagem) %>% 
                    str_pad(side = 'left', pad = "0",width = max(str_length(.),1))
                  )
    },
    options = list(lengthMenu = c(10, 25, 50,100), pageLength = 10))
  })
  

  #plotar mapa
  output$plot1 = renderPlot({
    req(v$map_data)
    
    compute_breaks_int = function(lims){
      seq(0,sqrt(lims[2]),length.out = 6)^2 %>% round() %>% unique
    }
    
    compute_breaks_float = function(lims){
      seq(0,sqrt(lims[2]),length.out = 6)^2  %>% unique
    }
    
    
    req(v$map_data)
    if (input$contagem == 'Absoluta'){
    ggplot(v$map_data) +
      geom_sf(aes(fill=VOTOS, color= VOTOS),size=0.1) +
      geom_sf(data=municipios,fill=NA,color='black',size=0.15) +
      scale_fill_viridis_c(aesthetics = c('colour','fill'),
                           trans='sqrt',
                           breaks = compute_breaks_int) +
      coord_sf(datum=NA) +
      theme_map() +
      theme(panel.grid = element_blank(),
            legend.key.size = unit(.05,'npc'),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12))
    } else {
      ggplot(v$map_data) +
        geom_sf(aes(fill=porcentagem, color= porcentagem),size=0.1) +
        geom_sf(data=municipios,fill=NA,color='black',size=0.15) +
        scale_fill_viridis_c(aesthetics = c('colour','fill'),
                             labels = scales::percent,
                             trans='sqrt',
                             breaks = compute_breaks_float,
                             guide = guide_colorbar(title='Porcentagem\nda zona')) +
        coord_sf(datum=NA) +
        theme_map() +
        theme(panel.grid = element_blank(),
              legend.key.size = unit(.05,'npc'),
              legend.text = element_text(size=12),
              legend.title = element_text(size=12))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

