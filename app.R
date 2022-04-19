#======================================================================
#----------------------------- APPS CCS.02
#====================================================================
# 
# library(haven)
library(tidyverse)
library(shinyjs)
# #library(rsconnect)
# 
# #------Kibreria de mapas
library(leaflet)
# library(sp)
# library(sf)
# library(rgdal)
# library(RColorBrewer)
# library(tmap)
library(shiny)
library(shinydashboard)
# library(sjmisc)
library(rpivotTable)
# library(lubridate)
# library(anytime)
# #library(RPostgreSQL)
# #library(DBI)
library(stringr)
# library(htmlwidgets)
# #library(hash)
# #library(pool)


#-----------Base   
#-----------Base  


# Define UI for application that draws a histogram
ui <- dashboardPage( skin = "green",
                     
                     dashboardHeader(title="Citizen Complaint System CCS, Dedo Ciudadano",titleWidth=800),
                     dashboardSidebar(
                       
                       #-----------Logo  
                       
                       
                       
                       h2("Visualización de las Denuncias",align = "center",style = "color:yellow"),
                       a(href="https://jarecdata.com/", img(src='JarecData.jpg',style="margin-top: 0px; padding-left:36px;padding-bottom:5px", height = 73)),
                       hr(),
                       
                       div("El sistema de denuncia ciudadana es una potente herramienta tecnológica elaborada por Jarecdata para que la ciudadanía reporte 
               problemas a nivel comunitario o local para que sean resueltos de manera más efectiva",align = "center",style = "color:white")#,
                       
                       
                     ),
                     
                     
                     dashboardBody(
                       
                       
                       # Show a plot of the generated distribution
                       
                       fluidRow(
                         tabBox(title = p(strong("MENU PRINCIPAL")),width = 12,height = "2000px",    
                                
                                tabPanel("Mapa",
                                         leafletOutput("myMap")#,
                                         #box(title = "Registro", status = "primary", solidHeader = TRUE,collapsible = TRUE, DT::dataTableOutput("Base"),width = 14,height = "auto")
                                         
                                         ),
                                tabPanel("Base", box(title = "Base de Datos", status = "success", solidHeader = TRUE,collapsible = TRUE, DT::dataTableOutput("Base2"),width = 14,height = "auto")),
                                tabPanel("Pivote Table",box(title = "Pivote Table", status = "success", solidHeader = TRUE,collapsible = TRUE,rpivotTableOutput("Pivote",width = "20%", height = "100px"),width = 20,height = "1000px")))
                         
                         
                       )
                     )
                     
)






# --------------------------------------------------Server----------------------------------------------



server <- function(input, output) {
  
  
  #----------------------------------Lanzamiento de mapa
  
  
    output$myMap <- renderLeaflet({
    
    
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'blue'
    )
    
    
    #OpenStreetMap.HOT;OpenStreetMap.France;OpenStreetMap.DE
    
    link<-base$data.S01.P03_FOTO_PROBLEMA   
   
    
    
    
    leaflet(base)%>%
      addTiles()%>%
      addProviderTiles('OpenStreetMap.France')%>%
      setView(lat=13.7941847, lng=-88.8965302, zoom = 10)%>%
      addAwesomeMarkers(base,lat=base$Lat,lng=base$Lng,
                        popup=paste(sep="<br/>","<b> Ir al link para observar la Fotografía del Problema:</a></b>",link,"<b> Fecha del Problema:</a></b>",base$data.S01.P01_FECHA_HORA),label=~as.character(base$data.S01.P05_PROBLEMA ),icon=icons)#%>%
    #paste(a(href=link,"link"))
  })
  
  #---------------------------------Lanzamiento de Base
  
  
  base<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vShQ7B6hR53HOqS2OIAeNFI5eMeoUCZhq3ig_Y3a4MI0_aF4VGYBStO1pD0kGK6GYZ7VbYMx6TqImDY/pub?gid=0&single=true&output=csv")
  base <- base %>%
    dplyr::mutate(Lat=as.numeric(str_sub(data.S01.P02_GEORREFERENCIA,1,10))) %>% 
    dplyr::mutate(Lng=as.numeric(str_sub(data.S01.P02_GEORREFERENCIA,12,22))) 
  
  #attach(base)
  
  data<-base%>%
    select(ID="data.meta.instanceID",Dimension="data.S01.P04_DIMENSION",FechaHoraProblema="data.S01.P01_FECHA_HORA",Latitud="Lat",
           Longitud="Lng",Problema="data.S01.P05_PROBLEMA",OtroProblema="data.S01.P05.01_OTRO_PROBLEMA",Institucion="data.S01.P06_INSTITUCION",
           OtraInstitucion="data.S01.P07_OTRA_INSTITUCION",Depto="data.S01.P08_DEPARTAMENTO",Muni="data.S01.P09_MUNICIPIO",ReferenciaGeo="data.S01.P10_DIRECCION_REFERENCIA",Nota="data.S01.P12_ESCALA_GOBIERNO_LOCAL") %>%
    #mutate(FechaHoy=(as_date(today(tzone="America/El_Salvador"))))%>%
    mutate(FechaHoy=Sys.Date())%>%
    #mutate(FechaInicial=as_date(FechaHoy,tz="America/El_Salvador"))%>%
    mutate(FechaInicial=as.Date(FechaHoraProblema,"%d/%m/%y"))%>%
    mutate(PeriodoDias=as.numeric(FechaHoy-FechaInicial))%>%
    mutate(mean(Nota))
  
 
  
  #data<-dataData
  
  
  
  # refreshData <- reactive({
  #   invalidateLater(120, session) 
  #   data (pool)
  # }) 
  
  
 
  
  #---------------------------------Lanzamiento del menu 
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  }) 
  
  #---------------------------------Lanzamiento de Base2
  
  output$Base2<- DT::renderDataTable(
    
    
    #DT::datatable({data2<-data[,c(1,8,9,14,15)]
    DT::datatable({data[,c(1,6,7,3,10,11,12,16)]#%>%filter(data$Muni=="0510")
      
      
    },
    options=list(lengthMenu=list(c(7,15,-1),c('5','15','Todo')),
                 pageLength=15),
    filter="top",
    selection="multiple",
    style='bootstrap' 
    
    ))
  
  #-------------------------------Lanzamiento del graf 2
  output$Pivote<-rpivotTable::renderRpivotTable({
    
    rpivotTable(data[,c(2,6,7,3,10,11,16)]#%>%filter(data$Muni=="0510")
    )
    
  }) 
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
