
main_dir="C:\\Projects\\NUT2_GR_DB\\cys_smps"
#setwd(main_dir)

#
# if !require
library(leaflet)
library(shiny)
library(rgdal)
library(plotly)
library(bslib)
library(readxl)


#states <- readOGR("periferies.geojson")
#save(states,file='periferies.rdata')
# Load data -----
load(file='periferies.rdata')

inst_cap<-read.csv('inst_capacity_entsoe_gr_2015_2021.csv',sep=';')
cols<-inst_cap$type
rows<-rownames(inst_cap)
inst_cap<-inst_cap[,-1]
inst_cap<-t(inst_cap)
colnames(inst_cap)<-cols

gen_mix<-read.csv("dam_approx_values.csv",sep=';',dec=',')
gen_mix_dates<-gen_mix$date1[seq(1,nrow(gen_mix),by=24)]


subs<-read_xlsx(list.files(pattern = "deddhe") )

res_lics<-read.csv("wind_pv_licensses_data.csv",sep=';',dec=',')
res_lics<-res_lics[res_lics$status_2=="operation",]

# -----

ui <- navbarPage("NUTS2 GR", id="nav",
                 #theme = bs_theme(version = 4, bootswatch = "cerulean"),
                 
                 # First Panel         
                 tabPanel("Data explorer",
                          titlePanel('Generation mix'),
                          fluidRow(
                              column(2,
                                   selectInput("date1", "Date",
                                               choices=gen_mix_dates ,
                                               selected="1/11/2020",
                                               multiple=FALSE)
                              )
                          ),
                          #hr(),
                          DT::dataTableOutput("gen_mix_tb",height = 100),
                          hr(),
                          plotOutput("plot1")
                 ),           
                 
                 # Second Panel
                 tabPanel("Installed capacity",
                          titlePanel('Installed capacity (MW)'),
                          
                          DT::dataTableOutput("inst_cap_tb")
                 ),
                 
                 # Map panel
                 tabPanel("Interactive map",
                          titlePanel("HEDNO substations map & RES licensses"),
                          column(2, 
                                 wellPanel(
                                   selectizeInput(inputId = "nuts2_reg",
                                                  label= "NUTS2 Region",
                                                  choices=c("All",names(table(subs$nuts2_en)) ),
                                                  selected="Attica",
                                                  multiple=TRUE,
                                                  options = list(
                                                              'plugins' = list('remove_button'),
                                                              'create' = TRUE,
                                                              'persist' = FALSE) 
                                                  ),
                                   radioButtons("res_type", "RES type",
                                               choices=c("All","Wind",'PV') ,
                                               selected="All"),
                                   checkboxInput("subs_disp","Display HEDNO substations")
                                   
                                 ),
                     
                          ),
                          hr(), 
                          leafletOutput("mymap", width = "60%", height = "900")
                 )
                                  
)




pal <- colorNumeric("viridis", NULL)


server <- function(input, output, session) {
    output$gen_mix_tb <- DT::renderDataTable({
          colnames(gen_mix)[1]<-"Hours"
          gen_mix[gen_mix$date1==input$date1,-2]
    })
  
  
  
    output$plot1 <- renderPlot({
      tb=gen_mix[gen_mix$date1==input$date1,4:8]
      tb[is.na(tb)]<-0
      par(mar=c(8,5,2,2))
      cols<-c("#676467",'#F3CF71','#67E9DF','#5EB97D','#ADB0AE')
      barplot(as.matrix(t(tb)),
              col =cols,border=cols,
              xlim=c(1,28),ylim=c(0,1.2*max(apply(tb,1,sum)) ),
              tck=-0.01,xlab="Hours",ylab="Production (MWh)",
              cex.axis = 0.9,cex.names=0.9,names.arg = 1:24,width=0.5)
      legend("topleft",
             legend = colnames(tb),pch = 15,col = cols,bty='n')
      
      
      #fig <- plot_ly(tb, x = 1:nrow(tb), y = ~LIGNITE, type = 'bar', name = 'LIGN')
      #fig <- fig %>% add_trace(y = ~GAS, name = 'NG')
      #fig <- fig %>% add_trace(y = ~RES, name = 'RES')
      #fig <- fig %>% add_trace(y = ~HYDRO, name = 'HYDRO')
      #fig <- fig %>% add_trace(y = ~tot_imps, name = 'IMPS')
      #fig <- fig %>% layout(yaxis = list(title = 'Production (MWh)'), barmode = 'stack')
      #fig
    })
 
  
    # Data Explorer
    output$inst_cap_tb <- DT::renderDataTable({
          inst_cap
    })
    
    
    # map
    output$mymap <- renderLeaflet({
      message('reading geojson....')
      
      map<-leaflet(states) %>%
        setView(lng = 24.36, lat = 38.3601, zoom = 6.5) %>%
        
        # add Tiles
        addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter",
                         options = providerTileOptions(opacity =1)  ) %>%
        addProviderTiles("OpenStreetMap",  group = "OpenStreetMap") %>%
        addProviderTiles("Stamen.Toner",group = "Stamen.Toner") %>%
        addProviderTiles("Stamen.Terrain",group = "Stamen.Terrain") %>%
        addProviderTiles("Esri.WorldStreetMap",group = "Esri.WorldStreetMap") %>%
        addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron",
                         options = providerTileOptions(opacity = 2)) %>%
        addLayersControl(baseGroups = c("CartoDB.DarkMatter","OpenStreetMap",
                                        "Stamen.Toner","Stamen.Terrain", "Esri.WorldStreetMap",
                                        "CartoDB.Positron","CartoDB.Positron"),
                         position = "topleft") %>%
        
        addPolygons(stroke=FALSE,fillOpacity = 0.351,
                    fillColor =pal( sample(1:80,13) ),#"#f2d8b3",
                    weight = 0.9,smoothFactor = 0.1,
                    label=c('East Macedonia & Thrace', 'Central Macedonia',
                            'Western Macedonia','Epirus','Thessaly','North Aegean',
                            'South Aegean','Central Greeece','Western Greece',
                            'Peloponesse','Ionian islands','Crete','Attica')
        )
    })
    
    
    
    observe({
      
      if (length(input$nuts2_reg)==1) {
        if (input$nuts2_reg=="All") {
          res_lics_reg<-res_lics
        } else {
          res_lics_reg<-res_lics[res_lics$region==input$nuts2_reg,]
        }
        if (input$res_type=="Wind" | input$res_type=="PV") {
          res_lics_reg<-res_lics_reg[res_lics_reg$res_type==input$res_type,]
        }
        
        leafletProxy("mymap") %>% 
          clearMarkers() %>%
          addCircleMarkers(lng=res_lics_reg$x,lat=res_lics_reg$y,
                           color =ifelse(res_lics_reg$res_type=="PV",'yellow','blue'),
                           fillColor = ifelse(res_lics_reg$res_type=="PV",'yellow','blue'),
                           radius=2,opacity=0,fillOpacity = 1)
        
      } else if (length(input$nuts2_reg)>1) {
        
        res_lics_reg<-res_lics[res_lics$region %in% input$nuts2_reg,]
        if (input$res_type=="Wind" | input$res_type=="PV") {
          res_lics_reg<-res_lics_reg[res_lics_reg$res_type==input$res_type,]
        }
        
        leafletProxy("mymap") %>% 
          clearMarkers() %>%
          addCircleMarkers(lng=res_lics_reg$x,lat=res_lics_reg$y,
                           color =ifelse(res_lics_reg$res_type=="PV",'yellow','blue'),
                           fillColor = ifelse(res_lics_reg$res_type=="PV",'yellow','blue'),
                           radius=2,opacity=0,fillOpacity = 1)
        
      } else if(length(input$nuts2_reg)==0) {
        leafletProxy("mymap") %>% 
          clearMarkers()
      }
      
      if (input$subs_disp==TRUE) {
         leafletProxy("mymap") %>% 
           clearMarkers() %>%
           addCircleMarkers(lng=subs$SYNM,lat=subs$SYNP,
                           color =ifelse(subs$margin_icon=="Green",'green','red'),
                           fillColor = ifelse(subs$margin_icon=="Green",'green','red'),
                           radius=subs$thermal_margin,opacity=0.4,fillOpacity = 0.2)
      }
      
      
    })
}

shinyApp(ui, server)
