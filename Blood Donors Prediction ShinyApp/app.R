overall = read.table(text = 'Year 2001 2003 2004 2005 2006 2007
                              Donated 0 0 1 20 153 4
                              Not-Donated 2 4 1 221 332 10', header=T)
names(overall) <- c("Year", "2001", "2003", "2004", "2005", "2006", "2007")

df = data.frame(
  year = c(2001,2003,2004,2005,2006,2007),
  donated = c(0,0,1,20,153,4),
  not_donated = c(2,4,1,221,332,10)
)

library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(reactable)
library(shinyfilter)
library(leaflet)      #map
library(RColorBrewer) #map
library(highcharter)

blood <- read.csv("blood-transfusion-service-center.csv")
donation_location_df <- read.csv('New WebScraping - PDN Locations - Cleaned.csv')


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"
#Panel 1 pre data processing:
panel1 <- read.csv("BD.csv", stringsAsFactors = FALSE, header = TRUE, encoding = "UTF-8")

#Panel 2 pre data processing:
donation_location_df$state[donation_location_df$state=="KL"]<-"KUALA LUMPUR"


#Panel 3 pre data processing:
blood <- read.csv("blood-transfusion-service-center.csv", stringsAsFactors = FALSE, header = TRUE, encoding = "UTF-8")
#add new column (Patient ID)
blood['Patient ID'] <- c(1:748)
#move "Patient ID" to first column
bloods <- blood%>%
  select(`Patient ID`, everything())
#Filter "Last blood donation" to 4 until 24 months only.
bloods <- bloods[bloods$V1 %in% 4:24,]
#add new column to calculate percentage
bloods['Percentage']<-round((bloods['V2']/15)*100,0)
bloods['Percentage'][bloods$Percentage<49,] <- "Low"
bloods['Percentage'][((bloods$Percentage!= "Low" & bloods$Percentage>50) | bloods$Percentage<=100), ]  <- "High"
bloods['Percentage'][(bloods$Percentage != "Low"& bloods$Percentage != "High"), ] <- "Very High"
#Rename:
names(bloods)[names(bloods) == 'Percentage'] <- "Potential Blood Donors ?"
names(bloods)[names(bloods) == 'V1'] <- "Months since last donation"
names(bloods)[names(bloods) == 'ID'] <- "Patient ID"
names(bloods)[names(bloods) == 'V2'] <- "Total number of donation"
names(bloods)[names(bloods) == 'V3'] <- "Donated Blood Volumne (cc)"
#drop 2 columns
bloods <- bloods %>% select (-c(V4, Class))


#UI interface:
ui <- fluidPage(
  navbarPage("Blood Donors Prediction Model", theme = shinytheme("flatly"),
             # Panel 1 by Ainul and Siew Ling:
             tabPanel("Main", fluid = TRUE, icon = icon("glyphicon glyphicon-heart", lib="glyphicon"),
                      tags$style(button_color_css),
                      dashboardBody(
                        ## Using box to display plots
                        tabItem(tabName = "Summary",
                                box(title = "Descriptive Statistics of Blood Donors Dataset", status = "primary", solidHeader = T, width = 12,
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarMenu(
                                          
                                        ),
                                        tabBox(tags$head(
                                          tags$style(HTML("#plot2{ height:100vh !important; }"))
                                        ),
                                        
                                        id="tabchart1", width = "250px",
                                        tabPanel("Overall Summary", plotlyOutput("plot2")),
                                        tabPanel("Percentage of Potential Blood Donors", plotlyOutput("plot1")),
                                        #add new tab:
                                        tabPanel("Blood Donors History", 
                                                 selectizeInput("years", "Years", choices = unique(df$year), selected = unique(df$year)[5]), highchartOutput("plot"))
                                        )
                                      )
                                      
                                    )
                                    )
                        ))
             ),
             
             # Panel 2 by Jia Ling and Zheng Lim:
             tabPanel("Campaigns", fluid = TRUE, icon = icon("hospital"),
                      # Application title
                      titlePanel("Malaysia Blood Donation Campaigns 2022"),
                      ############CSS For MAP FILTER#############
                      tags$style("
              #controls {
                /* Appearance */
                background-color: white;
                padding: 0 20px 20px 20px;
                cursor: move;
                /* Fade out while not hovering */
                opacity: 0.55;
                zoom: 0.9;
                transition: opacity 500ms 0.1s;
              }
              #controls:hover {
                /* Fade in while hovering */
                opacity: 0.95;
                transition-delay: 0;
              }
               "),
                      ############CSS For MAP FILTER#############
                      
                      
                      
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    top = 220, right = "auto", left = 90, bottom = "auto", 
                                    draggable = TRUE,
                                    tags$i(h3("Pick a Date/Location")),
                                    
                                    # Calendar
                                    dateInput("map_date", label = "Date Selection", value = "2022-05-01", format = "yyyy-mm-dd"),
                                    
                                    
                                    fluidRow(column(3, verbatimTextOutput("value"))),
                                    selectizeInput(inputId = "map_state", label = "State",
                                                   multiple = FALSE,choices = donation_location_df$state, selected = c("1")   #I HAVE NO IDEA why C("1") = KL
                                    ),
                                    style = " z-index: 10;" ## z-index modification
                                    
                      ),
                      
                      # Map
                      
                      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      leafletOutput("map", width = "100%", height = "80vh"),
                      #leafletOutput("map", height = 800),
                      #absolutePanel(top = 10, right = 10,)
                      
                      
             
             ),
             
             # Panel 3 by Xin Yang:
             tabPanel("Potential Blood Donors", fluid = TRUE, icon = icon("stethoscope"),
                      titlePanel("Blood Donors Database"),
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          
                          selectizeInput(inputId = "sel_percentage", label = "Potential Blood Donors ?",
                                         multiple = TRUE,choices = sort(unique(bloods$`Potential Blood Donors ?`)), 
                                         options = list(onChange = event("ev_click"))),
                          
                          
                          use_tooltips(background = "#1B3F8C", foreground = "#FFFFFF")
                        ),
                        mainPanel(
                          reactableOutput(outputId = "tbl_cars")
                        )
                      )
             )
                      
             )
             
             )

             
########
        ##tabItem(tabName = "charts1",
        ##        fluidRow(
        ##          tabBox(id="tabchart2",
        ##                 tabPanel("Analysis on Blood Donor", plotlyOutput("plot1")),
        
        ##          )
        ##        )
        




server <- function(input, output, session){

  #Panel 1 Function by Ainul and Siew Ling:
  #choices <- reactive(panel1$year)
  observeEvent(input$jump_to_summary, {
    updateTabsetPanel(session, "intabset", selected = "summary")
  }
  #input$tabselected,{
  #updateSelectInput(session, 'selected,indicator', choices = choices[[input$tabselected]])
  )
  
  ## For Panel 1: Blood Donors History tab.
  reactivedf <- reactive({
    filtereddf <- df %>%
      dplyr::filter(year == input$years)
    filtereddf
  })
  
  output$plot <- renderHighchart({
    highchart() %>%
      hc_add_series(type = "column", reactivedf()$donated, name = "Donated") %>%
      hc_add_series(type = "column", reactivedf()$not_donated, name = "Not Donated") %>%
      hc_xAxis(labels = list(enabled = FALSE)) %>%
      hc_title(text = input$years)
  })
  
  ## Plotly Histogram
  output$plot1 <- renderPlotly({
    
    fig4<-plot_ly(data=panel1, 
                  labels = ~panel1$Class, 
                  #values = ~X1960,
                  type="pie")
    
    fig4 <- fig4 %>% layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      legend = list(title=list(text='<b> Legend </b>'))
    )
    
    fig <- subplot(fig4) #%>%
    
  })
  
  ## Plotly Histogram
  output$plot2 <- renderPlotly({
    fig1<-plot_ly(data=panel1,
                  x=~panel1$Monetary,
                  type="histogram",
                  name="Total blood donated in cc")%>%
      
      layout(plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))
    
    fig2<-plot_ly(data=panel1,
                  x=~panel1$Recency,
                  type="histogram",
                  name="Months Since Last Donation")%>%
      
      layout(plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))
    
    fig3<-plot_ly(data=panel1,
                  x=~panel1$Frequency,
                  type="histogram",
                  name="Total Number of Donation")%>%
      
      layout(plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))
    
    fig5<-plot_ly(data=panel1,
                  x=~panel1$Month1,
                  type="histogram",
                  name="Months since First Donation")%>%
      
      layout(plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))
    
    fig <- subplot(fig1, fig2, fig3, fig5, nrows = 4) 
    fig <- fig %>% layout(legend = list(orientation = 'h')) #%>%
  })
  
  output$data <- renderTable({
    panel1[,c("mpg", input$variable), drop=FALSE]
    
  }, rownames = TRUE);

#Panel 2 Function by JiaLing and Zheng Lim:
  # Calendar to pick the date
  #renderPrint <- reactive({ req(input$date)})
  # Reactive expression for the data subsetted to what the user selected
  donation_location_df$DATE = as.Date(donation_location_df$DATE, format = "%d/%m/%Y")
  
  filteredData <- reactive({
    req(input$map_state)
    req(input$map_date)
    
    df <- donation_location_df[donation_location_df$state == input$map_state & donation_location_df$DATE == input$map_date,]
    df
  })
  
  
  output$map <- renderLeaflet({
    req(input$map_state)
    req(input$map_date)
    
    leaflet(donation_location_df) %>% addTiles() %>%
      fitBounds(~min(101.303146), ~min(2.8035642), ~max(101.8586387), ~max(3.2879646))
  })
  
  
  leafIcons <- icons(
    iconUrl = ifelse(donation_location_df$CampaignType == "PUSAT PENDERMAAN STATIK",
                     "https://www.manokamnahospital.in/images/icon/location.svg",
                     "https://cdn0.iconfinder.com/data/icons/refugee-filledoutline/64/TENT-red_cross-hospital-first_aid-tent-512.png"
    ),
    iconWidth = 38, iconHeight = 38,
    shadowUrl = "https://leafletjs.com/SlavaUkraini/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
  )
  
  
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(lng = filteredData()$long, lat = filteredData()$lat, popup = paste(filteredData()$location, "<br>", "Date:", filteredData()$DATE, "<br>", "State:", filteredData()$state), icon = leafIcons
      )
  })
  
#Panel 3 Function by XY:
  r <- reactiveValues(mybloods = bloods)
  
  define_filters(input,
                 "tbl_cars",
                 c(sel_percentage = 'Potential Blood Donors ?'
                 ),
                 bloods)  
  
  
  
  observeEvent(input$ev_click, {
    r$mybloods <- update_filters(input, session, "tbl_cars")
    update_tooltips("tbl_cars", 
                    session, 
                    tooltip = TRUE, 
                    title_avail = "Available is:", 
                    title_nonavail = "Currently not available is:",
                    popover_title = "My filters",
                    max_avail = 10,
                    max_nonavail = 10)
  })
  
  
  output$tbl_cars <- renderReactable({
    reactable(data = r$mybloods,
              filterable = TRUE,
              rownames = FALSE,
              selection = "multiple",
              showPageSizeOptions = TRUE,
              paginationType = "jump",
              showSortable = TRUE,
              highlight = TRUE,
              resizable = TRUE,
              rowStyle = list(cursor = "pointer"),
              onClick = "select"
    )
  })
  

}
 

shinyApp(ui = ui, server = server)
