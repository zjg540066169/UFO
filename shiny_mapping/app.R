#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(DT)
library(rgdal)


ufo_data = read_csv(file = "../data/tidied_data_final.csv")

ufo =
    ufo_data %>%  
    na.omit(ufo_data) %>% 
    separate(date_time, into = c( "date","time"), sep = " " ) %>%
    separate( date, into = c("month","day","year"), sep = "/") %>%
    mutate(state = recode(state , 
                          "Alabama"="AL", "Oregon" = "OR","Ohio" = "OH","New York" = "NY","New Mexico" = "NM", 
                          "Alaska"="AK", "Nevada" = "NV","Nebraska" = "NE", "Florida" ="FL","Georgia"="GA",
                          "Arizona" = "AZ","Idaho"="ID","Illinois" ="IL","Indiana"="IN","Iowa" ="IA",
                          "Kansas"="KS","Missouri" ="MO","Kentucky" = "KY","Louisiana"="LA","Maine"="ME",
                          "Maryland"="MD","Massachusetts" = "MA","Michigan"="MI","Minnesota"="MN",
                          "California" = "CA","Texas" = "TX","Tennessee" = "TN","Pennsylvania" = "PA",
                          "Colorado" = "CO","Washington" = "WA","Vermont" = "VT","Utah" = "UT"
    ))


#### mapping
US =
    
    ufo %>%
    filter(country == "USA") %>%
    group_by(state) %>%
    summarise(Num.UFO = n()) 


states = readOGR("../data/cb_2018_us_state_500k.shp")

#check for the matching bewtween shapefile and my US dataset
is.element(US$state,(states$STUSPS))
is.element(states$STUSPS,US$state)

# missing 14,38,39,46,45 so to create new_states df with only matching states
new_states = subset(states, is.element(states$STUSPS,US$state))
is.element(new_states$STUSPS,US$state)
is.element(US$state,new_states$STUSPS)

## make it into same order
US = US[order(match(US$state, (new_states$STUSPS))),]

# set for the range based on number being observed--discrete
# bins = c(0,50,100,150,200,250,300,350,400,450,500,550)
# pal = colorBin("YlOrRdBu",domain = US$Num.observed,bins = bins)

pal <- colorNumeric(palette = "Reds",domain = c(0,1))
labels = paste( "<p>","Number of UFO Observed from 1925 to 2015 ：", US$Num.UFO,"<p>",sep = "")
leaflet() %>%
    setView(-96,37.8,4) %>%
    addTiles() %>%
    addPolygons(data = states,
                weight = 1,
                smoothFactor = 0.2,
                color = "white",
                fillOpacity = 0.75,
                fillColor = pal(US$Num.UFO),
                label = lapply(labels, HTML)
    ) %>%
    addLegend(pal = pal,
              values = US$Num.UFO,
              opacity = 0.7,
              position = "topright")






# Define UI for application that draws a histogram

ui = dashboardPage(
    skin = "red",
    dashboardHeader(title="Observing UFO"),
    dashboardSidebar(
        sliderInput("date_range", label = "Date Range",
                    min = min(US$year),max = max(US$year),
                    value = c(min(US$year),max(US$year)),
                    sep = "",step =1)
),
dashboardBody(
    fluidRow(box(width = 12,leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
data_input = reactive({ US %>% filter(year>= input$date_range[1])%>%
        filter(year<= input$date_range[2]) %>%
        group_by(state) %>%
        summarise(Num.UFO = n()) 
    
})
data_input_ordered = reactive({
    data_input()[order(match(data_input()$state,states$STUSPS)),]
})
labels = reactive({
    paste("<p>","Number of UFO Observed Across Time ：", data_input_ordered$Num.UFO,"</p>",
          sep = "")})
output$mymap = renderLeaflet(
   
     leaflet() %>%
        setView(-96,37.8,4) %>%
        addTiles() %>%
        addPolygons(data = states,
                    weight = 1,
                    smoothFactor = 0.2,
                    color = "white",
                    fillOpacity = 0.75,
                    fillColor = pal(data_input_ordered$Num.UFO),
                    label = lapply(labels(), HTML)
        ) %>%
        addLegend(pal = pal,
                  values = data_input_ordered$Num.UFO,
                  opacity = 0.7,
                  position = "topright")
    
)
output$summary_table  = renderDataTable(data_input())

}

# Run the application 
shinyApp(ui = ui, server = server)
