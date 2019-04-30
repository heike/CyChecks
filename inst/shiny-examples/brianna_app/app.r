#brianna shiny skeleton

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(leaflet)

#load the rda file
load(file = "C:/Users/lawto/Desktop/Spring2019/STAT585/final project/CyChecks/data/sals_dept.rda")

ui <- fluidPage(
  # title
  titlePanel("CyChecks"),
  # navigation bar: two selection tabs are available
  navbarPage(title = "Results Display",
             # temporal tab
             tabPanel("Temporal",
                      sidebarLayout(
                        sidebarPanel(
                          ## select time
                          #dateInput("date", "Select Date", min = newdat$Date %>% min(), max = newdat$Date %>% max(), value = newdat$Date %>% min()),
                          selectInput("year",label = ("Select Year"),
                                      choices = sort(unique(sals_dept$fiscal_year)),
                                      selected = 2018),
                          ## select response
                          radioButtons("res", "Select variable:",
                                       c("total_salary_paid" = "salary",
                                         "base_salary" = "bsalary")),
                          ## slider for breaks
                          sliderInput("b", "Break of the histogram", min = 3, max = 100, value = 10)
                        ),
                        mainPanel(
                          helpText("Here we present the histogram of the selected variable on the selected year"),
                          plotOutput("plot1")
                        )
                      )
             ),
             # spacial tab
             tabPanel("Spacial",
                      sidebarLayout(
                        ## county selection
                        sidebarPanel(
                          selectInput("place", label = "County:",
                                      choices = unique(sals_dept$place_of_residence), selected = "STORY"),
                          radioButtons("res2", "Select variable:",
                                       c("total_salary_paid" = "salary",
                                         "base_salary" = "bsalary"))
                        ),
                        ## map and responses
                        mainPanel(
                          helpText("Here we present the map of the selected city with the selected variable"),
                          leafletOutput("map")
                        )

                      )
             )

  ),
  verbatimTextOutput("summary")
)

server <- function(input, output) {

  # for plot under Temporal tab
  output$plot1 <- renderPlot({
    temp_dat <- sals_dept %>% filter(year == input$year)
    if(nrow(temp_dat) >0) {
      if(input$res == "salary") {
        hist(temp_dat$`total_salary_paid`, col ="blue", breaks=input$b,
             main = "Histogram of Salary (USD)", xlab = "Salary (USD)")
      } else {
        hist(temp_dat$`base_salary`, col ="red", breaks=input$b,
             main = "Histogram of Volume Sold (Liters)", xlab = "Volume Sold (Liters)")
      }
    }
  })

  # # for spacial dataset cleaning: store names are dirty, eg "Hyvee" and "HyVee"
  # spacialSet <- reactive({ newdat %>%
  #     filter(City == input$city) %>%
  #     group_by(`Store Number`) %>%
  #     summarise(`Sale (Dollars)` = `Sale (Dollars)` %>% sum(.,na.rm = T),
  #               `Volume Sold (Liters)` = `Volume Sold (Liters)` %>% sum(.,na.rm = T)) %>%
  #     dplyr::right_join(storeinfo) %>%
  #     filter(!is.na(`Sale (Dollars)`))
  # })
  # for plot under Spacial tab
  # output$map <-renderLeaflet({
  #   if (input$res2=="sale") {
  #     spacialSet() %>%
  #       leaflet()%>%
  #       addTiles() %>%
  #       addMarkers(lng = ~Long,
  #                  lat = ~Lat,
  #                  popup = ~paste("Store Name: ", spacialSet()$`Store Name`, "<br>",
  #                                 "Sale (Dollars): ", spacialSet()$`Sale (Dollars)`, "<br>"),
  #                  clusterOptions = leaflet::markerClusterOptions())
  #   } else{
  #     spacialSet() %>%
  #       leaflet()%>%
  #       addTiles() %>%
  #       addMarkers(lng = ~Long,
  #                  lat = ~Lat,
  #                  popup = ~paste("Store Name: ", spacialSet()$`Store Name`, "<br>",
  #                                 "Volume Sold (Dollars): ", spacialSet()$`Volume Sold (Liters)`, "<br>"),
  #                  clusterOptions = leaflet::markerClusterOptions())
  #
  #   }
  # })

}

shinyApp(ui = ui, server = server)


