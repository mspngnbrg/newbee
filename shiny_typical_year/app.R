# Explore a typical meteorological year

library(shiny)


library(dplyr)
library(ggplot2)
library('ggthemes')

# manually removed the last part with the legend from the .csv!

tmy <- read.csv("../data/raw-data/tmy_Hinnerup_56.273_9.987_2007_2016.csv", skip = 16, header = T)
# tmy <- read.csv("data/raw-data/tmy_Hinnerup_56.273_9.987_2007_2016.csv", skip = 16, header = T)

tmy$datetime <- seq.POSIXt(as.POSIXct("2013-01-01 01:00"), by = "60 min", length.out = length(tmy$time))

#T2m: 2-m air temperature (degree Celsius)
#RH: relative humidity (%)
#G(h): Global irradiance on the horizontal plane (W/m2)
#Gb(n): Beam/direct irradiance on a plane always normal to sun rays (W/m2)
#Gd(h): Diffuse irradiance on the horizontal plane (W/m2)
#IR(h): Surface infrared (thermal) irradiance on a horizontal plane (W/m2)
#WS10m: 10-m total wind speed (m/s)
#WD10m: 10-m wind direction (0 = N, 90 = E) (degree)
#SP: Surface (air) pressure (Pa)
#PVGIS (c) European Union, 2001-2021

names(tmy) <- c("time.UTC.", "temperature_celsius", "rel_humidity", "global_irridiance", "Gb.n.", "Gd.h.", "infrared", "wind_speed", "WD10m", "air_pressure")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Typical meteorological year Hinnerup"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            selectInput("response", "Choose your response variable",
                        names(tmy)[-1]) # date not clickable
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
