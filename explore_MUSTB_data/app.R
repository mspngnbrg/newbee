#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library('ggthemes')

# load performance data

perf <- read.csv("../data/raw-data/Table_V_Colony_inspection.csv")
perf$day_of_year <- calculate_day_of_year(perf$sampM, perf$sampD )
names(perf)
perf$resUnit_desc

perf2 <- perf %>%
    group_by(siteNo, hiveNo, sampY, day_of_year, paramText, resUnit_desc) %>%
    summarize(min = min(resVal),
              max = max(resVal),
              median = median(resVal))

# load resource data

reso <- read.csv("../data/raw-data/Table_II_Resource_providing_unit_coord.csv")
# reso <- read.csv("data/raw-data/Table_II_Resource_providing_unit_coord.csv")
reso$day_of_year <- calculate_day_of_year(reso$sampM, reso$sampD )
names(reso)
# reso$resUnit_desc

reso2 <- reso %>%
    group_by(siteNo, sampY, day_of_year) %>%
    summarize(records = n())

#variable_of_interest <- setNames(perf$paramText)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data set exploration"),

    sidebarLayout(

        sidebarPanel(
            helpText("Top figure shows number of available floral resource records per day."),
            # Sidebar for user choices

            selectInput("response", "Colony performance: choose your response variable",
                        unique(perf$paramText)
            ),
            radioButtons("sitechoice", "Label: Choose site",
                         choices = sort(unique(perf$siteNo))
            ),
            radioButtons("plotchoice", "Choose per colony: min, max or median values? ",
                         choices = c(max = "max")
            )

        ),
        # main panel for plotting
        mainPanel(
            plotOutput("resouPlot"),
            plotOutput("perfPlot")

        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    # performance plot
    output$perfPlot <- renderPlot({

        x <- perf2 %>%
            filter(paramText == input$response, siteNo == input$sitechoice)
        plot(head(x))

        plot(ggplot(data=x, aes(x = day_of_year, y = max, col = factor(hiveNo))) +
                 facet_grid(rows = vars(sampY)) +
                 geom_point()+
                 geom_line(alpha = 0.2, lwd = 0.5)+
                 #geom_point(y = min, col = "green")
                 xlim(0, 365)+
                 #scale_x_continuous(breaks = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)-15, labels = month.abb)+
                 xlab("Day of the year")+
                 ylab(paste0( input$plotchoice, " " , unique(x$paramText), " Unit =  ", unique(x$resUnit_desc)))+
                 theme_tufte() + theme(axis.line=element_line())+
                 theme(legend.position="bottom")
        )

    })

    # floral resource plot
    output$resouPlot <- renderPlot({

        x <- reso2 %>%
            filter(siteNo == input$sitechoice)

        plot(ggplot(data=x, aes(x = day_of_year, y = records)) +
                 facet_grid(rows = vars(sampY)) +
                 geom_point()+
                 #geom_line(alpha = 0.2, lwd = 0.5)+
                 #geom_point(y = min, col = "green")
                 xlim(0, 365)+
                 #scale_x_continuous(breaks = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)-15, labels = month.abb)+
                 xlab("Day of the year")+
                 ylab("n observations / day")+
                 theme_tufte() + theme(axis.line=element_line())
        )

    })
}

# Run the application
shinyApp(ui = ui, server = server)

# rsconnect::deployApp('../explore_MUSTB_data/')
