library(shiny)
library(shinydashboard)
library(ggplot2)
library(collapsibleTree)
library(shinycssloaders)
library(knitr)
library(DT)
library(tigris)
library(rsconnect)
deployApp()
noise <- read.csv("sound_data.csv", fileEncoding = "UTF-8-BOM")
noise <- na.omit(noise)

noise.df <- data.frame(noise)
noise.df["location"][noise.df["location"] == 1] <- "1-School"
noise.df["location"][noise.df["location"] == 2] <- "2-Inside"
noise.df["location"][noise.df["location"] == 3] <- "3-West Way"
noise.df["location"][noise.df["location"] == 4] <- "4-Main Road"

noise.df["day"][noise.df["day"] == 1] <- "1-Monday"
noise.df["day"][noise.df["day"] == 2] <- "2-Tuesday"
noise.df["day"][noise.df["day"] == 3] <- "3-Wednesday"
noise.df["day"][noise.df["day"] == 4] <- "4-Thursday"
noise.df["day"][noise.df["day"] == 5] <- "5-Friday"
noise.df["day"][noise.df["day"] == 6] <- "6-Saturday"
noise.df["day"][noise.df["day"] == 7] <- "7-Sunday"

noise.df["weekday"][noise.df["weekday"] == "Y"] <- "Weekday"
noise.df["weekday"][noise.df["weekday"] == "N"] <- "Weekend"

noise.df["time_range"][noise.df["time_range"] == 1] <- "1-Morning"
noise.df["time_range"][noise.df["time_range"] == 2] <- "2-Noon"
noise.df["time_range"][noise.df["time_range"] == 3] <- "3-Evening"

noise.df$date <- as.Date.character(noise.df$date, tryFormats = c("%d/%m/%Y","%Y-%m-%d") )
noise.df$location <- as.factor(noise.df$location)
noise.df$date <- as.factor(noise.df$date)
noise.df$day <- as.factor(noise.df$day)
noise.df$weekday <- as.factor(noise.df$weekday)
noise.df$time_range <- as.factor(noise.df$time_range)
noise.df$dB <- as.character(noise.df$dB)

rmarkdown::render("home.rmd")
rmarkdown::render("botley.rmd")

header <- dashboardHeader(title = "Noise Pollution in Botley, Oxford",
                          titleWidth = 350)
sidebar <- dashboardSidebar(
  tags$head(tags$style(
    HTML('.shiny-server-account { display: none; }')
  )),
  sidebarMenuOutput("Semi_collapsible_sidebar"),
  width = 350,
  sidebarMenu(
    # id = "tabs",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem(
      "Botley Map",
      tabName = "map",
      icon = icon("thumbtack")
    ),
    menuItem("Noise Data", tabName = "table", icon = icon("table")),
    menuItem(
      "Noise Charts",
      tabName = "charts",
      icon = icon("chart-line")
    ),
    menuItem(
      "Compare Area",
      tabName = "compare",
      icon = icon("compass")
    ),
    menuItem(
      "Graph Info",
      tabName = "info",
      icon = icon("info"),
      tags$p(
        strong(em(" 1- Bar Chart :")),
        em(" This graph presents categorical data with"),
        style = "font-size: 10pt"
      ),
      tags$p(
        em("rectangular bars with heights or lengths proportional to"),
        style = "font-size: 10pt"
      ),
      tags$p(em("the values that they represent."), style = "font-size: 10pt"),
      tags$p(strong(em(" 2- Boxplot :")), em(" A boxplot is a graph that gives you a good"), style = "font-size: 10pt"),
      tags$p(
        em("indication of how the values in the data are spread out."),
        style = "font-size: 10pt"
      ),
      tags$p(em("Outliers are shown as black dots."), style = "font-size: 10pt"),
      tags$p(strong(em(" 3- Histogram :")), em(" In a histogram, bars represent ranges "), style = "font-size: 10pt"),
      tags$p(
        em("instead of individual values. Each bar represents a range "),
        style = "font-size: 10pt"
      ),
      tags$p(
        em("of data points, and the height of the bar tells us how many "),
        style = "font-size: 10pt"
      ),
      tags$p(em("data points are in that range."), style = "font-size: 10pt"),
      tags$p(
        strong(em(" 4- Density :")),
        em(" This chart is a variation of a Histogram that"),
        style = "font-size: 10pt"
      ),
      tags$p(
        em("allows for smoother distributions by smoothing out the noise."),
        style = "font-size: 10pt"
      )
      
    )
  )
)
body <-   dashboardBody(
  # color as the rest of the header.
  tags$head(
    tags$link(rel =  "stylesheet", type = "text/css", href = "font.css")
  ),
  tabItems(
    tabItem(
      tabName = "home" ,
      class = 'active',
      role = "figure",
      includeHTML("home.html")
    ),
    tabItem(
      tabName = "map",
      class = 'active',
      role = "figure",
      includeHTML("botley.html")
    ),
    tabItem(
      tabName = "table",
      dataTableOutput("noise") %>% withSpinner(color = "green")
    ),
    tabItem(tabName = "charts",
            fluidRow(
              box(
                width = 15,
                box(width = 4,
                    selectInput(
                      "group", "Select Group", choices = names(noise.df[, -c(6, 7)])
                    )),
                box(width = 4,
                    selectInput(
                      "variable", "Select Variable", list(dB = "dB")
                    )),
                solidHeader = T,
                collapsible = TRUE,
                title = "Select Options"
              ),
              box(
                width = 15,
                uiOutput("plot_bar", height = 350),
                solidHeader = T,
                collapsible = T,
                collapsed = TRUE,
                title = "Bar Chart"
              ),
              box(
                width = 15,
                uiOutput("plot_boxplot", height = 350),
                solidHeader = T,
                collapsible = T,
                collapsed = TRUE,
                title = "Box Plot"
              ),
              box(
                width = 15,
                uiOutput("plot_histogram", height = 350),
                solidHeader = T,
                collapsible = T,
                collapsed = TRUE,
                title = "Histogram"
              ),
              box(
                width = 15,
                uiOutput("plot_density", height = 350),
                solidHeader = T,
                collapsible = T,
                collapsed = TRUE,
                title = "Density"
              )
            )),
    tabItem(tabName = "compare",
            fluidRow(
              box(
                width = 15,
                box(width = 3,
                    selectInput(
                      "grp", "Select Group", choices = names(noise.df[, -c(1, 6, 7)])
                    )),
                box(width = 3,
                    selectInput("var", "Select Variable", list(dB = "dB"))),
                box(width = 3,
                    selectInput(
                      "plot.type",
                      "Plot Type",
                      list(
                        boxplot = "boxplot",
                        histogram = "histogram",
                        density = "density",
                        bar = "bar"
                      )
                    )),
                box(
                  width = 6,
                  radioButtons(
                    inputId = "area1",
                    label = "Select 1st Area ",
                    choices = c(
                      "School" = "1-School",
                      "Inside" = "2-Inside",
                      "West Way" = "3-West Way",
                      "Main Road" = "4-Main Road"
                    )
                  )
                ),
                box(
                  width = 6,
                  radioButtons(
                    inputId = "area2",
                    label = "Select 2nd Area ",
                    choices = c(
                      "School" = "1-School",
                      "Inside" = "2-Inside",
                      "West Way" = "3-West Way",
                      "Main Road" = "4-Main Road"
                    )
                  )
                )
                ,
                solidHeader = T,
                collapsible = TRUE,
                title = "Select Options"
              )
            ),
            fluidRow(
              box(width = 6, uiOutput("plot1", height = 350)),
              box(width = 6, uiOutput("plot2", height = 350))
            ))
  ))
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {

  output$noise <- renderDataTable(
    noise.df,
    filter = "top",
    colnames = c(
      'Location',
      'Date',
      'Day',
      'Weekday',
      'Time Range',
      'Time',
      'dB'
    )
  )
  
  
  observe({
    if (is.null(input$dataset))
      return()
    obj <- switch(input$dataset,
                  "noise.df" = noise)
    var.opts <- colnames(obj[, -c(1, 2, 3, 4, 5, 6)])
    grp.opts <- colnames(obj[, -c(2, 6, 7)])
    updateSelectInput(session, "variable", list(dB = "dB"))
    updateSelectInput(session, "group", choices = var.opts)
    
  })
  
  output$caption <- renderText({
    switch(
      input$plot.type,
      "bar" 	  	=	"Bar graph",
      "boxplot" 	= "Boxplot",
      "histogram" =	"Histogram",
      "density" 	=	"Density plot"
    )
  })
  
  output$plot_boxplot <- renderUI({
    plotOutput("boxplot")
  })
  
  output$plot_histogram <- renderUI({
    plotOutput("histogram")
  })
  
  output$plot_density <- renderUI({
    plotOutput("density")
  })
  
  output$plot_bar <- renderUI({
    plotOutput("bar")
  })
  
  output$plot1 <- renderUI({
    plotOutput("p1")
  })
  
  output$plot2 <- renderUI({
    plotOutput("p2")
  })
  ###Boxplot
  
  plotrange = reactive({
    Num1 = as.numeric(strsplit(input$variable, ",")[[1]][10])
    Num2 = as.numeric(strsplit(input$variable, ",")[[1]][10])
    range = c(Num1, Num2)
    return(range)
  })
  output$boxplot <- renderPlot({
    variable <- noise.df[[input$variable]]
    group <- noise.df[[input$group]]
    if (is.null(variable) || is.null(group))
      return(NULL)
    
    plot.obj <<- list()
    plot.obj$data <<- noise.df
    plot.obj$variable <<- with(plot.obj$data, get(input$variable))
    plot.obj$group <<- with(plot.obj$data, get(input$group))
    
    require(ggplot2)
    #plotting theme
    .theme <- theme(
      axis.line = element_line(colour = 'gray', size = 1),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
    boxplot <- ggplot(
      plot.obj$data,
      aes(
        x 		= plot.obj$group,
        y 		= plot.obj$variable,
        fill 	= as.factor(plot.obj$group),
        group 	= as.factor(plot.obj$group),
      )
    ) + geom_boxplot() +
      coord_cartesian(ylim = plotrange())
    
    boxplot <- boxplot + labs(fill 	= input$group,
                              x 		= "",
                              y 		= input$variable) +
      .theme
    
    print(boxplot)
  })
  
  ###Histogram
  output$histogram <- renderPlot({
    variable <- noise.df[[input$variable]]
    group <- noise.df[[input$group]]
    if (is.null(variable) || is.null(group))
      return(NULL)
    
    plot.obj <<- list() # not sure why input$X can not be used directly?
    plot.obj$data <<- noise.df
    plot.obj$variable <<- with(plot.obj$data, get(input$variable))
    plot.obj$group <<- with(plot.obj$data, get(input$group))
    
    .theme <- theme(
      axis.line = element_line(colour = 'gray', size = 1),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
    histogram <- ggplot(plot.obj$data,
                        aes(x 		= plot.obj$variable,
                            fill 	= as.factor(plot.obj$group),)) + geom_histogram(stat = "count",
                                                                                  alpha = 0.5,
                                                                                  position = "identity")
    
    histogram <- histogram + labs(fill 	= input$group,
                                  x 		= input$variable,
                                  y 		= "Count")  +
      .theme
    print(histogram)
  })
  
  ###Density
  output$density <- renderPlot({
    variable <- noise.df[[input$variable]]
    group <- noise.df[[input$group]]
    if (is.null(variable) || is.null(group))
      return(NULL)
    
    plot.obj <<- list()
    plot.obj$data <<- noise.df
    plot.obj$variable <<- with(plot.obj$data, get(input$variable))
    plot.obj$group <<- with(plot.obj$data, get(input$group))
    
    .theme <- theme(
      axis.line = element_line(colour = 'gray', size = 1),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
    density <- ggplot(plot.obj$data,
                      aes(
                        x 		= plot.obj$variable,
                        fill 	= as.factor(plot.obj$group),
                        group 	= as.factor(plot.obj$group),
                      )) + geom_density(alpha = 0.3)
    
    density <- density + labs(fill 	= input$group,
                              x 		= input$variable,
                              y 		= "Density")  +
      .theme
    print(density)
  })
  
  ###Bar
  output$bar <- renderPlot({
    variable <- noise.df[[input$variable]]
    group <- noise.df[[input$group]]
    if (is.null(variable) || is.null(group))
      return(NULL)
    
    plot.obj <<- list()
    plot.obj$data <<- noise.df
    plot.obj$variable <<- with(plot.obj$data, get(input$variable))
    plot.obj$group <<- with(plot.obj$data, get(input$group))
    
    .theme <- theme(
      axis.line = element_line(colour = 'grey', size = 1),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
    bar <- ggplot(
      plot.obj$data,
      aes(
        x 		= plot.obj$variable,
        # y     = ,
        fill 	= as.factor(plot.obj$group),
        group 	= as.factor(plot.obj$group),
      )
    )  + geom_bar(position = "dodge")
    
    bar <- bar + labs(fill 	= input$group,
                      x 		= input$variable,
                      y 		= "Count" )  +
       .theme
    print(bar)
  })
  
  ####Area Comparison
  ###Area 1
  output$p1 <- renderPlot({
    var <- noise.df[[input$var]]
    grp <- noise.df[[input$grp]]
    if (is.null(var) || is.null(grp))
      return(NULL)
    
    plot.a1 <<- list() # not sure why input$X can not be used directly?
    plot.a1$data <<- subset(noise.df, location %in% input$area1)
    plot.a1$var <<- with(plot.a1$data, get(input$var))
    plot.a1$grp <<- with(plot.a1$data, get(input$grp))
    
    #dynamic plotting options
    plot.type <- switch(
      input$plot.type,
      "boxplot" 	= geom_boxplot(),
      "histogram" =	geom_histogram(
        stat = "count",
        alpha = 0.5,
        position = "identity"
      ),
      "density" 	=	geom_density(alpha = 0.5),
      "bar" 	   	=	geom_bar(position = "dodge")
    )
    
    require(ggplot2)
    #plotting theme
    .theme <- theme(
      axis.line = element_line(colour = 'gray', size = 1),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text.x=element_text(angle=90,hjust=1)
    )
    if (input$plot.type == "boxplot")	{
      #control for 1D or 2D graphs
      p1 <- ggplot(
        plot.a1$data,
        aes(
          x 		= plot.a1$grp,
          y 		= plot.a1$var,
          fill 	= as.factor(plot.a1$grp),
          group = as.factor(plot.a1$grp)
        )
      ) + plot.type 
      
      p1 <- p1 + labs(fill 	= input$grp,
                      x 		= "",
                      y 		= "dB" )  +
        .theme      
      
    } else {
      
      p1 <- ggplot(plot.a1$data,
                   aes(
                     x 		= plot.a1$var,
                     fill 	= as.factor(plot.a1$grp),
                     group 	= as.factor(plot.a1$grp),
                   )) + plot.type
      p1 <- p1 + labs(fill 	= input$grp,
                      x 		= "dB",
                      y 		= "Count" )  +
        .theme      
    }
    
    print(p1)
  })
  
  # ###Area 2
  output$p2 <- renderPlot({
    var <- noise.df[[input$var]]
    grp <- noise.df[[input$grp]]
    if (is.null(var) || is.null(grp))
      return(NULL)
    
    plot.a2 <<- list() # not sure why input$X can not be used directly?
    plot.a2$data <<- subset(noise.df, location %in% input$area2)
    plot.a2$var <<- with(plot.a2$data, get(input$var))
    plot.a2$grp <<- with(plot.a2$data, get(input$grp))
    
    #dynamic plotting options
    plot.type <- switch(
      input$plot.type,
      "boxplot" 	= geom_boxplot(),
      "histogram" =	geom_histogram(
        stat = "count",
        alpha = 0.5,
        position = "identity"
      ),
      "density" 	=	geom_density(alpha = 0.5),
      "bar" 	   	=	geom_bar(position = "dodge")
    )
    
    require(ggplot2)
    #plotting theme
    .theme <- theme(
      axis.line = element_line(colour = 'gray', size = 1),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text.x=element_text(angle=90,hjust=1)
    )
    if (input$plot.type == "boxplot")	{
      #control for 1D or 2D graphs
      p2 <- ggplot(
        plot.a2$data,
        aes(
          x 		 = plot.a2$grp,
          y 	 	 = plot.a2$var,
          fill  = as.factor(plot.a2$grp),
          group = as.factor(plot.a2$grp)
        )
      ) + plot.type
      
      p2 <- p2 + labs(fill 	= input$grp,
                      x 		= "",
                      y 		= "dB" )  +
        .theme
      
    } else {
      p2 <- ggplot(plot.a2$data,
                   aes(
                     x 		= plot.a2$var,
                     fill 	= as.factor(plot.a2$grp),
                     group 	= as.factor(plot.a2$grp),
                   )) + plot.type
      p2 <- p2 + labs(fill 	= input$grp,
                      x 		= "dB",
                      y 		= "Count" )  +
        .theme      
    }
    
    print(p2)
  })
}

shinyApp(ui, server = server)