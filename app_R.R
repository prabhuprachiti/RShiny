library(shiny) # Load required packages
library(plotrix)
library(RColorBrewer)
library(ggplot2)

df <- read.csv("Dataset_RShiny.csv")
df_demographics <- df[df$DAY == 0 ,] #since there are multiple entires for each patient, we don't want to count each info multiple times

# Define UI for application
if (interactive()) {
  ui <- fluidPage(
  
  # Header
    titlePanel(h4('R Shiny Demo Project', align = "center")),
  
    # Separate Panel for each of the tasks
      tabsetPanel( 

        tabPanel("Demographics", #Panel 1 -- for the demographic data
  
          #side bar panel
          sidebarPanel(
            selectInput("var", label = "1. Select the quantitative variable", #Age or BMI
                        choices = c("AGE"=3, "BMI"=4),
                        selected = 3), 
            selectInput("treatment", label = "1. Select the treatment condition", #High, Low, Placebo, Overall
                        choices = c("High", "Low", "Placebo", "Overall"),
                        selected = "Overall"), 
            sliderInput("bin", "2. Select the number of histogram BINs by using the slider below",
                        min=5, max=25, value=15),
            radioButtons("color", label = "3. Select the color of histogram",
                        choices = c("Green", "Red", "Gray"),
                        selected = "Gray")
          ),
  
          # Main Panel
          mainPanel(
                    htmlOutput("var_name"),
                    htmlOutput("treat"),
                    plotOutput("hist"),
                    plotOutput("pie")
          )
        ),
  
        tabPanel("Phenotypic Relationship",  #tab panel for task 2
                 sidebarPanel(selectInput("x_var", label = "1. Select the variable for the X-axis",
                                          choices = c("AGE"=3, "BMI"=4, "GENDER"=5, "BIOMARKER-A"=8, "BIOMARKER-B"=9),
                                          selected = 3) ),
                 sidebarPanel(selectInput("y_var", label = "2. Select the variable for the Y-axis",
                                          choices = c("AGE"=3, "BMI"=4, "GENDER"=5, "BIOMARKER-A"=8, "BIOMARKER-B"=9),
                                          selected = 4) ),
                 sidebarPanel(selectInput("facet_var", label = "3. Select the variable for the facet",
                                          choices = c("AGE"=3, "BMI"=4, "GENDER"=5, "BIOMARKER-A"=8, "BIOMARKER-B"=9),
                                          selected = 5) ),
                 mainPanel(htmlOutput("all_vars")),
                 mainPanel(plotOutput("facet", brush="plot_brush")),
                 fluidRow(column(width=5, tags$b(tags$i("Rows corresponding to datapoints under brush")),
                                 tableOutput("data_brush")))
        )
      ) 
  )

  server <- function(input, output) {
    
##################### panel-1 #######################     
    df_treatment <- reactive({ #only in a reactive environment
      req(input$treatment)
      if (input$treatment != "Overall"){
        df_treatment <- df_demographics[df_demographics$TREATMENT == input$treatment, ]      
      }
      else {
        df_treatment <- df_demographics
      }
    })
    
    output$var_name <- renderText({ 
      colm = as.numeric(input$var)
      paste("<br>Variable for histogram: <b>", names(df_treatment()[colm]))
    })
    
    output$treat <- renderText({ 
      paste("Treatment Group: <b>", input$treatment)
    })
    
    output$hist <- renderPlot({
      colm = as.numeric(input$var)
      h <- hist(df_treatment()[,colm], col =input$color, xlim = c(0, max(df_treatment()[,colm])),
            breaks = seq(0, max(df_treatment()[,colm]),l=input$bin), xlab = names(df_treatment()[colm]))
      h$counts=h$counts/sum(h$counts)
      plot(h, main=paste("Histogram of ", names(df_treatment()[colm]), "(",input$treatment,")"),
           col=input$color)
      })
    
    output$pie <- renderPlot({
      pie_table=table(df_treatment()[5]) #col 5 is gender
      pie_colors <- c("orange","green")
      labs <- paste(names(pie_table), "(", round(pie_table*100/sum(pie_table),digits=2), "%)", sep="") #sep="" is required to avoid unnecessary spaces
      pie3D(pie_table, radius=1.5, col=brewer.pal(7, "RdGy"), labels=labs, explode=0.2)
      
    })
    
##################### panel-2 #######################    
    
    output$all_vars <- renderText({ 
      x_col = as.numeric(input$x_var)
      y_col = as.numeric(input$y_var)
      facet_col = as.numeric(input$facet_var)

      paste("Plot for <b>", names(df_demographics[y_col]), "</b>v/s<b>", names(df_demographics[x_col]),
            "</b>with faceting based on<b>", names(df_demographics[facet_col]))
    })
    
    output$facet <- renderPlot({
      x_col = as.numeric(input$x_var)
      y_col = as.numeric(input$y_var)
      facet_col = as.numeric(input$facet_var)
      
      if (x_col == 8 || x_col == 9 || y_col == 8 || x_col == 9 || facet_col == 8 || facet_col == 9 ) {
        facet_table=df #include dose info iff biomarker data required
        color_scheme = facet_table$DAY
      }
      else{
        facet_table=df_demographics
        color_scheme = NULL
      }
      
      x_val=names(facet_table[x_col])
      y_val=names(facet_table[y_col])
      facet_val=names(facet_table[facet_col])
        
      p <- ggplot(facet_table, aes_string(x=x_val, y=y_val,color=color_scheme)) + geom_point() + geom_smooth(method=lm)
      p + facet_wrap(as.formula(paste("~", facet_val)))  
      
    })
    
    output$data_brush <-  renderTable({
      x_col = as.numeric(input$x_var)
      y_col = as.numeric(input$y_var)
      facet_col = as.numeric(input$facet_var)
      
      if (x_col == 8 || x_col == 9 || y_col == 8 || x_col == 9 || facet_col == 8 || facet_col == 9  ) {
        brush_table=df
      }
      else{
        brush_table=df_demographics
      }
      
      n = nrow(brushedPoints(brush_table, brush = input$plot_brush)) 
      if(n==0) # row count will be 0 when no selection is made by the brush
        return()
      else
        brushedPoints(brush_table, brush = input$plot_brush) # returns rows, allRows = TRUE can included as an argument
    })
  }
  
shinyApp(ui, server) #call the app
}
