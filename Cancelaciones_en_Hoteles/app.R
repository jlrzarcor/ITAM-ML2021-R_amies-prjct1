print(getwd())
instalar <- function(paquete) {
    
    if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
        library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    }
}

paquetes <- c("shiny","plotly","ggplot2","dplyr")
lapply(paquetes, instalar);
hotel_cr_train_dsh <- readRDS("hotel_cr_train.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("EDA Bivariate. DB Cancelaciones en hoteles"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            conditionalPanel(condition = "input.tabs == '1. Compare 2 Numeric Variables'",
                             selectInput("var1", "Variable 1", names(hotel_cr_train_dsh %>% select_if(is.numeric)))),
            conditionalPanel(condition = "input.tabs == '1. Compare 2 Numeric Variables'",
                             selectInput("var2", "Variable 2", names(hotel_cr_train_dsh %>% select_if(is.numeric)))),
            
            conditionalPanel(condition = "input.tabs == '2. Compare 2 Categorical Variables'",
                             selectInput("var3", "Variable 1", names(hotel_cr_train_dsh %>% select_if(is.factor)))),
            conditionalPanel(condition = "input.tabs == '2. Compare 2 Categorical Variables'",
                             selectInput("var4", "Variable 2", names(hotel_cr_train_dsh %>% select_if(is.factor)))),
            
            conditionalPanel(condition = "input.tabs == '3. Compare Numeric vs Categorical'",
                             selectInput("var5", "Numeric Variable", names(hotel_cr_train_dsh %>% select_if(is.numeric)))),
            conditionalPanel(condition = "input.tabs == '3. Compare Numeric vs Categorical'",
                             selectInput("var6", "Categorical Variable", names(hotel_cr_train_dsh %>% select_if(is.factor)))),
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", id="tabs",
                        tabPanel("1. Compare 2 Numeric Variables", plotlyOutput("nnPlot")),
                        tabPanel("2. Compare 2 Categorical Variables", plotlyOutput("ccPlot")),
                        tabPanel("3. Compare Numeric vs Categorical", plotlyOutput("ncPlot"))
            )
        )        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$nnPlot <- renderPlotly({
        inpvar1 <- input$var1
        inpvar2 <- input$var2
        
        p <- ggplot( data = hotel_cr_train_dsh, aes_string(x = inpvar1, y = inpvar2)) +
            geom_point(col = "darkblue", size = .7) + 
            geom_smooth(method = "loess", color = "steelblue", size = .3) 
        
        ggplotly(p)
        
    })
    
    output$ccPlot <- renderPlotly({
        inpvar3 <- input$var3
        inpvar4 <- input$var4
        
        aux1 <- hotel_cr_train_dsh %>% select(matches(inpvar3)) %>% pull
        aux2 <- hotel_cr_train_dsh %>% select(matches(inpvar4)) %>% pull 
        df_aux <- data.frame(var1 = aux1, var2 = aux2)
        
        inp_data <- df_aux %>% group_by(var1, var2) %>%
            dplyr::summarise(count = n()) %>% arrange(desc(count)) %>% ungroup()
        
        p <- ggplot( data = inp_data, aes(x = var2, y = count)) +
            geom_bar(stat = "identity", fill = "lightgray", col = "steelblue") +
            xlab('Categories') +
            ylab('Counting') + 
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(~var1, scale="free", ncol = 5)
        
        ggplotly(p)
        
    })
    
    output$ncPlot <- renderPlotly({
        inpvar5 <- input$var5
        inpvar6 <- input$var6
        
        p <- ggplot( data = hotel_cr_train_dsh, aes_string(x = inpvar6, y = inpvar5)) +
            geom_boxplot(outlier.color = "red") +
            theme(axis.text.x = element_text(angle = 90))
        
        ggplotly(p)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
