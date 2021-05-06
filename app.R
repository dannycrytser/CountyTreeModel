# ----LOADING LIBRARIES  ----

library(shiny)
library(tidyverse)
library(rsample)

library(RCurl)

library(maps)
library(mapproj)
library(rpart)
library(rpart.utils)
library(rpart.plot)



# ---- USER INTERFACE ----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How to predict an election with decision trees"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("cp",
                        "Complexity parameter (higher = more complex)",
                        min = 1,
                        max = 100,
                        value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tree_plot")
        )
    )
)


# ---- SERVER LOGIC ----



# tree_pred <- predict(tree_majority, test, type = "class")
# 
# table(tree_pred, test$majority)

# the test accuracy is 0.8843683, barely less than the training accuracy

# As an aside 

# Define server logic required to draw a histogram
server <- function(input, output) {
    main_df <- read_csv("main_df.csv")
    
    split_obj <- initial_split(main_df, prop  = 0.8)
    
    

    # make a training set
    train_df <- training(split_obj)
    
    
    # make a test set
    test_df  <- testing(split_obj)
    
    # generate image 
    
    output$tree_plot <- renderPlot({
            inv_cp <- 1/input$cp
            tree_majority <- rpart(majority~.-fips,
                                   method = "class", 
                                   data = na.omit(train_df),
                                   cp = inv_cp)
            tree_plot <- prp(tree_majority, 
                uniform=TRUE,
                main="Which way does a county vote?", 
                Margin = 0.05,
                border.col = "black",
                split.cex = 0.7, 
                extra = 0)
            tree_plot
            
    })
}

# ---- RUN APP ---- 

# Run the application 
shinyApp(ui = ui, server = server)
