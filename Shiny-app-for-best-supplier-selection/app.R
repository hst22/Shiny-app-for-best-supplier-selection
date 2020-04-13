library(shiny)

rank_master = read.csv("D:\\University\\Study Materials\\Spring Semester\\Grad Case Study\\WorthingtonIndustries\\Modified data\\master_rank.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Find the Best Supplier"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("width", "Width", 28, 80, 40, post = " inches"),
            radioButtons("alloy", "Alloy Category: 0 = Low C | 1 = High C",
                         choices = c(0, 1), selected = 0),
            sliderInput("top", "Top X Suppliers",1,15,5, pre = "x = "),
            sliderInput("coil_ratio", "Exclusion", 0, 0.5, 0.1, 0.05)
        ),
        mainPanel("Best Suppliers for the Specified Parameters", 
                  tableOutput("results")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    output$results = renderTable({
        
        new_table = subset(rank_master, ratio >= input$coil_ratio)   
        if (input$width >= 28 & input$width < 35 & input$alloy == 0){
            rank = as.data.frame(head(subset(new_table, width >= 28 & width < 35 & alloy_cat == 0, select = 
                                                 c(supplier, mean_width_var, sd_width_var, rms, n_coils), input$top)))
            row.names(rank) = 1:nrow(rank)
            return(rank)
        } else if (input$width >= 28 & input$width < 35 & input$alloy == 1){
            rank1= as.data.frame(head(subset(new_table, width >= 28 & width < 35 & alloy_cat == 1, select =
                                                 c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank1) = 1:nrow(rank1)
            return(rank1)
        } else if (input$width >= 35 & input$width < 50 & input$alloy == 0){
            rank2 = as.data.frame(head(subset(new_table, width >= 35 & width < 50 & alloy_cat == 0, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank2) = 1:nrow(rank2)
            return(rank2)
        } else if (input$width >= 35 & input$width < 50 & input$alloy == 1){
            rank3 = as.data.frame(head(subset(new_table, width >= 35 & width < 50 & alloy_cat == 1, select = 
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils), input$top)))
            row.names(rank3) = 1:nrow(rank3)
            return(rank3)
        } else if (input$width >= 50 & input$width < 60 & input$alloy == 0){
            rank4 = as.data.frame(head(subset(new_table, width >= 50 & width < 60 & alloy_cat == 0, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank4) = 1:nrow(rank4)
            return(rank4)
        } else if (input$width >= 50 & input$width < 60 & input$alloy == 1){
            rank5 = as.data.frame(head(subset(new_table, width >= 50 & width < 60 & alloy_cat == 1, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank5) = 1:nrow(rank5)
            return(rank5)
        } else if (input$width >= 60 & input$width < 65 & input$alloy == 0){
            rank6 = as.data.frame(head(subset(new_table, width >= 60 & width < 65 & alloy_cat == 0, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank6) = 1:nrow(rank6)
            return(rank6)
        } else if (input$width >= 60 & input$width < 65 & input$alloy == 1){
            rank7 = as.data.frame(head(subset(new_table, width >= 60 & width < 65 & alloy_cat == 1, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank7) = 1:nrow(rank7)
            return(rank7)
        } else if (input$width >= 65 & input$width < 70 & input$alloy == 0){
            rank8 = as.data.frame(head(subset(new_table, width >= 65 & width < 70 & alloy_cat == 0, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank8) = 1:nrow(rank8)
            return(rank8)
        } else if (input$width >= 65 & input$width < 70 & input$alloy == 1){
            rank9 = as.data.frame(head(subset(new_table, width >= 65 & width < 70 & alloy_cat == 1, select =
                                                  c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank9) = 1:nrow(rank9)
            return(rank9)
        } else if (input$width >= 70 & input$width < 80 & input$alloy == 0){
            rank10 = as.data.frame(head(subset(new_table, width >= 70 & width < 80 & alloy_cat == 0, select =
                                                   c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank10) = 1:nrow(rank10)
            return(rank10)
        } else if (input$width >= 70 & input$width < 80 & input$alloy == 1){
            rank11 = as.data.frame(head(subset(new_table, width >= 70 & width < 80 & alloy_cat == 1, select =
                                                   c(supplier, mean_width_var, sd_width_var, rms, n_coils)), input$top))
            row.names(rank11) = 1:nrow(rank11)
            return(rank11)
        } else if(input$width < 28 | input$width > 80){
            "Please enter a width in the range of 28 to 80!"
        } else if(input$alloy != 0 | input$alloy != 1){
            "Incorrect Alloy Category! Please select 0 for Low Carbon Steel and 1 for High Carbon Steel"
        } else {
            "Incorrect parameters entered!"
        }
    })
}


shinyApp(ui, server)
