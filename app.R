#installing the needed packages
#install.packages("arules)
#install.packages("shiny")
# install.packages(dplyr)
library(shiny)
library(arules)
library(dplyr)

 
# Defining the UI of the project
ui <- fluidPage(
  #adding the title of the project
  titlePanel("Data science project...Vamoooos team 74!"),
  
  sidebarLayout(
    #defining all the inputs in the sidebarPanel
    sidebarPanel(
      fileInput("file", "Choose CSV file"),
      numericInput("clusters", "Number of clusters:", value =0),
      numericInput("support", "Enter the support value", value = 0.0),
      numericInput("confidence", "Enter the confidence value", value = 0.0),
      actionButton("proceed", "Proceed")
    ),
    
    #defining all the outputs in the mainPanel
    mainPanel(
      plotOutput("Plots"),
      #to leave a space above the text
      tags$br(),
      #to prevent the text from showing until the proceed button is pressed
      conditionalPanel(
        condition = "input.proceed > 0",
        div("K_means_clustering", style = "text-align: center; font-weight: bold; font-size: 25px;")
      ),
      dataTableOutput("clustering"),
      #to leave a space above the text
      tags$br(), 
      #to prevent the text from showing until the proceed button is pressed
      conditionalPanel(
        condition = "input.proceed > 0",
        div("Generated_association_rules", style = "text-align: center; font-weight: bold; font-size: 25px;")
      ),
      dataTableOutput("association_rules")
    )
  )
)
proceed_clicked <- reactiveVal(FALSE)

# Defining the server logic
server <- function(input, output) {
  
  observeEvent(input$proceed, {
    proceed_clicked(TRUE)
    
    # Read CSV file
    req(input$file)
    td <- read.csv(input$file$datapath)
    
    #Data cleaning phase
    #to check for dublicates
    print(sum(duplicated(td)))
    
    #to remove the duplicates from the data set
    unique_td <- unique(td)
    
    #to check that all numeric columns are integers
    print(is.integer(unique_td$count))
    print(is.integer(unique_td$total))
    print(is.integer(unique_td$rnd))
    
    #to check if there's any NA data
    print(sum(is.na(unique_td)))
    
    #to check for outliers
    outlier1 <- boxplot(unique_td$count)$out
    outlier2 <- boxplot(unique_td$total)$out
    outlier3 <- boxplot(unique_td$rnd)$out
    
    #to Remove outliers
    ds_without_outliers <- unique_td[!unique_td$count %in% outlier1,]
    
    #the final cleanded data set
    final_ds <- ds_without_outliers
    
    #Visualization phase in one dashboard
    output$Plots <- renderPlot({
      par(mfrow = c(2,2))
      #comparing cash and credit using pie chart
      x <- table(final_ds$paymentType)
      percenage <- paste0(sprintf("%.2f",100*x/sum(x)),"%")
      pie(
        x,
        labels = c(paste(c("Cash","Credit"),percenage)),
        main = "Cash VS Credit",
        col = c("hotpink","hotpink4")
      )
    
    #compare age and sum of total spendng using scatterplot
      # Calculate total spending for each age
      age_total_spending <- aggregate(total ~ age, data = final_ds, FUN = sum)
      
      plot(
        y = age_total_spending$total,
        x = age_total_spending$age,
        main = "Comparing each age and its total spending",
        xlab = "Age",
        ylab = "Total spending",
        col = "hotpink4",
        type = "l"
      )

    #comparing total spending with respect to each city descendingly
      # Calculate total spending for each city
      #aggregate function will take the city value of each row and put similar values into one column in a new data frame
      # total~city will add the total spending of each city to its column in the new data frame
      #Fun = sum => will use summation to calculate the total spending of each city
      city_total_spending <- aggregate(total ~ city, data = final_ds, FUN = sum)
      # Sort the data in descending order of total spending
      # city_total_spending <- city_total_spending[order(city_total_spending$total, decreasing = TRUE), ]
      # # Create barplot
      # barplot(
      #   height = city_total_spending$total,
      #   name = city_total_spending$city,
      #   main = "Comparing city and its spending",
      #   xlab = "City",
      #   ylab = "Total spending",
      #   col = c("aquamarine4","aquamarine4","aquamarine3","aquamarine3","aquamarine2","aquamarine2","aquamarine1","aquamarine1","aquamarine","aquamarine")
      # )

      city_total_spending <- city_total_spending[order(city_total_spending$total, decreasing = TRUE), ]
      # Create barplot
      barplot(
        height = city_total_spending$total,
        name = city_total_spending$city,
        main = "Comparing city and its spending",
        xlab = "City",
        ylab = "Total spending",
        col = c("hotpink4","hotpink4","hotpink3","hotpink3","hotpink2","hotpink2","hotpink1","hotpink1","hotpink","hotpink")
      )
      
      
    #distribution of total spending by histogram
      # hist(
      #   final_ds$total,
      #   main = "Distribution of total spending by histogram",
      #   xlab = "Total spending",
      #   ylab = "Frequency",
      #   col = "skyblue"
      # )

    #distribution of total spending by box plot
    boxplot(
      final_ds$total,
      main = "Distribution of total spending by box plot",
      col = "hotpink3"
    )
    
    })
    
    
    #K-means clustering
    #Checking for the validation of the number of clusters
    Number_of_clusterss = NULL
    if(input$clusters>=2 && input$clusters<=4){
      Number_of_clusterss = input$clusters
    }else{
      alarm("Invalid Number of Clusters!")
    }
    
    
    #calculating the sum of total spending for each customer
    customer_total_spending <- aggregate(total ~ customer, data = final_ds, FUN = sum)
    
    #initializing a data frame with the needed columns
    dataframe_with_needed_columns <- data.frame(
      customer = final_ds$customer,
      age = final_ds$age,
      city = final_ds$city,
      total = final_ds$total
    )
    
    #merging all the rows with the same customer name
    dataframe_to_be_clustered <- dataframe_with_needed_columns %>%
      group_by(customer) %>%
      summarise(across(everything(), first)) 
    
    #adding the sum of total spending row to the data frame
    dataframe_to_be_clustered$total <- customer_total_spending$total
    
    
    # Perform k-means clustering
    kmeans_clustering <- kmeans(dataframe_to_be_clustered[,c("age","total")], centers = Number_of_clusterss)
    clustered_dataset <- dataframe_to_be_clustered
    #adding the cluster results to the data frame
    clustered_dataset$Cluster <- kmeans_clustering$cluster
    

    
    #Association rules using Apriori algorithm
    #validating the support and confidence
    minimum_apriory_support = NULL
    if(input$support>=0.001 && input$support<=1){
      minimum_apriory_support = input$support
    } else{
      alarm("Invalid support value")
    }
    
    minimum_apriory_confidence = NULL
    if(input$confidence>=0.001 && input$confidence<=1){
      minimum_apriory_confidence = input$confidence
    } else{
      alarm("Invalid confidence value")
    }
    
    #converting the column to transaction
    transactions <- strsplit(final_ds$items, split = ",")
    transactions <- as(transactions, "transactions")
    
    #applying the algorithm
    rules <- apriori(transactions, parameter = list(support = minimum_apriory_support, confidence = minimum_apriory_confidence, minlen = 2))
    #converting the results to a dataframe to be able to display it
    rules_df <- as(rules, "data.frame")
    
    #printing the result of the clustering
    output$K_means_clustering <- renderText({
      my_string <- "K-means clustering table"
      return(my_string)
    })
    #printing the clustering table
    output$clustering <- renderDataTable({
      clustered_dataset
    })
    
    
    #printing the result of the apriori algorithm
    output$Generated_association_rules <- renderText({
      my_string <- "Generated association rules"
      return(my_string)
    })
    #printing the association table
    output$association_rules <- renderDataTable({
      rules_df
    })
  })
}



# executing the code
shinyApp(ui = ui, server = server)

