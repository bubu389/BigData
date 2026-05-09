#Imported Libraries to compile the shiny Script logic
library(shiny) #Package used for creating interactive web applications and dashboards.
library(openxlsx) #Used for reading, and editing columns from Excel file
library(ggplot2) #provides a powerful techniques for dynamic plots;scatter and bar charts
library(GGally) #Extension of ggplot2 library for data analysis
library(dplyr) #Used for manipulation and data filtering
library(magrittr) #Allows concise code syntax
library(plotly) #create charts with user interactivity
library(forcats) #used to manipulate factors
library(patchwork) #Used to combine 2 plots together into 1 plot
library(gridExtra) #Provides function to arrange and sort plots

# Defining User Interface for Coursework1 app to draw plots
ui <- fluidPage(
  # Application title
  #The functions tags$head() and tags$style() are used to produce HTML tags for the tile page.
  tags$head(
    #CSS Styling to alter the Dashboard title
    tags$style(HTML("
      .title-panel {
        text-align: center;
        background-color: #add8e6;
        color: #333333;
        padding: 10px;
        margin-bottom: 20px;
      }
    "))
  ),
 

  # Sidebar with a toggle input for churn
  sidebarLayout(
   
    #Functionality in the side panel
    sidebarPanel(
      # Decrease to the minimal width of the sidebar panel
      width = 1,
      # 2 Radio buttons to filter visuals on screen
      radioButtons("toggleChurn", "Visual Filter", choices = c("Active and Churn Cohort","Churn Only Cohort"), selected = "Active and Churn Cohort")
    ),
   
    #Mainpanel to show visual plots on the screen
    mainPanel(
     
      #Generating the Title for the dashoard
      tags$div(class = "title-panel", titlePanel("Coursework 1 - Internet Churn Analysis")),
       
        #Defining rows in the layout 1 row 2 columns.
        #The columns have equal width of 6 as the maximum width is 12
        #PlotlyOutput is used to produce interactivity in churnPlot1 and ChurnPlot2
        fluidRow(
          #Showing ChurnPlot1 and ChurnPlot2 in the first Row
          column(width = 6, plotlyOutput("churnPlot1")),
          column(width = 6, plotlyOutput("churnPlot2"))
        ),
       
        #Defining rows in the layout 1 row 2 columns.
        #The columns have equal width of 6 as the maximum width is 12
        #PlotlyOutput is used to produce interactivity in churnPlot3 and ChurnPlot4  
        fluidRow(
          #Showing ChurnPlot3 and ChurnPlot4 in the Second Row
          column(width = 6, plotlyOutput("churnPlot3")),
          column(width = 6, plotlyOutput("churnPlot4"))
        ),
       
        #Defining rows in the layout 1 row 2 columns.
        #The columns have equal width of 6 as the maximum width is 12
        #PlotlyOutput is used to produce interactivity in churnPlot5 and ChurnPlot6  
        fluidRow(
          #Showing ChurnPlot5 and ChurnPlot6 in the Third Row
          column(width = 6, plotlyOutput("churnPlot5")),
          column(width = 6, plotlyOutput("churnPlot6"))
        ),
     
        #Defining rows in the layout 1 row 2 columns.
        #The columns have equal width of 6 as the maximum width is 12
        #PlotlyOutput is used to produce interactivity in churnPlot7 and ChurnPlot8
        fluidRow(
          #Showing ChurnPlot7 and ChurnPlot8 in the Forth Row
          column(width = 6, plotlyOutput("churnPlot7")),
          column(width = 6, plotlyOutput("churnPlot8"))
        ),
     
        #Defining rows in the layout 1 row 1 columns.
        #The column takes the maximum width is 12
        #PlotlyOutput is used to produce interactivity in churnPlot9
        fluidRow(
          #Showing ChurnPlot9 in the Fifth Row
          column(width = 12, plotlyOutput("churnPlot9")),
        ),
     
        #Defining rows in the layout 1 row 1 columns.
        #The column takes the maximum width is 12
        #PlotlyOutput is used to produce interactivity in churnPlot10
        fluidRow(
          #Showing ChurnPlot10 in the Fifth Row  
          column(width = 12, plotlyOutput("churnPlot10")),
        )
      )
    )
  )

#Localhost Server functionality to create visual Plots
server <- function(input, output) {
  #Reading the DS redcords from Excel File telcoRds
  telcoRds <- openxlsx::read.xlsx("../Data/telcoRds_1.xlsx")
 
  #Changing the Column Attribute Names
  colnames(telcoRds) <- c("Subscriber_CustomerID","Subscriber_Gender","Subscriber_Senior_Citizen","Subscriber_Partner","Subscriber_Dependents","Subscriber_TenureInMonths","Subscriber_PhoneService","Subscriber_MultipleLines","Subscriber_Internet_Service","Subscriber_Online_Security","Subscriber_Online_Backup","Subscriber_Device_Protection","Subscriber_Technical_Support","Subscriber_Streaming_TV_Online","Subscriber_Streaming_Movies_Online","Subscriber_Contract_Type","Subscriber_Paperless_Billing","Subscriber_Payment_Method_Type","Subscriber_Monthly_AccessFee","Subscriber_Total_Charges","churn","ChurnValue", "Subscriber_Churn_Score_Value", "CLV", "Subscriber_DisconnectionReason")
 
  # after reading + renaming:
  bad <- which(is.na(names(telcoRds)) | names(telcoRds) == "")
  bad
  names(telcoRds)[bad]
 
  # Force valid, non-empty, unique column names
  nm <- names(telcoRds)
 
  # replace NA/"" with generated names
  nm[is.na(nm) | nm == ""] <- paste0("X", which(is.na(nm) | nm == ""))
 
  # ensure uniqueness (in case duplicates exist)
  names(telcoRds) <- make.unique(nm)
 
 
  #Converting the dataset object to a data frame using the as.data.frame() function.
  telcoRds <- as.data.frame(telcoRds)
 
  #Modifiying specific columns of the dataset
  telcoRds <- telcoRds %>%
   
    #If the value is "No phone service", it is changed to "No".
    #If the value is "No", it is kept as "No".
    #If the value is "Yes", it is changed to "Yes".
    mutate_at(7, ~as.factor(case_when(. == "No phone service" ~ "No", . == "No" ~ "No", . == "Yes" ~ "Yes"))) %>%
   
   
    #The values "No internet service", "No", and "Yes" are mapped to "No", "No", and "Yes" respectively.
    mutate_at(c(9:14), ~as.factor(case_when(. == "No internet service" ~ "No", . == "No" ~ "No", . == "Yes" ~ "Yes")))
 
  #Creating the plots
  output$churnPlot1 <- renderPlotly({
   
    #The variable filteredData is instantiated and assigned the value of telcoRds. A new data frame is generated for the purpose of visualisation.
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    #Plotting the Visual based on the axis and values of Churn Value on Subscriber_TenureInMonths
    #Giving a Title of the Plot and colors to the bar graph
    g1 <- filteredData %>% sample_n(1000) %>% ggplot(aes(x = churn, y = Subscriber_TenureInMonths, fill = churn)) +
     
      #Function to create bar chart and bars will be plotted to their absolute values on y-axis
      geom_col(position = "identity") +
     
      #Adding text annotations to the plot x-axis and applying vertical adjustment with vjust
      annotate("text", x = "No", y = mean(filteredData$Subscriber_TenureInMonths[filteredData$churn == "No"]), label = paste("Avg:", round(mean(filteredData$Subscriber_TenureInMonths[filteredData$churn == "No"]), 0), "months"), vjust = -0.5) +
      annotate("text", x = "Yes", y = mean(filteredData$Subscriber_TenureInMonths[filteredData$churn == "Yes"]), label = paste("Avg:", round(mean(filteredData$Subscriber_TenureInMonths[filteredData$churn == "Yes"]), 0), "months"), vjust = -0.5) +
     
      #Setting the Plot Title
      labs(title = "Average Subscriber_TenureInMonths") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #Creating clean and user friendly plots by removing background grid lines
      #Simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g1)
  })
 
  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_Monthly_AccessFee
  #Giving a Title of the Plot and colors to the bar graph
  output$churnPlot2 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    #Plotting the data with Monthly Access Fee
    g2 <- filteredData %>% sample_n(1000) %>% ggplot(aes(x = churn, y = Subscriber_Monthly_AccessFee, fill = churn)) +
     
      #Function to create bar chart and bars will be plotted to their absolute values on y-axi
      geom_col(position = "identity") +
     
      #adding text annotations to the plot x-axis
      annotate("text", x = "No", y = mean(filteredData$Subscriber_Monthly_AccessFee[filteredData$churn == "No"]), label = paste("Avg:", round(mean(filteredData$Subscriber_Monthly_AccessFee[filteredData$churn == "No"]), 0), "months"), vjust = -0.5) +
      annotate("text", x = "Yes", y = mean(filteredData$Subscriber_Monthly_AccessFee[filteredData$churn == "Yes"]), label = paste("Avg:", round(mean(filteredData$Subscriber_Monthly_AccessFee[filteredData$churn == "Yes"]), 0), "months"), vjust = -0.5) +
     
      #Setting the Plot Title
      labs(title = "Average Monthly Charges") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #creating clean and user friendly plots by removing background grid lines
      #Simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g2)
  })

  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_Contract_Type
  #Giving a Title of the Plot and colors to the stacked bar graph
  output$churnPlot3 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    #Plotting the stacked bar chart with Contract Type
    g3 <- filteredData %>% sample_n(1000) %>% ggplot(aes(x=fct_rev(Subscriber_Contract_Type), fill=fct_rev(churn)))+  geom_bar(alpha=1) + labs(title="Customer Churn by Subscriber Contract Type", y="Subscriber Contract Type") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue"))+
     
      #Creating clean and user friendly plots by removing background grid lines
      #Simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g3)
  })
 
  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_Gender
  #Giving a Title of the Plot and colors to the stacked bar graph
  output$churnPlot4 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    g4 <- filteredData %>%
     
      # Convert Subscriber_Gender column to factor
      mutate(Subscriber_Gender = as.factor(Subscriber_Gender)) %>%  
     
      #Plotting the Stacked Bar Chart Plot with subscriber_Gender and Churn dimension
      ggplot(aes(x = Subscriber_Gender, fill = fct_rev(churn))) +
     
      #Setting the color with no transparency
      geom_bar(alpha = 1) +
     
      #Setting the Plot Title and fill up the Y-Axis
      labs(title = "Customer Churn on Subscriber Gender", y = "Subscriber Gender") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #Creating clean and user friendly plots by removing background grid lines
      #Simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g4)
  })
 
  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_Dependents
  #Giving a Title of the Plot and colors to the stacked bar graph
  output$churnPlot5 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    g5 <- filteredData %>%
      # Convert Subscriber_Dependents column to factor
      mutate(Subscriber_Dependents = as.factor(Subscriber_Dependents)) %>%  
     
      #Plotting the Stacked Bar Chart Plot with subscriber_Dependents and Churn dimension
      ggplot(aes(x = Subscriber_Dependents, fill = fct_rev(churn))) +
     
      #Setting the color with no transparency
      geom_bar(alpha = 1) +
     
      #Setting the Plot Title and fill up the Y-Axis
      labs(title = "Customer Churn on Subscriber Dependents", y = "Subscriber Dependents") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #creating clean and user friendly plots by removing background grid lines
      #simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g5)
  })
 
  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_Partner Dependency
  #Giving a Title of the Plot and colors to the stacked bar graph
  output$churnPlot6 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    g6 <- filteredData %>%
     
      #Convert Subscriber_Partner column to factor
      mutate(Subscriber_Partner = as.factor(Subscriber_Partner)) %>%
     
      #Plotting the Stacked Bar Chart Plot with Subscriber_Partner and Churn dimension
      ggplot(aes(x = Subscriber_Partner, fill = fct_rev(churn))) +
     
      #Setting the color with no transparency
      geom_bar(alpha = 1) +
     
      #Setting the Plot Title and fill up the Y-Axis
      labs(title = "Customer Churn on Subscriber Partner", y = "Subscriber Partner") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #creating clean and user friendly plots by removing background grid lines
      #simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g6)
  })
 
  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_Senior_Citizen
  #Giving a Title of the Plot and colors to the stacked bar graph
  output$churnPlot7 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    g7 <- filteredData %>%
     
      #Convert Subscriber_Senior_Citizen column to factor
      mutate(Subscriber_Senior_Citizen = as.factor(Subscriber_Senior_Citizen)) %>%  
     
      #Plotting the Stacked Bar Chart Plot with Subscriber_Senior_Citizen and Churn dimension
      ggplot(aes(x = Subscriber_Senior_Citizen, fill = fct_rev(churn))) +
     
      #Setting the color with no transparency
      geom_bar(alpha = 1) +
     
      #Setting the Plot Title and fill up the Y-Axis
      labs(title = "Customer Churn on Subscriber Senior Citizen", y = "Senior Citizen") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #creating clean and user friendly plots by removing background grid lines
      #simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g7)
  })

  #Plotting the Visual based on the axis and values of Churn Value on Customer lifetime value
  #Giving a Title of the Plot and colors to the stacked bar graph
  output$churnPlot8 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    #Plotting the Bar Chart Plot with Subscriber_CLV and Churn dimension
    g8 <- filteredData %>% sample_n(1000) %>% ggplot(aes(x = churn, y = CLV, fill = churn)) +
     
      #Function to create bar chart and bars will be plotted to their absolute values on y-axis
      geom_col(position = "identity") +
     
      #adding text annotations to the plot x-axis
      annotate("text", x = "No", y = mean(filteredData$CLV[filteredData$churn == "No"]), label = paste("Avg:", round(mean(filteredData$CLV[filteredData$churn == "No"]), 0), "Euros"), vjust = -0.5) +
      annotate("text", x = "Yes", y = mean(filteredData$CLV[filteredData$churn == "Yes"]), label = paste("Avg:", round(mean(filteredData$CLV[filteredData$churn == "Yes"]), 0), "Euros"), vjust = -0.5) +
     
      #Setting the Plot Title
      labs(title = "Average Customer Lifetime Value") +
     
      #Setting the Plot Bar Colors to
      #Lightgray for Churn Only Cohort
      #Lightblue for Active and Churn Cohort
      scale_fill_manual(values = c("lightgray", "lightblue")) +
     
      #creating clean and user friendly plots by removing background grid lines
      #simplifying axis from the plot to avoid cluttering
      theme_minimal() +
     
      #Setting up Plot appearance elements
      #Formatting the Plot Title text to Bold and size12
      theme(plot.title = element_text(face = "bold", size = 12),
            #Formatting the Plot Visual Axis Title to size10      
            axis.title = element_text(size = 10),
            #Formatting the Plot Visual Axis Text to size10
            axis.text = element_text(size = 10))
   
    #Rendering the Plot Object to an Interactive Plot Object
    ggplotly(g8)
  })

  #Plotting the Visual based on the axis and values of Churn Value based on multiple dimensions; Subscriber_TenureInMonths, CLV and monthtlyCharges
  #Giving a Title of the Plot and colors to the stacked bar graph
  #The task involves the creation of three plots pertaining to the analysis of
  #customer Subscriber_TenureInMonths, specifically scatter plots and a histogram.
  #These plots are then consolidated into a unified output through the utilisation of
  #subplotting techniques. The plots offer valuable insights into the correlation between
  #Subscriber_TenureInMonths, customer lifetime value (CLV), monthly charges, and churn.
 
  output$churnPlot9 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    #Affecting Type Casting to numeric
    filteredData$Subscriber_TenureInMonths <- as.numeric(as.character(filteredData$Subscriber_TenureInMonths))
    filteredData$CLV <- as.numeric(as.character(filteredData$CLV))
    filteredData$Subscriber_Monthly_AccessFee <- as.numeric(as.character(filteredData$Subscriber_Monthly_AccessFee))
   
    #Plot 1: Scatterplot of Subscriber_TenureInMonths vs. CLV
    #A scatter plot of Subscriber_TenureInMonths (x-axis) vs. CLV (y-axis) with markers colored by churn.
    #The layout function set titles, axis labels, and other visual properties.
    #showLegend is set to False not to show on Screen
    p1 <- filteredData %>%
      sample_n(1000) %>%
      #Plotting the Scatter Plot with Subscriber_TenureInMonths and CLV dimension
      plot_ly(x = ~Subscriber_TenureInMonths, y = ~CLV) %>%
     
      #Assigning the color to the Scattered plot
      add_markers(color = ~churn, colors = c("lightgray", "lightblue"), alpha = 0.6) %>%
     
      layout(title = "Subscriber Tenure in Months vs. Customer Lifetime Value", #Plot Title
             xaxis = list(title = "Subscriber Tenure in Months"), #X-Axis Title
             yaxis = list(title = "Customer Lifetime Value"), #Y-Axis Title
             showlegend = FALSE) #ShowLegend set to False
   
    #Plot 2: Scatterplot of Subscriber_TenureInMonths vs. Subscriber_Monthly_AccessFee
    #A scatter plot of Subscriber_TenureInMonths (x-axis) vs. Subscriber_Monthly_AccessFee (y-axis) with markers
    #The layout function set titles, axis labels, and other visual properties.
    #showLegend is set to False not to show on Screen
    p2 <- filteredData %>%
      sample_n(1000) %>%
      #Plotting the Scatter Plot with Subscriber_TenureInMonths and Subscriber_Monthly_AccessFee dimension
      plot_ly(x = ~Subscriber_TenureInMonths, y = ~Subscriber_Monthly_AccessFee) %>%
     
      #Assigning the color to the Scattered plot
      add_markers(color = ~churn, colors = c("lightgray", "lightblue"), alpha = 0.6) %>%
      layout(title = "Subscriber Tenure in Months vs. Monthly Charges", #Plot Title
             xaxis = list(title = "Subscriber Tenure in Months"),#X-Axis Title
             yaxis = list(title = "Monthly Charges"), #Y-Axis Title
             showlegend = FALSE) #ShowLegend set to False
   
    #Plot 3: Histogram of Subscriber_TenureInMonths
    #A histogram of Subscriber_TenureInMonths with bars colored by churn.
    #The layout function set titles, axis labels, and other visual properties.
    #showLegend is set to False not to show on Screen
    #Rendering the Plot Object to an Interactive Plot Object
    p3 <- filteredData %>%
     
      #Assigning the color to the Histogram plot
      #Plotting the Bars of the Histogram with Subscriber_TenureInMonths and churn
      plot_ly(x = ~Subscriber_TenureInMonths, type = "histogram", color = ~churn, colors = c("lightgray", "lightblue")) %>%
      layout(title = "Distribution of Subscriber Tenure in Months", #title of the Histogram
             xaxis = list(title = "Tenure in Months"), #title of the X-axis
             yaxis = list(title = "Count"), #title of the Y-axis
             showlegend = FALSE) #ShowLegend set to False not to show on screen
   
    #Arrange plots using subplot and add titles
    #Assigning P1, P2, P3 to 1 single row with nrows = 1
    #Setting Each plot with independent X and Y axes by setting shareX and share Y to FALSE
    subplot(p1, p2, p3, nrows = 1, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
  })
 
  #Plotting the Visual based on the axis and values of Churn Value on Subscriber_DisconnectionReason
  #Giving a Title of the Histogram Plot to the stacked bar graph
  output$churnPlot10 <- renderPlotly({
    filteredData <- telcoRds
   
    #This condition checks the value of variable toggleChurn
    #If toggle is set to "Churn Only Cohort" visuals will remove the active cohort and keep the churned cohort
    if (input$toggleChurn == "Churn Only Cohort") {
     
      #Filtering on Churn Flag status YES
      filteredData <- filteredData[filteredData$churn == "Yes", ]
    }
   
    #Create a histogram plot based on a dimension Subscriber_DisconnectionReason
    #ShowLegend is set to False not to be displayed on screen
    #Rendering the Plot Object to an Interactive Plot Object
    p <- filteredData %>%
     
      #Plotting the Histogram with Subscriber_DisconnectionReason
      plot_ly(x = ~Subscriber_DisconnectionReason, type = "histogram", color = ~churn, colors = c("lightgray", "lightblue")) %>%
      layout(title = "Histogram of Disconnection Drivers", #title of the Histogram
             xaxis = list(title = "Disconnection Reason"), #title of the X-axis
             yaxis = list(title = "Count"), #title of the Y-axis
             showlegend = FALSE) #ShowLegend set to False not to show on screen
   
    p
  })
 
}

#Executing the dashboard on localhost
shinyApp(ui = ui, server = server)
