library(shiny)
library(maps)
library(mapdata)
library(ggplot2)
library(dplyr)
df<-readRDS("Data/df.rds") 
year <- "2018"
strat<-"Overall"
c<-1
goodq<-c()
count<-c()
for (q in unique(df$Question)){
  df2<-df %>%filter(!DataValue=="", Question == q, YearEnd=="2018", Stratification1 == strat, DataValueType == "Crude Rate")%>%select(QuestionID)
  if (length(df2$QuestionID)>50){
    goodq[c]<-q
    count[c]<-length(df2$QuestionID)
    c<-c+1
  }
}
ui <- fluidPage(
  titlePanel("Explore Chronic Disease Indicator Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select an Indicator:", choices = goodq),
    ),
    mainPanel(
      plotOutput("map"),
    )
  )
)

server <- function(input, output) {

  #results_df %>% filter(length>50) in console to find the qids that have full data
  #there are 15
  
  output$map <- renderPlot({
    ALC6_0<-df %>%filter(Question == input$indicator, !LocationDesc== "District of Columbia", !DataValue=="", YearStart == year, Stratification1 == strat, DataValueType == "Crude Rate") %>%
      mutate(state = tolower(LocationDesc),
             value = DataValue) %>% 
      select(state, value)
    # get the map data for the US
    us_map <- map_data("state")
    
    # merge the state values with the map data
    us_map_values <- merge(us_map, ALC6_0, by.x = "region", by.y="state")
    
    # Convert 'value' column to numeric
    us_map_values$value <- as.numeric(as.character(us_map_values$value))
    #The merge unsorts the dataframe and apparently the order matters for the map
    us_map_values_sorted <- us_map_values %>% arrange((order))
    # Create the map using ggplot
    ggplot(us_map_values_sorted, aes(x = long, y = lat, group = group, fill = value)) +
      geom_polygon(color = "black", size = 0.1) +  # Add borders to states
      scale_fill_gradient(low = "lightblue", high = "darkred") +  # Change color scheme
      labs(fill = "Per 10,000",  # Rename legend
           title = input$indicator,  # Add title
           caption = "https://chronicdata.cdc.gov") +  # Add source
      theme_minimal() +  # Use minimal theme
      theme(legend.position = "bottom",  # Move legend to bottom
            plot.title = element_text(hjust = 0.5))  # Center title
  })
  
}
shinyApp(ui, server)