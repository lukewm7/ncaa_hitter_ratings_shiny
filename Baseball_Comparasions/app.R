#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(shiny)
library(baseballr)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)
library(bslib)
library(purrr)
library(RColorBrewer)

# Load dataset with all NCAA college teams from your github
ncaa_stats <- read.csv("player_stats.csv")

# Define UI for shiny app
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "superhero", version = 5),
  
  # Title of app
  titlePanel("Luke's Ratings: College Baseball Hitters"),
  
  #Subtitle
  h5("Ratings Created Using a Combination of Sabermetrics and Personal Input"),
  
  tags$br(),
  
  # Sidebar with division and conference input fields
  sidebarLayout(
    sidebarPanel(
      selectInput("division", "Select Division", choices = c(1,2,3), selected = 1),
      uiOutput("ConferenceControl"),
      plotlyOutput("top_overall") 
    ),
    
    # Main panel to display output
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("By Player", plotOutput("top_by_team_plot"), tags$br(), DT::dataTableOutput("stats_table")),
                  tabPanel("By Team", plotlyOutput("top_teams"), tags$br(), DT::dataTableOutput("team_stats_table")),
                  tabPanel("Ratings Explained", uiOutput("ratings_rmd"))
      )
    )
  )
)

# Define server function
server <- function(input, output, session) {
  
  # RMD to explain how I calculated the ratings
  output$ratings_rmd <- renderUI({
    includeHTML("ratings_overview.html")
  })
  
  # Filter dataset to only include schools in selected division
  ncaa_teams_conference_choices <- reactive({
    ncaa_stats %>% 
      filter(division == input$division)%>%
      select(conference)%>%
      unique()
  })
  
  output$ConferenceControl <- renderUI({
    selectInput("ConferenceC", label="Conference", choices = ncaa_teams_conference_choices()$conference, selected = ncaa_teams_conference_choices()$conference[[1]])
  })
  
  # Filter the resulting dataframe to get all schools where the conference matches the user input
  df_conf_teams <- reactive({
    ncaa_stats %>% 
      filter(division == input$division, conference == input$ConferenceC)
  })
  
  
  #Creating the final dataset
  conference_stats <- reactive({
    
    # Calculate 8 metrics for each player in the conference_stats dataframe
    conference_stats <- df_conf_teams() %>%
      mutate_if(is.numeric, ~if_else(is.na(.), 0, .))%>%
      filter(AB > 50)%>%
      mutate(non_hr_pwr = (X2B + X3B)/AB,
             gap_power = non_hr_pwr/mean(non_hr_pwr, na.rm = TRUE),
             ISO = SlgPct - BA,
             Pop = ISO / mean(ISO, na.rm = TRUE),
             Contact = BA / mean(BA, na.rm = TRUE), 
             bb_rate = BB/AB,
             Eye = bb_rate / mean(bb_rate, na.rm = TRUE),
             k_rate = K/AB,
             SO = mean(k_rate, na.rm = TRUE)/k_rate,
             raw_hit = (gap_power/sd(gap_power))+(Pop/sd(Pop))+(Contact/sd(Contact))+(Eye/sd(Eye))+(SO/sd(SO)),
             Hit = raw_hit/mean(raw_hit, na.rm = TRUE),
             sp1 = ((SB+3)/(SB+CS+7)-.4)*20,
             sp2 = (sqrt((SB+CS)/((H - HR - X2B - X3B) + BB + HBP)))*(1/.07),
             sp3 = (X3B/(AB - HR - K)) * 625,
             sp4 = (((R - HR)/(H + BB + HBP - HR)) - 0.1) * 25,
             sp5 = (0.063 - (DP/(AB-HR-K))) * (1/0.007),
             raw_run = (sp1/sd(sp1, na.rm = TRUE)) + (sp2/sd(sp2, na.rm = TRUE)) + (sp3/sd(sp3, na.rm = TRUE)) + (sp4/sd(sp4, na.rm = TRUE)) + (sp5/sd(sp5, na.rm = TRUE)),
             Run = raw_run/mean(raw_run, na.rm = TRUE))%>%
      select(player_name, team_name, Yr, Pos, gap_power, Pop, Contact, Eye, SO, Hit, Run)%>%
      mutate_if(is.numeric, funs(rescale(., to = c(70, 97))))%>%
      mutate(OVR = (.9 * Hit) + (.1 * Run))%>%
      mutate_if(is.numeric, round)
  })
  
  #Data Grouped by Team in the Conference
  grouped_team <- reactive({
    grouped_data <- conference_stats() %>%
      group_by(team_name)%>%
      summarise(gap_power = mean(gap_power),
                Pop = mean(Pop),
                Contact = mean(Contact),
                Eye = mean(Eye),
                SO = mean(SO),
                Hit = mean(Hit),
                Run = mean(Run),
                OVR = mean(OVR))%>%
      mutate_if(is.numeric, round)
  })
  
  # Create reactive expression for top 3 players by team
  top3_by_team <- reactive({
    conference_stats() %>%
      group_by(team_name) %>%
      top_n(3, OVR)
  })
  
  #Plot for top players from each team
  output$top_by_team_plot <- renderPlot({
    ggplot(top3_by_team(), aes(x = reorder(player_name, OVR), y = OVR, color = team_name)) +
      geom_segment(aes(x = reorder(player_name, OVR), xend = reorder(player_name, OVR), y = 0, yend = OVR), color = "gray", size = 1) +
      geom_point(size = 3) +
      facet_wrap(~team_name, ncol = 2, scales = "free") +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      coord_flip() +
      xlab("Player Name") +
      ylab("Overall Rating") +
      ggtitle("Top 3 Players by Overall Rating for Each Team in Conference")
  })
  
  #Plotly for Team Stats
  output$top_teams <- renderPlotly({
  p2 <- ggplot(grouped_team(), aes(x = reorder(team_name, OVR), y = OVR, color = team_name, 
                        text = paste( "Overall Rating:", OVR, "<br>",
                                     "Hit:", Hit, "<br>", "Run:", Run))) +
    geom_segment(aes(x = reorder(team_name, OVR), xend = reorder(team_name, OVR), y = 0, yend = OVR), 
                 color = "gray", size = 1.5, position = position_dodge(width = 0.75)) +
    geom_point(size = 6, position = position_dodge(width = 0.75)) +
    labs(title = "Top Teams by OVR",
         x = "",
         y = "Overall Rating") +
    coord_flip() +
    theme_minimal()+
    geom_text(aes(label = OVR),color = "black", size = 3, vjust = -1.5, position = position_dodge(width = 0.75))
  
  ggplotly(p2, tooltip = "text")
  })
  
  #Plotly for top 10 players in the conference
  output$top_overall <- renderPlotly({
    data <- conference_stats() %>% 
      filter(!is.na(OVR)) %>% 
      arrange(desc(OVR)) %>% 
      head(10)
    
    p3 <- ggplot(data, aes(x = reorder(player_name, OVR), y = OVR, color = team_name, 
                          text = paste("Player:", player_name, "<br>", "Team:", team_name, "<br>", 
                                       "Overall Rating:", OVR, "<br>",
                                       "Hit:", Hit, "<br>", "Run:", Run))) +
      geom_segment(aes(x = reorder(player_name, OVR), xend = reorder(player_name, OVR), y = 0, yend = OVR), 
                   color = "gray", size = 1.5, position = position_dodge(width = 0.75)) +
      geom_point(size = 6, position = position_dodge(width = 0.75)) +
      labs(title = "Top 10 Players(OVR)",
           x = "Player Name",
           y = "Overall Rating") +
      coord_flip() +
      theme_minimal() +
      guides(color = FALSE)+
      geom_text(aes(label = OVR),color = "black", size = 3, vjust = -1.5, position = position_dodge(width = 0.75))
    
    ggplotly(p3, tooltip = "text")
  })
  
  
  #Table for all player stats
  output$stats_table <- DT::renderDataTable({
    # Create the table using the conference_stats() data frame
    DT::datatable(conference_stats(), colnames = c("ID", "Player Name", "Team Name", "Year", "Pos", "Gap Power", "Pop", "Contact", "Eye", "SO", "Hit", "Run", "OVR"))%>%
      formatRound(columns = c("gap_power", "Pop", "Contact", "Eye", "SO", "Hit", "Run", "OVR"), digits = 0) %>%
      formatStyle(
        columns = c("gap_power", "Pop", "Contact", "Eye", "SO", "Hit", "Run", "OVR"),
        backgroundColor = styleInterval(c(70, 80, 90), c("#8b0000", "#E57373", "#81C784", "#006400")),
        fontWeight = 'bold'
      )
  })
  
  # Table for all team stats
  output$team_stats_table <- DT::renderDataTable({
    # Create the table using the conference_stats() data frame
    DT::datatable(grouped_team(), colnames = c("Team Name", "Gap Power", "Pop", "Contact", "Eye", "SO", "Hit", "Run", "OVR")) %>%
      formatRound(columns = c("gap_power", "Pop", "Contact", "Eye", "SO", "Hit", "Run", "OVR"), digits = 0) %>%
      formatStyle(
        columns = c("gap_power", "Pop", "Contact", "Eye", "SO", "Hit", "Run", "OVR"),
        backgroundColor = styleInterval(c(70, 80, 90), c("#8b0000", "#E57373", "#81C784", "#006400")),
        fontWeight = 'bold'
      )
  })
  
  

}


# Run the shiny app
shinyApp(ui = ui, server = server)


