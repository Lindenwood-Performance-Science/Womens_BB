library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(RPostgres)
library(DT)
library(dplyr)
library(xgboost)
library(DBI)
library(pool)
library(toastui)
library(ggplot2)
library(lubridate)
library(av)
library(aws.s3)
library(plotly)


options(shiny.sanitize.errors = TRUE)      # Sanitize errors
options(shiny.timeout = 3200)              # Increase the idle timeout to 60 minutes
options(shiny.maxRequestSize = 6*1024^3)  # 6 GB limit



players <- data.frame(
  name = c("Kiara Smith", "Stevi Lockhart", "Gracy Wernli", "Ellie Brueggemann",
           "Aalayah Wilson", "Brooke Coffey", "Alyssa Nielsen", "Mary McGrath",
           "Mya Skoff", "Tina Winn", "Mykayla Cunningham", "Mariah Stewart",
           "Gracie Kelsey", "Justis Odom"),
  number = c(  0, 2, 3, 4,
               5, 12, 15, 21,
               22, 23, 24, 26,
               33, 35),
  position = c("G", "G", "G", "G",
               "G", "G", "F", "G",
               "G", "F", "G", "C",
               "F", "C")
  
)


# UI definition
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, .main-header {
        background-color: black !important;
        color: #B5A36A !important;
      }
      .skin-blue .main-header .logo {
        background-color: black !important;
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar {
        background-color: black !important;
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .sidebar-toggle {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .navbar-custom-menu > .dropdown > a {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .navbar-nav > li > a {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .navbar-nav > li > a:hover {
        background-color: #292929 !important;
      }
      
      /* Style for all DataTables table headers */
      .dataTables_wrapper .dataTable thead {
        background-color: #B5A36A; /* Header background color */
        color: #333; /* Header text color */
        font-size: 14px; /* Header font size */
      }
      
      #videoProgressPitching 
      .progress-bar {
      background-color: #B5A36A;
    }
      
    "))
  ),
  div(id = "loginpage", 
      wellPanel(
        textInput("userName", "Username"),
        passwordInput("passwd", "Password"),
        actionButton("login", "Log in", icon = icon("sign-in"))
      )
  ),
  div(id = "homepage", style = "display: none;",
      dashboardPage(
        dashboardHeader(
          title = "ROAR",
          titleWidth = 250
          
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("house")),
            menuItem("Game Charter", tabName = "gameCharter", icon = icon("basketball"))
          ),
          tags$style(".sidebar-search-container { display: none; }")
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "home",
                    fluidPage(
                      titlePanel("ROAR HomePage"),
                      tags$style(HTML("
                        .full-width-img {
                          width: 100%;
                          height: auto;
                          display: block;
                        }
                      ")),
                      tags$img(src = "LionsLogo.png", class = "full-width-img")
                    )
            ),
            
            tabItem(tabName = "gameCharter",
                    
                    
                    uiOutput("setUp"),
                    uiOutput("startingLineups"),
                    uiOutput("charting"),
                    uiOutput("edit")
            )
            
            
            
          )
        )
      )
  )
)

# Server logic
server <- function(input, output, session) {
  # Hardcoded credentials (for demonstration purposes)
  credentials <- reactiveValues(
    username = "",
    password = ""
  )
  
  observeEvent(input$login, {
    username <- isolate(input$userName)
    password <- isolate(input$passwd)
    
    if (username == credentials$username && password == credentials$password) {
      shinyjs::hide("loginpage")
      shinyjs::show("homepage")
    } else {
      shinyjs::alert("Invalid username or password!")
    }
  })
  
  
  ####################################### Game Charter ########################################
  
  GCState <- reactiveVal("SetUP")
  
  output$GCState <- renderText({
    GCState()
  })
  
  output$setUp <- renderUI({
    
    if(GCState() == "SetUP"){
      tagList(
        fluidRow(
          column(3, dateInput("gameDate", "Game Date", value = Sys.Date())),
          column(3, selectizeInput("oppo", "Opponent", choices = c("Scrimmage"), options = list(create = TRUE))),
          column(3, selectInput("H_A", "Home or Away", choices = c("Home", "Away", "Scrimmage")))
        )
      )
    }
    
    
    
    
    
    
    
  })
  
  output$startingLineups <- renderUI({
    
    req(GCState() == "SetUP")
    req(input$H_A)
    
    if (input$H_A == "Scrimmage"){
      tagList(
        fluidRow(
          column(3, selectInput("LU_1", "Lindenwood Home Team 1", choices = players$number)),
          column(3, selectInput("Oppo_1", "Lindenwood Away Team 1", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_2", "Lindenwood Home Team 2", choices = players$number)),
          column(3, selectInput("Oppo_2", "Lindenwood Away Team 2", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_3", "Lindenwood Home Team 3", choices = players$number)),
          column(3, selectInput("Oppo_3", "Lindenwood Away Team 3", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_4", "Lindenwood Home Team 4", choices = players$number)),
          column(3, selectInput("Oppo_4", "Lindenwood Away Team 4", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_5", "Lindenwood Home Team 5", choices = players$number)),
          column(3, selectInput("Oppo_5", "Lindenwood Away Team 5", choices = players$number))
        )
        
      )
    } else {
      tagList(
        fluidRow(
          column(3, selectInput("LU_1", "Lindenwood 1", choices = players$number)),
          column(3, numericInput("Oppo_1", "Opposing Team 1", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_2", "Lindenwood 2", choices = players$number)),
          column(3, numericInput("Oppo_2", "Opposing Team 2", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_3", "Lindenwood 3", choices = players$number)),
          column(3, numericInput("Oppo_3", "Opposing Team 3", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_4", "Lindenwood 4", choices = players$number)),
          column(3, numericInput("Oppo_4", "Opposing Team 4", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_5", "Lindenwood 5", choices = players$number)),
          column(3, numericInput("Oppo_5", "Opposing Team 5", value = 0))
        ),
        
        actionButton("startGame", "Start Game")
        
      )
    }
  })
  
  gameValues <- reactiveValues(
    HOME_1 = 0,
    HOME_2 = 0, 
    HOME_3 = 0,
    HOME_4 = 0,
    HOME_5 = 0,
    AWAY_1 = 0,
    AWAY_2 = 0,
    AWAY_3 = 0,
    AWAY_4 = 0,
    AWAY_5 = 0,
    opponent = "",
    date = Sys.Date(),
    quarter = 0,
    home_away = ""
  )
  
  observeEvent(input$startGame, {
    # Update game state to "charting"
    GCState("charting")
    
    # Set home and away lineups based on Home/Away/Scrimmage selection
    if (input$H_A %in% c("Home", "Scrimmage")) {
      gameValues$HOME_1 <- input$LU_1
      gameValues$HOME_2 <- input$LU_2
      gameValues$HOME_3 <- input$LU_3
      gameValues$HOME_4 <- input$LU_4
      gameValues$HOME_5 <- input$LU_5
      gameValues$AWAY_1 <- input$Oppo_1
      gameValues$AWAY_2 <- input$Oppo_2
      gameValues$AWAY_3 <- input$Oppo_3
      gameValues$AWAY_4 <- input$Oppo_4
      gameValues$AWAY_5 <- input$Oppo_5
    } else {
      gameValues$HOME_1 <- input$Oppo_1
      gameValues$HOME_2 <- input$Oppo_2
      gameValues$HOME_3 <- input$Oppo_3
      gameValues$HOME_4 <- input$Oppo_4
      gameValues$HOME_5 <- input$Oppo_5
      gameValues$AWAY_1 <- input$LU_1
      gameValues$AWAY_2 <- input$LU_2
      gameValues$AWAY_3 <- input$LU_3
      gameValues$AWAY_4 <- input$LU_4
      gameValues$AWAY_5 <- input$LU_5
    }
    
    # Set game metadata
    gameValues$opponent <- input$oppo
    gameValues$date <- input$gameDate
    gameValues$home_away <- input$H_A
    gameValues$quarter <- 1  # Starting with quarter 1
    
    
  })
  
  on_court_lindenwood <- reactive({
    c(
      gameValues$HOME_1,
      gameValues$HOME_2,
      gameValues$HOME_3,
      gameValues$HOME_4,
      gameValues$HOME_5
    )
  })
  on_court_opponent <- reactive({
    c(
      gameValues$AWAY_1,
      gameValues$AWAY_2,
      gameValues$AWAY_3,
      gameValues$AWAY_4,
      gameValues$AWAY_5
    )
  })
  
  output$charting <- renderUI({
    
    if (GCState() == "charting") {
      tagList(
        radioButtons("eventType", "Shot or Rebound or Free Throw", choices = c("Shot", "Rebound", "Free Throw"), inline = TRUE),
        tags$h4("Select Shooter/Rebounder"),
        fluidRow(
          column(6,
                 tags$h5("Lindenwood Players"),
                 radioButtons("eventPerson", NULL, choices = setNames(on_court_lindenwood(), paste( on_court_lindenwood())), inline = TRUE)),
          column(6,
                 tags$h5("Opponent Players"),
                 radioButtons("eventPerson", NULL, choices = setNames(on_court_opponent(), paste( on_court_opponent())), inline = TRUE))),
        conditionalPanel("input.eventType == 'Shot'", 
                         fluidRow(
                           column(4, 
                                  radioButtons("dbocShot", "Shot Off the Catch or Dribble", choices = c("Catch", "Dribble"), inline = TRUE)),
                           column(4, 
                                  radioButtons("shotOutcomeShot", "Make or Miss Shot", choices = c("Make", "Miss"), inline = TRUE)))),
        conditionalPanel("input.eventType == 'Rebound'",  
                         radioButtons("offOrDefRebound", "Defensive or Offensive Rebound", choices = c("Defensive", "Offensive"), inline = TRUE)),
        conditionalPanel("input.eventType == 'Free Throw'", 
                         fluidRow(
                           column(4, 
                                  radioButtons("shotOutcomeFT", "Make or Miss Free Throw", choices = c("Make", "Miss"), inline = TRUE)))),
        plotOutput("court", click = "courtClick"),
        actionButton("submitBtn", "Submit"),
        actionButton("editBtn", "Edit Situation")
      )
    }
  })
  
  
  
  
  output$court <- renderPlot({
    par(mai = c(0, 0, 0, 0))
    plot(1, ylim = c(-30, 30), xlim = c(-50, 50), type = 'n', yaxs = 'i', xaxs = 'i', ylab = '', xlab = '', axes = F, asp = 1)
    
    #out of bounds
    rect(xleft = -47, ybottom = -25,
         xright = 47, ytop = 25,
         border = "black", lwd = 2)
    
    #half-court line
    segments(0, -25, 0, 25, col = "black", lwd = 2)
    
    #center circle
    symbols(x = 0, y = 0, circles = 6, inches = FALSE, add = TRUE, fg = "black", lwd = 2)
    
    #backboard
    segments(-43,3,-43,-3, col = "black", lwd = 2)
    segments(43,3,43,-3, col = "black", lwd = 2)
    
    #paint
    rect(-47, -7.5, -28, 7.5, border = "black", lwd = 2)
    rect(28, -7.5, 47, 7.5, border = "black", lwd = 2)
    
    #free throw cirlces
    curve(sqrt(6^2 - (x + 28)^2), from = -28, to = -22, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(6^2 - (x + 28)^2), from = -28, to = -22, add = TRUE, col = "black", lwd = 2)
    curve(sqrt(6^2 - (x - 28)^2), from = 22, to = 28, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(6^2 - (x - 28)^2), from = 22, to = 28, add = TRUE, col = "black", lwd = 2)
    
    #hoops
    symbols(x = -41.75, y = 0, circles = 1.5/2, inches = FALSE, add = TRUE, fg = "black", lwd = 2)  # Left hoop
    symbols(x = 41.75, y = 0, circles = 1.5/2, inches = FALSE, add = TRUE, fg = "black", lwd = 2)   # Right hoop
    
    
    #3pt lines
    segments(-47, -21.65625, -37, -21.65625, col = "black", lwd = 2)
    segments(-47, 21.65625, -37, 21.65625, col = "black", lwd = 2)
    
    segments(47, -21.65625, 37, -21.65625, col = "black", lwd = 2)
    segments(47, 21.65625, 37, 21.65625, col = "black", lwd =2)
    
    #3pt arcs
    curve(sqrt(22.14583^2 - (x + 41.75)^2), from = -37, to = -19.60417, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(22.14583^2 - (x + 41.75)^2), from = -37, to = -19.60417, add = TRUE, col = "black", lwd = 2)
    
    curve(sqrt(22.14583^2 - (x - 41.75)^2), from = 37, to = 19.60417, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(22.14583^2 - (x - 41.75)^2), from = 37, to = 19.60417, add = TRUE, col = "black", lwd = 2)
    
    
   
    # Event Locs
    if (!is.null(GCCourtPoint$temp_court$x)) {
      points(
        GCCourtPoint$temp_court$x,
        GCCourtPoint$temp_court$y,
        col = "green",
        pch = ifelse(input$eventType == "Shot", 19, 
                     ifelse(input$eventType == "Rebound", 15, 17)), cex = 1.5)
    }
    
    if (!is.null(GCCourtPoint$permanent$x)) {
      points(
        GCCourtPoint$permanent$x,
        GCCourtPoint$permanent$y,
        col = "red",
        pch = ifelse(GCCourtPoint$permanent$event == "Shot", 19, 
                     ifelse(GCCourtPoint$permanent$event == "Rebound", 15, 17)), cex = 1.5)
    }
    
    
  }, bg = "transparent")
  
  
  observeEvent(input$submitBtn, {
    # Check if temporary points exist
  if (nrow(GCCourtPoint$temp_court) == 0) {
    showNotification("Error: No event location selected on the court.", type = "error", duration = 3)
    return()
  }
  
  # Check if necessary inputs are selected based on event type
  if (is.null(input$eventType)) {
    showNotification("Error: Please select an event type (Shot, Rebound, Free Throw).", type = "error", duration = 3)
    return()
  }
  
  if (is.null(input$eventPerson)) {
    showNotification("Error: Please select the Shooter/Rebounder.", type = "error", duration = 3)
    return()
  }
  
  if (input$eventType == "Shot") {
    if (is.null(input$shotOutcomeShot)) {
      showNotification("Error: Please select Shot Outcome (Make or Miss).", type = "error", duration = 3)
      return()
    }
    if (is.null(input$dbocShot)) {
      showNotification("Error: Please select Shot Type (Catch or Dribble).", type = "error", duration = 3)
      return()
    }
  } else if (input$eventType == "Rebound") {
    if (is.null(input$offOrDefRebound)) {
      showNotification("Error: Please select Offensive or Defensive rebound.", type = "error", duration = 3)
      return()
    }
  } else if (input$eventType == "Free Throw") {
    if (is.null(input$shotOutcomeFT)) {
      showNotification("Error: Please select Free Throw Outcome (Make or Miss).", type = "error", duration = 3)
      return()
    }
  }
    
      print("IN")
      playerID <- input$eventPerson
      eventX <- round(GCCourtPoint$temp_court$x, 2)
      eventY <- round(GCCourtPoint$temp_court$y, 2)
      Time <- format(Sys.time(), tz = "America/Chicago")
      
      
      # Clear the temporary points after submitting
      GCCourtPoint$temp_court <- data.frame(x = numeric(0), y = numeric(0))
      
      if (eventX > 0){
        distance <- ((eventX-41.75)^2 + (eventY)^2)^0.5
      } else {
        distance <- ((eventX+41.75)^2 + (eventY)^2)^0.5
      }
      
      
      print(distance)
      event<-input$eventType
      
      con <- dbConnect(RPostgres::Postgres(),
                       dbname = "ps1",
                       user = "pythoncon",
                       password = "password",
                       host = "18.217.248.114",
                       port = "5432")
      
      shotCategory <- "FG"
      
    tryCatch({
      if (event == "Shot") {
        shotResult <- input$shotOutcomeShot
        shotType <- input$dbocShot
        # Function to classify shot zones
        event_zone <- function(distance) {
          if (distance < 10) {
            return("short")
          } else if (distance >= 10 & distance <= 20) {
            return("mid")
          } else {
            return("long")
          }
        }
        shot_zone <- event_zone(distance)
        
        # Function for 3pt or not
        if (distance > 22 && eventX != 0) {
          shotCategory <- "Three Point"  
        } else if (eventY == 0) { 
          shotCategory <- "Free Throw"
        } else {
          shotCategory <- "Field Goal" 
        }
        
        
        shotTypeValue <- ifelse(shotCategory == "Three Point", 1, 0)  # 1 for 3-point shot, 0 for other shots
        fgTypeValue <- ifelse(shotCategory == "Free Throw", 0, 1)  # 0 for Free Throw, 1 for FG
        ftValue <- ifelse(shotCategory == "Free Throw", 1, 0)  # 1 for Free Throw, 0 otherwise
        
        
        print(playerID)
        print(eventX)
        print(eventY)
        print(distance)
        print(shotResult)
        print(shotType)
        print(shotTypeValue)
        print(shotCategory)
        print(fgTypeValue)
        print(gameValues$date)
        print(gameValues$opponent)
        print(gameValues$home_away)
        print(gameValues$quarter)
        print(Time)
        print(gameValues$HOME_1)
        print(gameValues$HOME_2)
        print(gameValues$HOME_3)
        print(gameValues$HOME_4)
        print(gameValues$HOME_5)
        print(gameValues$AWAY_1)
        print(gameValues$AWAY_2)
        print(gameValues$AWAY_3)
        print(gameValues$AWAY_4)
        print(gameValues$AWAY_5)
        
        # Insert shot data into database
        insert_query <- glue::glue(
          "
    INSERT INTO w_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, made_miss, free_throw, three_pt, dribble_catch, fg, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, quarter, time_of_day)
    
    VALUES(
      'Shot', '{playerID}', {eventX}, {eventY}, {distance}, '{shot_zone}', '{shotResult}', '{ftValue}', {shotTypeValue}, '{shotType}', {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, {gameValues$AWAY_1}, {gameValues$AWAY_2}, {gameValues$AWAY_3}, {gameValues$AWAY_4}, {gameValues$AWAY_5},
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$quarter}, '{Time}'
    )
    "
        )
        
        print(insert_query)
        
        dbExecute(con, insert_query)
        dbDisconnect(con)
        
        print("Done")
      }
      
      #rebound 
      if (event == "Rebound") {
        reboundResult <- input$offOrDefRebound
        # Function to classify shot zones
        event_zone <- function(distance) {
          if (distance < 10) {
            return("short")
          } else if (distance >= 10 & distance <= 20) {
            return("mid")
          } else {
            return("long")
          }
        }
        rebound_zone <- event_zone(distance)
        
        # Function to classify shot recovery zones
        shot_recovery_zone <- function(distance) {
          if (distance < 10) {
            return("short")
          } else if (distance >= 10 & distance <= 20) {
            return("mid")
          } else {
            return("long")
          }
        }
        recovery_zone <- shot_recovery_zone(distance)
        
        # Print for debugging
        print(playerID)
        print(eventX)
        print(eventY)
        print(distance)
        print(reboundResult)
        print(gameValues$date)
        print(gameValues$opponent)
        print(gameValues$home_away)
        print(gameValues$quarter)
        print(Time)
        print(gameValues$HOME_1)
        print(gameValues$HOME_2)
        print(gameValues$HOME_3)
        print(gameValues$HOME_4)
        print(gameValues$HOME_5)
        print(gameValues$AWAY_1)
        print(gameValues$AWAY_2)
        print(gameValues$AWAY_3)
        print(gameValues$AWAY_4)
        print(gameValues$AWAY_5)
        
        # Insert rebound data into database
        insert_query <- glue::glue(
          "
    INSERT INTO w_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, off_def, shot_recovery_zone, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, quarter, time_of_day)
    
    VALUES(
      'Rebound', '{playerID}', {eventX}, {eventY}, {distance}, '{rebound_zone}', '{reboundResult}', '{recovery_zone}',
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, {gameValues$AWAY_1}, {gameValues$AWAY_2}, {gameValues$AWAY_3}, {gameValues$AWAY_4}, {gameValues$AWAY_5},
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$quarter}, '{Time}'
    )
    "
        )
        
        print(insert_query)
        
        dbExecute(con, insert_query)
        dbDisconnect(con)
        
        print("Done")
        
      }
      
      #free throw 
      if (event == "Free Throw") {  
        distance <- 15  # Fixed distance for free throws
        
        # Shot result (make or miss)
        ftResult <- input$shotOutcomeFT  
        
        # Free throw shotCategory and fg settings
        shotCategory <- "Free Throw"
        shotTypeValue <- 0  # Free throw is not a 3-pointer
        fgTypeValue <- 0  # Free throw doesn't count as a field goal
        ftValue <- 1  # Free throw shot is always 1
        
        
        insert_query <- glue::glue(
          "
    INSERT INTO w_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, made_miss, free_throw, three_pt, fg, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, quarter, time_of_day)
    
    VALUES(
      'Free Throw', '{playerID}', {eventX}, {eventY}, {distance}, '{ftResult}', '{ftValue}', {shotTypeValue}, {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, {gameValues$AWAY_1}, {gameValues$AWAY_2}, {gameValues$AWAY_3}, {gameValues$AWAY_4}, {gameValues$AWAY_5},
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$quarter}, '{Time}'
    )
    "
        )
        
        print(insert_query)
        dbExecute(con, insert_query)
        dbDisconnect(con)
        
        print("Done")
      }
      showNotification("Event successfully recorded!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification("Error: Could not save data to the database.", type = "error", duration = 3)
    }, finally = {
    })
  })
  
  GCCourtPoint <- reactiveValues(
    temp_court = data.frame(x = numeric(0), y = numeric(0)),
    permanent = data.frame(x = numeric(0), y = numeric(0), event = NULL)
  )
  
  observeEvent(input$courtClick,{
    x <- input$courtClick$x
    y <- input$courtClick$y
    new_point <- data.frame(x = x, y = y)
    GCCourtPoint$temp_court <- new_point
  })
  
  observeEvent(input$editBtn, {
    # Open modal to edit players and quarter
    showModal(modalDialog(
      title = "Edit Situation",
      fluidRow(
        column(6, 
               selectInput("editHome1", "Home Player 1", choices = players$number, selected = gameValues$HOME_1)),
        column(6, 
               numericInput("editAway1", "Away Player 1", value = gameValues$AWAY_1, min = 0, step = 1))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome2", "Home Player 2", choices = players$number, selected = gameValues$HOME_2)),
        column(6, 
               numericInput("editAway2", "Away Player 2", value = gameValues$AWAY_2, min = 0, step = 1))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome3", "Home Player 3", choices = players$number, selected = gameValues$HOME_3)),
        column(6, 
               numericInput("editAway3", "Away Player 3", value = gameValues$AWAY_3, min = 0, step = 1))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome4", "Home Player 4", choices = players$number, selected = gameValues$HOME_4)),
        column(6, 
               numericInput("editAway4", "Away Player 4", value = gameValues$AWAY_4, min = 0, step = 1))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome5", "Home Player 5", choices = players$number, selected = gameValues$HOME_5)),
        column(6, 
               numericInput("editAway5", "Away Player 5", value = gameValues$AWAY_5, min = 0, step = 1))
      ),
      fluidRow(
        column(6, 
               selectInput("editQuarter", "Quarter", choices = 1:4, selected = gameValues$quarter))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveChanges", "Save Changes")
      )
    ))
  })
  
  
  
  observeEvent(input$saveChanges, {
      gameValues$HOME_1 <- input$editHome1
      gameValues$HOME_2 <- input$editHome2
      gameValues$HOME_3 <- input$editHome3
      gameValues$HOME_4 <- input$editHome4
      gameValues$HOME_5 <- input$editHome5
      gameValues$AWAY_1 <- input$editAway1
      gameValues$AWAY_2 <- input$editAway2
      gameValues$AWAY_3 <- input$editAway3
      gameValues$AWAY_4 <- input$editAway4
      gameValues$AWAY_5 <- input$editAway5
      gameValues$quarter <- input$editQuarter
      
      # Close the modal
      removeModal()
  })
  
}


# Shiny app
shinyApp(ui, server)
