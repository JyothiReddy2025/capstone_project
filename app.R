library(shiny)
library(RSQLite)
library(shinythemes)
library(shinydashboard)
library(DT)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyverse)
library(Rcpp)
library(ggplot2)
library(rmarkdown)
library(rJava)
library(gridExtra)
library(rsconnect)
library(here)


library(gridExtra)



ui <-  navbarPage(title = "Maryville-Lab",id = "Navp", collapsible = TRUE, inverse = TRUE, theme = shinythemes::shinytheme("journal"), 
                  #navbarPage( id = "Navp",
                  tabPanel(title = "Login", "Login", "Welcome to Maryville lab",
                           fluidPage(title = NULL,
                                     fluidRow(
                                       column(width = 12,
                                              sidebarLayout(
                                                sidebarPanel(id = "Login",
                                                             textInput("userName", "User Name*", ""),
                                                             passwordInput("passwd", "Password*:"),
                                                             br(),actionButton("Login", "Log in", class = "btn-primary"),
                                                             br(),br(),actionButton("NewUser", "Sign Up", class = "btn-primary"),
                                                             verbatimTextOutput("dataInfo")
                                                ),
                                                mainPanel(
                                                  img(src = "lab.jpg", height = 700, width = 1100)
                                                ),
                                                #position = c("right"),
                                              ))))),
                  
                  tabPanel(title = "NewUser", "NewUser",
                           fluidPage(title = NULL,
                                     fluidRow(
                                       
                                       column(width = 12,
                                              sidebarLayout(
                                                sidebarPanel(id = "NewUser",
                                                             textInput("First_Name", "First Name*", ""),
                                                             textInput("Last_Name", "Last Name*", ""),
                                                             textInput("email", "Email*", ""),
                                                             textInput("UserId", "User Name*", ""),
                                                             passwordInput("password", "Password*"),
                                                             textInput("Role", "Role*", ""),
                                                             selectInput("Gender", "Gender*", c("Choose one" = "", "Male" = "male", "Female" = "female")),
                                                             dateInput("DOB", "Date of Birth*", value = "yyyy-mm-dd", format = "yyyy/mm/dd"),
                                                             checkboxInput("terms*", "Terms and Conditions", FALSE),
                                                             actionButton("submit", "Sign Up", class = "btn-primary")
                                                ),
                                                mainPanel(""),
                                                #position = c("right"),
                                              )
                                              
                                       )))),
                  tabPanel(title = "Customer",span(actionButton("Logout", "Logout", class = "btn-primary"),style = "position:absolute;right:2em;"),
                           #fluidPage(title = NULL,
                           #fluidRow(
                           
                           sidebarLayout(
                             sidebarPanel(id = "Customer",
                                          # wellPanel(
                                          
                                          verbatimTextOutput("CustInfo"),
                                          downloadButton('downloadReportCustomer','Download Results'),
                                          htmlOutput("CustId"),
                                          selectInput("gender", "Gender:",c("Male", "Female")),
                                          dateInput("RequestDate", "Request Date", format = "yyyy-mm-dd"),
                                          htmlOutput("TestName1"),
                                          htmlOutput("LabLocation1"),
                                          br(),actionButton("order", "Order")),
                             
                             mainPanel(h3(textOutput("caption"))),
                             #position = c("right"),
                             
                           )),
                  
                  tabPanel(title = "Analyst", span(actionButton("Logout", "Logout", class = "btn-primary"),style = "position:absolute;right:2em;"),
                           h1(strong("Analyst Page")), 
                           
                           navbarPage(id = "analystpage", verbatimTextOutput("AnaInfo"),
                                      
                                      frow1 <- fluidRow(                
                                        
                                        title = "Test Results"
                                        ,actionButton("displayResults", label = "Display Records")
                                        ,actionButton("action", label = "Update Records")
                                        ,br(),br()
                                        , width = "1100px"
                                        ,status = "primary"
                                        ,solidHeader = TRUE 
                                        ,collapsible = TRUE
                                        ,label = "View Results"   ### )
                                        ,DTOutput("Results", height = "300px", width = "1100px")
                                        ###           ,actionButton("action", label = "Write to DB")
                                      ),      
                                      
                                      
                                      
                                      ##### Add new tests tab
                                      
                                      tabPanel("Add New Test Types", id= "testtypes",
                                               
                                               tagList(
                                                 div(id = "TestTypes", br(), br(),
                                                     wellPanel(
                                                       
                                                       br(),
                                                       textInput ("TestName", "Test Name:"),
                                                       
                                                       br(),actionButton("save", "Save"))),
                                                 tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                               )          
                                      )
                           )
                           
                  ), 
                  tabPanel("About","Our Lab Information", fluidPage(fluidRow(column(width=12,sidebarLayout(sidebarPanel(p('Laboratory Corporation of America Holdings, more commonly known as LabCorp, is an American S&P 500
                           company headquartered in Burlington, North Carolina. It operates one of the largest clinical laboratory
                           networks in the world, with a United States network of 36 primary laboratories. Before a merger with National 
                           Health Laboratory in 1995,',style = "color:black")),mainPanel(img(src = "history.png", height = 400, width = 900))))))
                  ),
                  
                  
                  tabPanel(title = "Dashboard", 
                           dashboardPage(skin = "yellow",
                                         
                                         dashboardHeader(),
                                         dashboardSidebar(
                                           selectInput("LabLoc", "Lab Location:", choices = c("Chesterfield",     "Ballwin")),
                                           checkboxGroupInput("Ttype", label = h3("Test Type"),choices = list("Lipid Profile" , "Glucose"),selected = 'Glucose'),
                                           fluidRow(column(2, verbatimTextOutput("Ttype"))),
                                           fluidRow(column(3,checkboxGroupInput("Sex", label = h3("Gender"),choices = list("Male" , "Female"),selected = 'Female'))),
                                           fluidRow(column(3, verbatimTextOutput("Sex")))
                                         ), 
                                         ####dashboardSidebar end
                                         
                                         dashboardBody(
                                           
                                           fluidRow( 
                                             box(
                                               title = "Max cholesterol/Diabetic By Location"
                                               ,status = "primary"
                                               ,solidHeader = TRUE 
                                               ,collapsible = TRUE 
                                               ,plotOutput("MaxTestResultsbyType", height = "300px")
                                             ),
                                             fluidRow(valueBoxOutput("value1"), valueBoxOutput("value2"),valueBoxOutput("value3"),         
                                                      br(),br(),br(),
                                                      downloadButton('downloadReport','Download Orders'))
                                             
                                           )
                                           
                                           
                                         )   # Dashboard body end
                                         
                           )  ####dashboardPage end
                           
                  ),  
                  tabPanel("Locations", "Here are the Locations",
                           fluidPage(title = NULL,
                                     fluidRow(
                                       column(width = 12,
                                              sidebarLayout(
                                                sidebarPanel("Chesterfield",
                                                             helpText("Baxter Crossings",br(),
                                                                      "Chesterfield",br(),
                                                                      "MO - 63005")
                                                ),
                                                mainPanel(
                                                  img(src = "lab.jpg", height = 400, width = 600)
                                                  
                                                  
                                                ),
                                                #position = c("right"),
                                              )),
                                       
                                       column(width = 12,
                                              sidebarLayout(
                                                sidebarPanel("Ballwin",
                                                             helpText("Maryville",br(),
                                                                      "Ballwin",br(),
                                                                      "St.Louis",br(),
                                                                      "MO - 63005")
                                                             
                                                ),
                                                mainPanel(""),
                                                #position = c("right"),
                                              )
                                              
                                       )))),
                  
                  
                  tabPanel("Support and Contact", "Here are the Support Details",
                           fluidPage(title=NULL, 
                                     fluidRow(
                                       column(width=12,
                                              sidebarLayout(
                                                sidebarPanel("EMAIL",
                                                             helpText(("jkothakota1@live.maryville.edu"))),
                                                mainPanel()
                                              )),
                                       column(width=12,
                                              sidebarLayout(
                                                sidebarPanel("Manager",
                                                             helpText(("650-452-0451"))),
                                                mainPanel()),
                                              sidebarLayout(
                                                sidebarPanel("Support-Team",
                                                             helpText(("123-456-789"))),
                                                mainPanel())
                                              
                                       ),
                                       
                                     ))
                           
                           
                  )
                  #,
                  
                  #      tabPanel("Download Data",
                  #               fluidPage(title=NULL, 
                  #                         fluidRow(
                  #                           mainPanel(
                  #                           downloadButton('downloadReport'))
                  #                           
                  #                         ))
                  
                  
                  #     )
                  
                  
                  
                  ##############   Logout button  ###############
                  
                  , tabPanel(title = "Logout", actionButton("Logout", "Logout") )
                  
                  
                  ############################
                  
                  
                  
)


################################################################
################# Database Connection ############################
################################################################

top_dir <- here()
sqlitePath <- paste0(top_dir,'/',"labdata.db")

db <- dbConnect(SQLite(), sqlitePath)
# Construct the fetching query
query <- sprintf("CREATE TABLE IF NOT EXISTS USERS(First_Name TEXT, Last_Name TEXT, email TEXT, UserId TEXT, password TEXT, Gender TEXT, DOB TEXT, Role TEXT);", table)
#query <- sprintf("CREATE TABLE IF NOT EXISTS USERS(Password TEXT, Name TEXT, UserId TEXT, Role TEXT);", table)
query1 <- sprintf("CREATE TABLE IF NOT EXISTS TestTypes(TestName TEXT);", table)
query2 <- sprintf("CREATE TABLE IF NOT EXISTS TestResults(CustId TEXT, gender TEXT, RequestDate TEXT, LabLocation1 TEXT, TestName1 TEXT, Test1 TEXT, Test1Std TEXT, Test1Results TEXT, Test2 TEXT, Test2Std TEXT, Test2Results TEXT, Test3 TEXT, Test3Std TEXT, Test3Results TEXT, Test4 TEXT, Test4Std TEXT, Test4Results TEXT);", table)
query3 <- sprintf("CREATE TABLE IF NOT EXISTS Location(LabLocation TEXT);", table)
table_create <- dbExecute(db, query)
table_create1 <- dbExecute(db, query1)
table_create2 <- dbExecute(db, query2)
table_create3 <- dbExecute(db, query3)

dbDisconnect(db)
#Defining the feilds from new user registration
#query4 <- sprintf("INSERT INTO Location (LabLocation)VALUES('Ballwin');", table)
#table_create5 <- dbExecute(db, query4)
#query4 <- sprintf("UPDATE USERS SET Role = 'analyst' WHERE First_Name = 'jyothi';", table)
#table_create5 <- dbExecute(db, query4)
#Lipid Profile" , "Glucose"
#query6 <- sprintf("SELECT UserId from USERS")
#table_create7 <- dbExecute(db, query6)

#dbReadTable(db, "USERS")
#db_drop_table(db, "TestTypes")


NewUserRegistration <- c("First_Name", "Last_Name", "email", "UserId", "password", "Gender", "DOB", "Role")
NewTestTypes <- c("TestName")
NewTestOrder <- c("CustId","gender", "RequestDate","TestName1","LabLocation1", "Test1Std")


################################################################
################# Server side logic ############################
################################################################

server <- function(input, output, session) {
  
  hideTab(inputId = "Navp", target = "Dashboard")
  hideTab(inputId = "Navp", target = "Analyst")
  hideTab(inputId = "Navp", target = "Customer")
  hideTab(inputId = "Navp", target = "Logout")
  hideTab(inputId = "Navp", target = "NewUser")
  
  
  Logged = FALSE
  
  ############### New user  ################
  
  # When the form data for new USERS 
  
  observeEvent(input$NewUser, {
    hideTab(inputId = "Navp", target = "Login")
    showTab(inputId = "Navp", target = "NewUser")
    updateNavlistPanel(session, "Navp", selected = "NewUser")
  })
  
  
  formData <- reactive({
    
    data <- sapply(NewUserRegistration, function(x) input[[x]])
    data
  })
  
  table <- "USERS"
  
  observeEvent(input$submit, {
    showTab(inputId = "Navp", target = "Login")
    hideTab(inputId = "Navp", target = "NewUser")
    
    saveData(formData())
    updateNavlistPanel(session, "Navp", selected = "Login")
  })
  ####### Save quesry for New user
  
  saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table, 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbExecute(db, query)
    dbDisconnect(db)
  }
  
  
  ############### New Tests  Types ################
  
  # When the form data for new tests 
  
  newTestFormData <- reactive({
    
    newtestdata <- sapply(NewTestTypes, function(x) input[[x]])
    newtestdata
  })
  
  # When the click on Save button in Add test Types in admin page save to TestTypes
  table1 <- "TestTypes"
  
  observeEvent(input$save, {
    
    saveTestData(newTestFormData())
    
  })
  
  
  ####### Save query for New tests
  
  saveTestData <- function(newtestdata) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table1, 
      paste(names(newtestdata), collapse = ", "),
      paste(newtestdata, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  
  ################ New Test Orders    ######################
  
  
  # When the form data for test order
  
  TestOrderFormData <- reactive({
    
    
    orderdata <- sapply(NewTestOrder, function(x) input[[x]])
    
    orderdata$RequestDate <- as.character(orderdata$RequestDate)
    
    if (orderdata$TestName1 == "Glucose") {
      
      orderdata$Test1 <- "Fasting"
      orderdata$Test1Std <- "60 - 100 mg/dL"
      
      orderdata$Test2 <- "Post-2Hrs"
      orderdata$Test2Std <- "120 - 180 mg/dL"
      
    }
    
    if (orderdata$TestName1 == "Lipid Profile") {
      
      orderdata$Test1 <- "Cholesterol"
      orderdata$Test1Std <- "<200 mg/dL"
      
      orderdata$Test2 <- "Triglycerides"
      orderdata$Test2Std <- "<150 mg/dL"
      
      orderdata$Test3 <- "HDL Cholesterol"
      orderdata$Test3Std <- ">40 mg/dL"
      
      orderdata$Test4 <- "LDL Calculated"
      orderdata$Test4Std <- "<130 mg/dL"
    }
    
    orderdata
  })
  
  # When the order button is clicked, save the test order form data to TestResults table
  
  ordertable <- "TestResults"
  
  observeEvent(input$order, {
    
    
    ########### Validate whether user logged in or not###########
    if (input$CustId =="None") {
      
      output$CustInfo <- renderText({"Please login before ordering your tests. Thank you!!!!"})
      return()
    }
    
    saveOrderData(TestOrderFormData())
    
  })
  
  ####### Save query for test order 
  
  saveOrderData <- function(orderdata) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      ordertable, 
      paste(names(orderdata), collapse = ", "),
      paste(orderdata, collapse = "', '")
    )
    ############# Submit the update query and disconnect
    dbExecute(db, query)
    dbDisconnect(db)
    
    ############# Successfully order test message #######################
    ####     updateTextInput(session, "CustId", value = '')
    output$CustInfo <- renderText({"You have successfully placed your tests. Thank you!!!!"})
    return()
    
    
    
  }
  
  ############## END Save Test Orders#######################
  
  
  ################## Cancel or clear ########################
  
  observeEvent(input$cancel, {
    
    updateTextInput(session, "Name", value = '')
    updateTextInput(session, "UserId", value = '')
    updateTextInput(session, "Password", value = '')
  })
  
  ############################# Log in validation ################
  
  USER <- reactiveValues(Logged = Logged)
  
  inputdata <- reactive({
    
    validate(need(isolate(input$userName) == "", "Please Enter User name"))
    
  })
  
  
  
  ############# User Login #####################
  
  observeEvent(input$Login, { 
    
    output$dataInfo <- renderText({""})
    
    ### Check if user already logged in
    
    if (USER$Logged) {
      
      output$dataInfo <- renderText(stop({"You have already logged in!!!!!!"}))
      
      return()
      
    }
    
    #  Check if User Name & Password entered or not
    
    if(input$userName == "" & input$passwd == "") {
      
      output$dataInfo <- renderText({"Please check your credentials"})
      
      return()
    }
    
    if(input$userName == "" ) {
      
      output$dataInfo <- renderText({"Please check your User"})
      return()
    }
    
    if(input$passwd == "") {
      
      output$dataInfo <- renderText({"Please check your password"})
      return()
    }
    
    
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          
          query <- sprintf({"
         SELECT UserId, Role
           FROM USERS
           WHERE UserId ='%s' and Password ='%s'"},
                           Username, Password, serialize=F) 
          
          db <- dbConnect(SQLite(), sqlitePath)
          userrec <- dbGetQuery(db, query) 
          #  print(userrec)
          
          dbDisconnect(db)
          
          
          if (length(userrec$UserId) == 0 ) {
            
            # print error/ warning message
            
            
            output$dataInfo <- renderText({"If you are a new user please register before login OR Check your credentials"})
            return()
            
          } else {
            
            if ( userrec$UserId == Username ) {
              
              USER$Logged <- TRUE}
            
            successInfo <- cbind ("You Have Successfully logged in as", Username)
            
            output$HomeInfo <- renderText({successInfo})
            output$CustInfo <- renderText({successInfo})
            ###     output$AnaInfo <- renderText({successInfo})
            
            output$dataInfo <- renderText({""})   ##### Clear previous message
            
          }
        } 
      }
    }    
    
    
    
    if (USER$Logged == TRUE) 
    {
      ######################### LOAD User Name in Customer tab ###############
      ###################################################
      
      output$CustId <- renderUI({
        selectInput("CustId", "Customer ID", userrec$UserId) })
      
      
      ########### Hide some Tabs when Login #######################
      
      updateTextInput(session, "userName", value = '')
      updateTextInput(session, "passwd", value = '')
      
      if ( userrec$Role == "analyst" ) { 
        
        showTab(inputId = "Navp", target = "Dashboard")
        showTab(inputId = "Navp", target = "Analyst")
        hideTab(inputId = "Navp", target = "NewUser")
        
        hideTab(inputId = "Navp", target = "Login")
        #   hideTab(inputId = "Navp", target = "NewUser")
        hideTab(inputId = "Navp", target = "Customer")
        
        updateNavlistPanel(session, "Navp", selected = "Analyst")
        
        
      }
      if ( userrec$Role == "customer" ) {
        
        showTab(inputId = "Navp", target = "Customer")
        
        hideTab(inputId = "Navp", target = "Dashboard")
        hideTab(inputId = "Navp", target = "Analyst")
        hideTab(inputId = "Navp", target = "Login")
        hideTab(inputId = "Navp", target = "NewUser")
        
        updateNavlistPanel(session, "Navp", selected = "Customer")}
      
    }
  })
  
  ################### Logout logic#####################
  
  observeEvent(input$Logout, { 
    
    
    USER$Logged <- FALSE 
    
    hideTab(inputId = "Navp", target = "Customer")
    hideTab(inputId = "Navp", target = "Analyst")
    showTab(inputId = "Navp", target = "Login")
    hideTab(inputId = "Navp", target = "Dashboard")
    hideTab(inputId = "Navp", target = "NewUser")
    
    updateTextInput(session, "userName", value = '')
    updateTextInput(session, "passwd", value = '')
    
    output$dataInfo <- renderText({""})
    output$HomeInfo <- renderText({"You Have successfully Logged out"})
    output$CustInfo <- renderText({""})
    
    output$CustId <- renderUI({
      selectInput("CustId", "Customer ID", "") })
    
    updateNavlistPanel(session, "Navp", selected = "Login")
    
    
  })
  
  
  #####################Loaddata function 
  
  loadData <- function(fields, table, sortCol= '' , whereCls = ''){
    if (whereCls == "")
      query <- sprintf("SELECT %s FROM %s", fields, table)
    else
      query <- sprintf("SELECT %s FROM %s WHERE %s", fields, table, whereCls)
    db <- dbConnect(SQLite(), sqlitePath)
    dataDB <- dbGetQuery(db, query)
    
    if(sortCol != "") dataDB[order(dataDB[sortCol]),] 
    else dataDB
    dbDisconnect(db)
    
    print(dataDB)
  }
  
  ############Initiallize user if not logged in ###############
  output$CustId <- renderUI({
    selectInput("CustId", "Customer ID", "None") })
  #############################################################
  
  
  ################# load Test Name in Customer tab
  
  Listdata <- loadData("TestName", "TestTypes","TestName","")
  #print(Listdata)
  
  #Listdata<- rbind(data.frame("TestName" = ""), Listdata)
  
  Testnamelist <- setNames(Listdata$TestName, Listdata$TestName)
  
  output$TestName1 <- renderUI({
    selectInput("TestName1", "Test Name: ", Testnamelist)
  })
  
  ####### Lab Locations load data from database
  
  Listdata1 <- loadData("LabLocation", "Location","LabLocation","")
  # print(Listdata1)
  
  LabLoclist <- setNames(Listdata1$LabLocation, Listdata1$LabLocation)
  
  output$LabLocation1 <- renderUI({
    selectInput("LabLocation1", "Lab: ", LabLoclist)
  })
  
  
  
  ################ Data Table start###############################
  ###########################################################
  ###########################################################
  
  observeEvent(input$displayResults, {  
    
    
    db <- dbConnect(SQLite(), sqlitePath)
    
    datatb <- tbl(db, "TestResults")
    
    datatb <- datatb %>% as.data.frame()
    
    
    #TestResults <- filter(datatb,  (is.na(Test1Results) | Test1Results == 0))
    
    TestResults <- datatb
    output$Results <- renderDT(TestResults, options = 
                                 list(scrollX = TRUE), editable = TRUE)
    
    
    #necessary code to replace data once edited
    proxy1 = dataTableProxy('Results')
    
    #print(proxy1)
    
    ####TestResults_rows <- which(datatb$ TestName1 != "")
    TestResults_rows <- which(TestResults$TestName1 != "" | is.na(TestResults$TestName1) )
    
    # print(TestResuts_rows)
    
    
    observeEvent(input$Results_cell_edit, {
      
      info = input$Results_cell_edit
      str(info)
      
      
      i = info$row
      j = info$col
      v = info$value
      
      ############ get new value
      new_value <- DT::coerceValue(v, TestResults[i, j])
      
      ############# update local copy of TestResuts
      TestResults[i, j] <<- new_value
      
      ############# update local copy of data
      
      datatb[TestResults_rows[i], j] <<- new_value
      
      ############# update browser
      replaceData(proxy1, TestResults, resetPaging = TRUE)  # important
      
      ##### dbDisconnect(db)
      
    })
    
    
    observeEvent(input$action, {
      
      dbWriteTable(db, "TestResults", data.frame(datatb), overwrite = TRUE)
      
    })
    
    ### dbDisconnect(db)
    
  })   ########### end of display results
  
  ################ end of Data Table ###############################
  ###########################################################
  ###########################################################
  
  ############   DashBoard ##############################
  
  #dbDisconnect(db)
  db <- dbConnect(SQLite(), sqlitePath)
  
  testresultstabel <- tbl(db, "TestResults")
  
  testresultstabel <- testresultstabel %>% as.data.frame()
  
  ################ initialize ##################
  
  vals <- reactiveValues(MaxTestResultsbyType=NULL, TestResultsPerCustomer = NULL)
  
  
  
  ################ Value 1 ################################
  
  Dashboarddata <- reactive({
    Dashboarddata <- testresultstabel %>% 
      filter(LabLocation1 %in% input$LabLoc) 
    ###   %>% filter(TestName1 %in% input$Ttype) %>%
    ###   filter(gender %in% input$Sex) 
    
    if(is.null(input$Sex))
      return()
    Dashboarddata
  })
  
  output$value1 <- renderValueBox({
    
    valueBox(h4("Total Tests by Location:"), 
             formatC(count(Dashboarddata()), format="d", big.mark=','),
             paste('Total Tests by Location:',count(Dashboarddata()))
             ,icon = icon("stats",lib='glyphicon')
             ,color = "purple") 
    
  })
  
  ################### Value 1 End #######################  
  
  ################ Value 2 ################################
  
  Dashboarddata2 <- reactive({
    Dashboarddata2 <- testresultstabel %>% 
      filter(LabLocation1 %in% input$LabLoc) %>%
      filter(TestName1 %in% input$Ttype) 
    
    if(is.null(input$Ttype))
      return()
    Dashboarddata2
    
    
  })
  
  
  output$value2 <- renderValueBox({
    
    valueBox(h4("Total Tests by Test Type:"),
             formatC(count(Dashboarddata2()), format="d", big.mark=','),
             paste('Total Tests',count(Dashboarddata2()))
             ,icon = icon("stats",lib='glyphicon')
             ,color = "green") 
    
  })
  
  ################### Value 2 End #######################
  
  ################ Value 3 ################################
  
  Dashboarddata3 <- reactive({
    Dashboarddata3 <- testresultstabel %>% 
      filter(LabLocation1 %in% input$LabLoc)  %>%
      filter(TestName1 %in% input$Ttype) %>%
      filter(gender %in% input$Sex) 
    
    if(is.null(input$Sex))
      return()
    Dashboarddata3
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(h4("Total Tests by Gender:"), 
             formatC(count(Dashboarddata3()), format="d", big.mark=','),
             paste('Total Tests by Gender:',count(Dashboarddata3()))
             ,icon = icon("stats",lib='glyphicon')
             ,color = "red") 
    
  })
  
  ################### Value 3 End #######################
  
  ################### Histogram - Max Results by Gender########################
  
  
  #creating the plotOutput content
  output$MaxTestResultsbyType <- renderPlot({
    
    vals$MaxTestResultsbyType <-    ggplot(data =Dashboarddata2(),
                                           aes(x=TestName1, y=Test1Results, fill=factor(gender))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Test Results") + 
      xlab("Test Name") + theme(legend.position="bottom" 
                                ,plot.title = element_text(size=15, face="bold")) + 
      labs(fill = "gender")
    
    vals$MaxTestResultsbyType
    
  })
  
  ###############
  
  
  Dashboarddata4 <- reactive({
    Dashboarddata4 <- testresultstabel %>% 
      filter(LabLocation1 %in% input$LabLoc) %>%
      filter(TestName1 == "Lipid Profile") 
    
    if(is.null(input$Ttype))
      return()
    Dashboarddata4
    
    
  })
  
  
  output$TestResultsPerCustomer <- renderPlot({
    
    ##  vals$TestResultsPerCustomer <-   ggplot(data = testresultstabel, 
    ##        aes(x=CustID, y=Test1Results, fill=factor(TestName1))) + 
    ##   geom_bar(position = "dodge", stat = "identity", colour='yellow') + ylab("Test Results)") + 
    ##   xlab("Customer") + theme(legend.position="bottom"
    ##                          ,plot.title = element_text(size=15, face="bold")) + 
    ##   ggtitle("Test Results by Customer") + labs(fill = "Test Name")
    
    
    
    vals$TestResultsPerCustomer <- ggplot(Dashboarddata4(), 
                                          aes(x = CustID, y = Test1Results,fill=factor(TestName1))) + 
      ###  ggplot(Dashboarddata4(), 
      ###        aes(x = CustID, y = Test1Results,fill=factor(TestName1))) +
      
      geom_point(size = 5, stat = "identity") + ylab("Test Results") + 
      xlab("Customer") + theme(legend.position="bottom"
                               ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Test Results by Customer") + labs(fill = "Test Name")
    
    vals$TestResultsPerCustomer
    
  })
  
  
  ############   End of DashBoard ##############################
  
  
  
  ##############################################################
  ###############  Download Dashboard ##########################
  ##############################################################
  
  
  res <- reactive ({dbGetQuery(db, "SELECT * FROM TestResults where  Test1Results is null")})
  
  
  output$downloadReport <- downloadHandler(
    
    
    
    filename = function() {
      paste('Data_To_Analyze')
    },
    content = function(file) {
      write.csv(res(), file)
      # pdf(file)
      # pdf("test.pdf", height=11, width=10)
      #  grid.table(res)
      #dev.off()
    }
  )
  
  name<-reactive({name<-input$CustId})
  
  #rescust <- reactive ({dbGetQuery(db, "SELECT * FROM TestResults where  CustId = 'jyothi' and Test1Results <> ' ' ")})
  rescust <- reactive ({
    print(input$userName)
    print(input$CustId)
    outp1 <- dbGetQuery(db, sqlInterpolate(ANSI(), 
                                           "SELECT * FROM TestResults where  CustId = ?CustId and Test1Results <> ' '",
                                           CustId = name() ))
  })
  
  
  output$downloadReportCustomer <- downloadHandler(
    
    
    
    filename = function() {
      paste('Results')
    },
    content = function(file) {
      write.csv(rescust(), file)
      # pdf(file)
      # pdf("test.pdf", height=11, width=10)
      #  grid.table(res)
      #dev.off()
    }
  )
  ############   End of download file ##############################
  
  
  ############### Logo on home page #####################
  
  top_dir <- here()
  filename <- paste0(top_dir,'/',"MaryvilleLogo.png")
  output$logo <- renderImage({
    
    
    ##filename <- normalizePath(file.path('C:/Users/genre/OneDrive/Desktop/MS_DataSceince/R',
    #paste('MaryvilleLogo', '.png', sep='')))
    
    
    # Return a list containing the filename
    list(src = filename)
    
    
  },deleteFile = FALSE
  
  
  
  
  )
  ########################  Shiny.io authentication ####################
  
  rsconnect::setAccountInfo(name='kjyothirmayee', 
                            token='DCDF8B4C860A00DFFF173B9462897839', 
                            secret='1rW/yN4Ube47VV0u2eCEjr5Judiu2fXhBSQ7KpCo')
  
} ########################  Shiny.io authentication end ####################


shinyApp(ui = ui, server = server)
