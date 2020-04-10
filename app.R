
# load the required packages
library(shinycssloaders)
library(shiny)
library(tibbletime)
library(shinydashboard)
library(shinyWidgets) ##selection of Branches
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(eeptools)
library(lubridate)
library(data.table)
library(ggvis)
library(stringr)
library(DT)
#library(openair)
#library(flexdashboard)
library(rmarkdown)
library(stringi)
library(purrr)
#library(taRifx)
library(sjPlot)
library(RODBC) 
##########psw#######
library(shinyjs)
library(glue)
library(scales)
#library(devtools)
library(shinyauthr)
library(rAmCharts)
library(plotly)
library(dygraphs)
library(xts)
library(readxl)
library(shinydashboardPlus)
library(digest)
#digest("a", serialize=FALSE)

#################psw################

source("pass.R", local = TRUE)
###########################/ui.R/##################################

#-------------------Load data-------------------------------------------------------

#df <- read_excel("sample.xlsx")

df <- read_csv("sample.csv")

# colnames(df)<- c("CLIENT.CODE","KAM","DISTRICT","CLIENT.TYPE.CODE","CLIENT.TYPE","CLIENT","SUFFIX","INVOICE.CATEGORY","DISCOUNT...9","PRODUCT.CODE","PRODUCT","QUANTITY","FREE.UNITS"
#                  ,"TOTAL QTY","SUPPLIER CODE","SUPPLIER","BRANCH CODE","BRANCH","YEAR","MONTH","FOB TO","REALTO","GTHEOTO","THEOTO","MARGIN","DISCOUNT...26","GLOBDISC","REGCLI" 
#                  ,"RESP BRANCH","TELESALE","NEW.CLASSIFICATION" ,"RESP.SUPPLIER" ,"FOCUS.PRODUCT")


df$MONTH <- as.factor(mapvalues(df$MONTH,
                                from=c(1,2,3,4,5,6,7,8,9,10,11,12),
                                to=c("01-01","02-01","03-01","04-01","05-01","06-01","07-01","08-01","09-01","10-01","11-01","12-01")))
df$YEAR <- ifelse(df$YEAR == "20","2020-","2019-")
df$DATE <- paste(df$YEAR,df$MONTH)
df$DATE <- as.Date(df$DATE)



# Adding image or logo along with the title in the header
header <- dashboardHeader(title = 'Personal | Dashboard',titleWidth = 230,
                          tags$li(a(img(src = 'BK.png',height = "35px",width = "200px",align = "center"),
                                    style = "padding-top:2px; padding-bottom:1px;"),
                                  class = "dropdown"),
                          
                          dropdownMenu(type="message",badgeStatus = "success",
                                       messageItem(from = "Dashboard", message = "Under Construction ")),
                          tags$li(class = "dropdown", style = "padding: 8px;",
                                  shinyauthr::logoutUI("logout")))
##header <- dashboardHeader(title = img(src="BK.png", height = 40, align = "top",style = "padding-top:1px; padding-bottom:3px;"))


#######Select branch option setting######




QQ<-pickerInput("countryInput", "District",choices=unique(df$DISTRICT),multiple = T ,options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                                                                                                                     `selected-text-format`= "static", title = "Select district"),
                choicesOpt = list(
                  style = rep(("color: black; background: white; font-weight: bold;"),94)))



cer<-conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                      tags$img(src="loading_circle.gif")
)


#-------------Here is to disply validation error message in red-------------------------------# 
TT<-fluidPage(
  
  tags$head( 
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
  ),
  
  #--------------------------------END COLOR VALIDATION MESSAGE------------------------------------#
  verbatimTextOutput("server"),
  tags$script('
              $(document).ready(function(){
              var d = new Date();
              var target = $("#clientTime");
              target.val(d.toLocaleString());
              target.trigger("change");
              });
              '),
  textInput("clientTime", "", value = ""),
  verbatimTextOutput("local")
)

#----------------------------------------Date range setting----------------------------------#
ZZ<-dateRangeInput(inputId = "dates",
                   #label = em("Select time period",style="text-align:center;color:#FFA319;font-size:100%"),
                   label = "Select time period:",
                   start = min(df$DATE),
                   end =  max(df$DATE),
                   min =  min(df$DATE),
                   max =  max(df$DATE),
                   format = "yyyy-mm-dd")

#########################Layout setup#######################################################
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  div(sidebarUserPanel(textOutput("welcome1"),subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"))),
  sidebarMenu(
    ZZ,
    QQ,
    menuItem("Home",tabName = "main12", icon = icon("bank")),
    
    
    menuItem("SALES", icon = icon("dashboard"),
             menuSubItem(
               "Overview", tabName = "cust1", icon = icon("table"))),
             
    
    TT
  )
  
)






#-------------------------Setups---------------------------------------------------##



body <- dashboardBody(
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #0B539D;
                            }
                            
                            
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #0B539D;
                            }
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #0B539D;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #f2f3f5;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #ebe8e8;
                            color: #000000;
                            }
                            
                            
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #d9d7d8;
                            }
                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #ebe1e6;
                            }
                            
                            /* body */
                            .content-wrapper, .right-side {
                            background-color:  #ffffff;
                            }
                            
                            '))),tags$style(HTML("

                                                 .box.box-solid.box-primary>.box-header {
                                                 color:#fff;
                                                 background:#0B539D
                                                 }
                                                 
                                                 .box.box-solid.box-primary{
                                                 border-bottom-color:#0B539D;
                                                 border-left-color:#0B539D;
                                                 border-right-color:#0B539D;
                                                 border-top-color:#0B539D;
                                                 
                                                 }
                                                 
                                                 ")),
  shinyjs::useShinyjs(),
  tags$head(tags$style(".table{margin: 0 auto;}"),
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript"),
            includeScript("returnClick.js")
  ),
  shinyauthr::loginUI("login"),
  uiOutput("user_table"),
  HTML('<div data-iframe-height></div>'),
  tabItems(
    tabItem(tabName = "main12", uiOutput("bodyHome")),
    
    tabItem(tabName = "cust1",  uiOutput("frow1"),uiOutput("frow2"))
    
    
    
    
    
    
    
  ))

ui <-dashboardPage(header, sidebar,body, skin='blue'
)


#######create the server functions for the dashboard ####################################

server <- function(input, output, session) {
  ############################################################PASWORD FROM HERE###########
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  
  user_info <- reactive({credentials()$info})
  
  user_data <- reactive({
    req(credentials()$user_auth)
    
    if (user_info()$permissions == "admin") {
      dplyr::starwars[,1:10]
    } else if (user_info()$permissions == "standard") {
      dplyr::storms[,1:11]
    }
    
  })
  
  output$welcome1 <- renderText({
    req(credentials()$user_auth)
    
    glue("Welcome {user_info()$name}")
  })
  #-------------------------------------data setup----------------------------------------
  
  DATA1 <- reactive({
    validate(
      need(input$countryInput!= "", message="Please select Branch(es)")
    )
    df %>%filter(DATE >= input$dates[1], DATE<= input$dates[2],DISTRICT%in% input$countryInput)})
  
  
  
  
  
  
  
  
  ############################BODYHOME #############################################
  output$bodyHome<- renderUI({
    req(credentials()$user_auth)
    tabItem(tabName = "main12", value="main_panel",
            fluidRow(
              box(
                title = HTML("<font color=\"#00b3FF\"><b>Welcome to the Samuel Mutindas Dashboard</b></font>"), width = 12, status = "primary",
                
                HTML("The availability of a <b>dashboard for acess trending Business</b> and <b>operational transactions/records </b> will play a significant role in providing at-a-glance views of the <i>Samuel Mutindas's KPIs </i> and provide insights through automated analytics and reports.
                     ")
              )
            
              ),
          
            fluidRow(
              box(
                title = "Samuel Mutinda  (Samuel Mutindaike@gmail.com) @ 2020", width = 12, status = "success"
                #h4("This dashboard is designed by a Data scientist under management department.")
                
              )
            )
    )
  })
  
  #--------------------------- outputs----------------------------------------------------
  
  
  valueBox2 <- function (value, subtitle, icon = NULL, backgroundColor = "", textColor = "#e3972f", width = 4, href = NULL)
  {
    
    boxContent <- div(
      class = paste0("small-box"),
      style = paste0("background-color: ", backgroundColor, "; color: ", textColor, ";"),
      div(
        class = "inner",
        h1(p(span(subtitle, style = "color:#0B539D"), style = "font-size: 50%;")),
        
        
        
        
        
        h2(value)
        
      ),
      if (!is.null(icon)) {
        div(class = "icon-large", icon)
      }
    )
    if (!is.null(href)) {
      boxContent <- a(href = href, boxContent)
    }
    div(
      class = if (!is.null(width)) paste0("col-sm-", width),
      boxContent
    )
  }
  
  
  
  ########----------------------------------dashboard building starts here--------------------------------------------------
  output$frow1<- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      valueBoxOutput("box1",width = 4),
      valueBoxOutput("box2",width = 4),
      valueBoxOutput("box3",width = 4)
      
      
    )
    
  })
  
  
  output$box1 <- renderValueBox({
    out <- DATA1()%>%
      group_by(Customers =CLIENT.CODE ) %>%
      dplyr::summarise(No_of_transaction = n(),Amount = sum(MARGIN))
    valueBox2(
      format(round(sum(out$No_of_transaction), 0), big.mark = ","),
      subtitle = "Customers served",
      
      backgroundColor = ""
    )
  })
  
  output$box2 <- renderValueBox({
    out <- DATA1()%>%
      group_by(Customers =CLIENT.CODE ) %>%
      dplyr::summarise(No_of_transaction = n(),Amount = sum(MARGIN))
    valueBox2(
      format(round(sum(out$Amount), 0), big.mark = ","),
      subtitle = "Total Margin",
      
      backgroundColor = ""
    )
  })
  
  output$frow2<- renderUI({
    req(credentials()$user_auth)
    fluidRow(
      # p("The graph tracks the number of loans on each type of customer"),
      box(title = 'Volume of transaction over customer type', width = 6, height = 310,  status = "primary", 
          solidHeader = TRUE, collapsible = TRUE,plotlyOutput("plot1", height = 240)),
      
      box(title = 'Margin of transaction over customer type', width = 6,height = 310,  status = "primary", 
          solidHeader = TRUE, collapsible = TRUE,plotlyOutput("plot2",height = 240)),
      
      box(title = 'Volume of sales over time', width = 6,  status = "primary", 
          solidHeader = TRUE, collapsible = TRUE,plotlyOutput("plot3"),style = "height:500px;"),
      box(title = 'Margin of transaction over time', width = 6,  status = "primary", 
          solidHeader = TRUE, collapsible = TRUE,plotlyOutput("plot4"),style = "height:500px;")
  
    )})
  
  
  
  
  output$plot1 <- renderPlotly({
    req(credentials()$user_auth)
    
    out <- DATA1() %>%
      #dplyr::filter(Product == "BK")%>%
      group_by(group =CLIENT.TYPE ) %>%
      dplyr::summarise(No_of_transaction = n(),Amount = sum(MARGIN))
    
    
    #dodod<-data.frame(table(friday32()$AMOUNT_RESP1))
    plot_ly(out, labels = ~group, values = ~No_of_transaction, type = 'pie', marker = list(colors = 'rgb(55, 83, 109)'))%>%config(displayModeBar = FALSE)
    
  })
  
  output$plot2 <- renderPlotly({
    req(credentials()$user_auth)
    
    out <- DATA1() %>%
      #dplyr::filter(Product == "BK")%>%
      group_by(group =CLIENT.TYPE ) %>%
      dplyr::summarise(No_of_transaction = n(),Amount = sum(MARGIN))
    
    
    #dodod<-data.frame(table(friday32()$AMOUNT_RESP1))
    plot_ly(out, labels = ~group, values = ~Amount, type = 'pie', marker = list(colors = 'rgb(55, 83, 109)'))%>%config(displayModeBar = FALSE)
    
  })
 
  
  output$plot3 <- renderPlotly({
    req(credentials()$user_auth)
    Loans <-DATA1()
    Loans$DATE <- as.numeric(format(Loans$DATE,'%Y'))
    N <- subset(Loans, DATE >= "2013-01-01")
    data <- N %>%
      group_by(DATE,CLIENT.TYPE) %>%
      dplyr::summarise(number = sum(MARGIN))%>%
      tidyr::spread(value = number, key= CLIENT.TYPE , fill = 0)
    
    data.table::melt(data, id.vars='DATE') %>%
      plot_ly(x = ~DATE, y = ~value, type = 'bar',
              name = ~variable,color = ~variable, marker = list(color = 'rainbow')) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')%>%
      layout(legend = list(x = 0.1, y = -1.3))%>%
      config(displaylogo = FALSE)
    
    
  })
  
  output$plot4 <- renderPlotly({
    req(credentials()$user_auth)
    Loans <-DATA1()
    Loans$DATE <- as.numeric(format(Loans$DATE,'%Y'))
    N <- subset(Loans, DATE >= "2013-01-01")
    data <- N %>%
      group_by(DATE,CLIENT.TYPE) %>%
      dplyr::summarise(number = sum(MARGIN))%>%
      tidyr::spread(value = number, key= CLIENT.TYPE , fill = 0)
    
    data.table::melt(data, id.vars='DATE') %>%
      plot_ly(x = ~DATE, y = ~value, type = 'bar',
              name = ~variable,color = ~variable, marker = list(color = 'rainbow')) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')%>%
      layout(legend = list(x = 0.1, y = -1.3))%>%
      config(displaylogo = FALSE)
    
    
  })
  
  
  
   
  
}

shinyApp(ui, server)