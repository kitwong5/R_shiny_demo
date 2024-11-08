library(shiny)
library(plotly)
library(ggplot2) 
library(RColorBrewer) 
library(readr)
library(tidyr) 
library(dplyr) 
library(lubridate)


# Import S&P500 companies csv file
comp_df <- data.frame(read.csv( "Data/sp500_companies_20240517.csv", header=TRUE))
# order by symbol
comp_df <- comp_df[order(comp_df$Symbol, decreasing = FALSE), ]
# Import S&P500 companies stock price file
stock_df <- data.frame(read.csv( "Data/sp500_stocks_20240517.csv", header=TRUE))
stock_df$Date <- as.Date(stock_df$Date) 
# Import S&P500 companies index price file
index_df <- data.frame(read.csv( "Data/sp500_index_20240517.csv", header=TRUE))
index_df$Date <- as.Date(index_df$Date) 
index_df <- mutate(index_df, Symbol = "INX")
# Adjust value unit
comp_df$Ebitda <- comp_df$Ebitda/1000000000
comp_df$Marketcap <- comp_df$Marketcap/1000000000


ui <- fluidPage(
#ui <- fixedPage(
  tabsetPanel( id = "tabset",
    # Tab 1 #           
    tabPanel("S&P500 Sector Composition", 
             tabPanel("Sector Composition", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(width=3,
                            selectInput("sector", "Select Sector", 
                                        choices = c("ALL","Basic Materials",
                                        "Communication Services",
                                        "Consumer Cyclical",
                                        "Consumer Defensive",
                                        "Energy",
                                        "Financial Services",
                                        "Healthcare",
                                        "Industrials",
                                        "Real Estate",
                                        "Technology",
                                        "Utilities"), selected = "ALL")
                                     ),
                        mainPanel(
                          p(" "),
                          plotlyOutput("sector_plot"),
                          p(" "),
                          span(textOutput('ref_text1'), style="font-style:italic;font-size:9px")
                        )
                      )
             )
    ),
    # Tab 2 #
    tabPanel("S&P500 Sector Details",
             tabPanel("Sector Details", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     selectInput("sd_sector", "Select Sector", 
                                                 choices = c("ALL","Basic Materials",
                                                             "Communication Services",
                                                             "Consumer Cyclical",
                                                             "Consumer Defensive",
                                                             "Energy",
                                                             "Financial Services",
                                                             "Healthcare",
                                                             "Industrials",
                                                             "Real Estate",
                                                             "Technology",
                                                             "Utilities"), selected = "ALL"),
                                     selectInput("sd_rpt_period", "Select Return Period", 
                                                 choices = c("Last 1 mth"= "1",
                                                             "Last 2 mths"= "2",
                                                             "Last 3 mths"= "3",
                                                             "Last 4 mths"= "4",
                                                             "Last 5 mths"= "5",
                                                             "Last 6 mths"= "6",
                                                             "Last 7 mths"= "7",
                                                             "Last 8 mths"= "8",
                                                             "Last 9 mths"= "9",
                                                             "Last 10 mths"= "10",
                                                             "Last 11 mths"= "11",
                                                             "Last 12 mths"= "12"), selected = "12"),
                                     conditionalPanel(
                                       condition = "input.sd_sector != 'ALL'",
                                       selectInput("x_name", "Select Report x-asix", 
                                                   choices = c("Ebitda" = "Ebitda",
                                                               "Full Time Employees" = "Fulltimeemployees",
                                                               "Market Cap" = "Marketcap",
                                                               "Revenue Growth Rate" = "Revenuegrowth"), selected = "Revenuegrowth"),
                                       selectInput("y_name", "Select Report y-asix", 
                                                   choices = c("Ebitda" = "Ebitda",
                                                               "Full Time Employees" = "Fulltimeemployees",
                                                               "Market Cap" = "Marketcap",
                                                               "Revenue Growth Rate" = "Revenuegrowth"), selected = "Ebitda"),
                                    )
                        ),
                        mainPanel(
                          p(" "),
                          plotlyOutput("sector_det_plot"),
                          p(" "),
                          span(textOutput('ref_text2'), style="font-style:italic;font-size:9px")
                        )
                    )
             )
    ),
    # Tab 3 #
    tabPanel("S&P500 Individual Stock performance",
             tabPanel("Stock performance", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("slider", "Choose Date Range:", min= as.Date("2023-05-17"), max=as.Date("2024-05-17"), value=c(as.Date("2023-05-17"), as.Date("2024-05-17")), timeFormat="%b %Y"),
                          selectInput("stk_sector", "Select Sector to Add Stock(s) to plot", 
                                      choices = c("ALL","Basic Materials",
                                                  "Communication Services",
                                                  "Consumer Cyclical",
                                                  "Consumer Defensive",
                                                  "Energy",
                                                  "Financial Services",
                                                  "Healthcare",
                                                  "Industrials",
                                                  "Real Estate",
                                                  "Technology",
                                                  "Utilities"), selected = "ALL"),
                          selectInput("stk_industry", "Select Industry to Add Stock(s) to plot", 
                                        choices =  NULL, selected = "ALL"),
                          selectizeInput("stk_symbol", "Select Stock Symbol(s) to plot (max = 5)", 
                                      multiple = TRUE,
                                      choices =  comp_df$Symbol, 
                                      selected = FALSE,
                                      options = list(maxItems = 5)),
                          conditionalPanel(
                            condition = "input.stk_symbol.length > 0",
                            radioButtons("index_show","Show Index?",
                                         choiceNames = c("Yes","No"),
                                         choiceValues = c("Y","N")
                                         )
                          )
                                     ),
                        mainPanel(
                          p(" "),
                          plotlyOutput("stk_plot"),
                          p(" "),
                          span(textOutput('ref_text3'), style="font-style:italic;font-size:9px")
                        )
                      )
             )
    )
  )
)
server <- function(input, output, session) {
  

  # prepare fill color
  num_colors <- 20
  palette_colors <- colorRampPalette(brewer.pal(8, "Set2"))(num_colors)
  
  ############################################
  ## Tab 1: Generate Sector Composition Plot #
  ############################################
  output$sector_plot <- renderPlotly({
    if (input$sector == "ALL"){
      # prepare Sector plot
      comp_df1 <- comp_df %>% count(Sector) %>% mutate(pct = n / sum(n))
      p<-ggplot(comp_df1, aes(x=reorder(Sector,n), y=n, text=paste("Count:",n), fill = Sector, label = scales::percent(pct))) +
        geom_bar(stat="identity", position = 'dodge')+
        scale_fill_manual(values = palette_colors) +
        coord_flip() +
        labs(x = "Sector", y = "No. of Stocks", title="S&P500 - Sector Breakdown") +
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
              legend.text = element_text(size=7)
              ) +
        geom_text(position = position_dodge(width = 1),hjust = -0.1, size = 2.4,inherit.aes = TRUE) 
      ggplotly(p, tooltip = "text")
    } else{
      # filter dataset according to input selection
      comp_df <- comp_df %>% filter(Sector == input$sector)
      # prepare Industry plot
      comp_df1 <- comp_df %>% count(Industry) %>% mutate(pct = n / sum(n))
      p<-ggplot(comp_df1, aes(x=reorder(Industry,n), y=n, text=paste("Count:",n),fill = Industry, label = scales::percent(pct))) +
        geom_bar(stat="identity", position = 'dodge')+
        scale_fill_manual(values = palette_colors) +
        coord_flip() +
        labs(x = "Industry", y = "No. of Stocks", title=paste("S&P500",input$sector,"Sector - Industry Breakdown")) +
        theme(plot.margin = margin(0, 0, 0, 0, "cm"),
              legend.text = element_text(size=7)) +
        geom_text(position = position_dodge(width = 1),hjust = -0.1, size = 2.4,inherit.aes = TRUE) 
      ggplotly(p, tooltip = "text")
    }
  })
  

  ##########################################
  ## Tab2: Generate Sector Details Plot    #
  ##########################################
  # Filter Tab2 Report Period based on selected Sector input

  
  output$sector_det_plot <- renderPlotly({  
    
    # define return period
    asOfDate = as.Date("2024-05-17") # the data image as of date
    # get input report period
    from_mth <- as.numeric(input$sd_rpt_period)
    fromAsOfDate = asOfDate %m-% months(from_mth)  
    fromAsOfDateSk <- max(stock_df[(stock_df$Date <= fromAsOfDate), "Date"]) 
    # prepare return snapshot according to input date range
    stock_df_from <- stock_df %>% filter(Date == fromAsOfDateSk)
    stock_df_to <- stock_df %>% filter(Date == asOfDate)
    # Merge the stock price snapshots for YTD or MTD return calculation
    stock_df_MTD = merge(stock_df_from, stock_df_to, by="Symbol")
    # YTD or MTD return = ((Current value - Beginning value) / Beginning value) * 100
    stock_df_MTD <- mutate(stock_df_MTD, 
                           Return = ((stock_df_MTD$Adj.Close.y - stock_df_MTD$Adj.Close.x)
                                     /stock_df_MTD$Adj.Close.x))
    det_df = merge(subset(comp_df, select = c(Symbol, Shortname, Sector, Industry, Revenuegrowth, Ebitda, Marketcap, Currentprice, Fulltimeemployees)), 
                   subset(stock_df_MTD, select = c(Symbol, Return)), by="Symbol")
    det_df <- det_df %>% mutate(Return_group = ifelse(Return > 0, "Profit", "Loss"))
    # drop missing value (stocks add to index after the period)
    det_df[(is.na(det_df$Return_group) | det_df$Return_group==""), 'Symbol' ]
    det_df = det_df[!det_df$Symbol %in% c("SOLV","GEV","VLTO") ,]
    
    if (input$sd_sector == "ALL"){
      tit_lab <- paste(paste("S&P500 Individual Stocks' MTD Return by Sector (Last",input$sd_rpt_period), "mths)") 
      
      p <- ggplot(det_df, aes(x=Sector, fill = Return_group)) +
                  geom_bar()+
                  scale_fill_manual(name="Return", values=c("violetred2", "royalblue2"))+
                  theme(axis.text.x = element_text(angle = 30, vjust = 0.4, hjust=0.4))+
                  labs(x = "Sector", y = "No. of Stocks", title=tit_lab) 
      ggplotly(p)
    } else {
      # filter dataset according to input selection
      det_df <- det_df %>% filter(Sector == input$sd_sector)
      # map x-axis and y-axis name
      if(input$x_name == "Revenuegrowth") {
        x_title = "Revenue Growth Rate"
      } else if (input$x_name == "Marketcap") {
        x_title = "Market Cap"
      } else if (input$x_name == "Fulltimeemployees") {
        x_title = "Full Time Employees"
      } else {
        x_title = input$x_name
      }
      if(input$y_name == "Revenuegrowth") {
        y_title = "Revenue Growth Rate"
      } else if (input$y_name == "Marketcap") {
        y_title = "Market Cap"
      } else if (input$y_name == "Fulltimeemployees") {
        y_title = "Full Time Employees"
      } else {
        y_title = input$y_name
      }
      if (input$x_name == "Marketcap"||input$x_name == "Ebitda"){
        x_la_name = paste(x_title, " (in 1000 millions)")
      } else {
        x_la_name = x_title
      }
      if (input$y_name == "Marketcap"||input$y_name == "Ebitda"){
        y_la_name = paste(y_title, " (in 1000 millions)")
      } else {
        y_la_name = y_title
      }
      title_lab <- paste("S&P500 Individual Stocks' MTD Return (Last",from_mth, "mths)") 
      
      # display scatter plot
      p <-  ggplot(det_df, aes_string(x = input$x_name, y = input$y_name)) +
                  aes(text = paste("Symbol:",Symbol,"<br>Company:",Shortname,"<br>Return:",round(Return,2)*100,"%"))+
                  geom_point(aes(color = Return_group))+
                  scale_color_manual(name="Return", values = c("violetred2", "royalblue2"))+
                  labs(x=x_la_name, y=y_la_name, title=title_lab) 
      
      ggplotly(p, tooltip = "text")
    }
    
  })

  
  ####################################
  ## Tab3: Generate Price Plot       #
  ####################################
  
  # Filter data based on selected data range
  filteredData <- reactive({
    if (is.null(input$stk_symbol)){
      subset(index_df, Date >= sliderMonth$Month[1] & Date <= sliderMonth$Month[2])
    } else {
      if (input$index_show == "Y"){
        filter_stock_df <- filter(stock_df, Symbol %in% input$stk_symbol)
        index_stock_df <- full_join(index_df, filter_stock_df[c('Date', 'Symbol', 'Close')])
        subset(index_stock_df, Date >= sliderMonth$Month[1] & Date <= sliderMonth$Month[2])
      } else {
        filter_stock_df <- filter(stock_df, Symbol %in% input$stk_symbol)
        subset(filter_stock_df, Date >= sliderMonth$Month[1] & Date <= sliderMonth$Month[2])
      }
    }
  })
  # Filter companies dataset based on selected Sector input
  com_filter <- reactive({
    req(input$stk_sector)
    filter(comp_df, Sector == input$stk_sector)
  })
  # Filter stock dataset based on selected Sector input
  sym_filter <- reactive({
    req(input$stk_industry)
    if (input$stk_sector == "ALL"){
      comp_df
    } else{
      filter(comp_df, Industry == input$stk_industry)
    }  
  })
  # Update industry select box
  observeEvent(com_filter(), {
    updateSelectInput(session, "stk_industry", choices = unique(com_filter()$Industry))  
  })
  # Update symbol select box
  observeEvent(sym_filter(), {
    updateSelectizeInput(session, "stk_symbol", choices = unique(sym_filter()$Symbol))
  })

  output$stk_plot <- renderPlotly({
    
    # filter dataset base on selected data range
    index_plot_df <- filteredData()
    # generate index price plot
    if (is.null(input$stk_symbol)){
      title_lb = "S&P500 Index (INX) Performance"
    } else {
      if (input$index_show == "Y"){
        title_lb = "S&P500 Index (INX) & selected Stock(s) Performance"
      } else {
        title_lb = "Selected Stock(s) Performance"
      }  
    }
    
    p <- ggplot(index_plot_df, aes(x=Date, y=Close)) +
      geom_line(aes(colour=Symbol,  group=Symbol, linetype = Symbol %in% c("INX") )) +
      scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%b %Y") +
      aes(text = paste("Date:",Date,"<br>Price:",round(Close,2),"<br>Symbol:",Symbol))+
      labs(x = "Date", y = "Close Price", title=title_lb) +
      scale_linetype_manual(values = c("TRUE" = "dotted", "FALSE" = "solid")) +
      guides(linetype = FALSE)+
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
    ggplotly(p, tooltip = "text")
  })
  
  output$ref_text1 <- renderText({ "Data Source: Larxel. (20 May 2004). S&P 500 Stocks (daily updated). Kaggle. https://www.kaggle.com/datasets/andrewmvd/sp-500-stocks/data" })
  output$ref_text2 <- renderText({ "Data Source: Larxel. (20 May 2004). S&P 500 Stocks (daily updated). Kaggle. https://www.kaggle.com/datasets/andrewmvd/sp-500-stocks/data" })
  output$ref_text3 <- renderText({ "Data Source: Larxel. (20 May 2004). S&P 500 Stocks (daily updated). Kaggle. https://www.kaggle.com/datasets/andrewmvd/sp-500-stocks/data" })
#    output$test <- renderText({
#    paste("X Name: ", input$x_name)
#  })
  
#  output$test2 <- renderText({
#    paste("Y name: ", input$y_name)
#  })

  monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
  }
  sliderMonth <- reactiveValues()
  observe({
    full.date <- as.POSIXct(input$slider, tz="GMT")
    sliderMonth$Month <- as.character(monthStart(full.date))
  })
}

# Run the applicaton
shinyApp(ui, server)