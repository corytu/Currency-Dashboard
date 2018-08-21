library(shiny)
library(shinydashboard)
library(magrittr)
library(ggplot2)
library(plotly)
library(httr)
library(rvest)
library(reshape2)
moneytype <- c("美金 (USD)", "澳幣 (AUD)", "南非幣 (ZAR)",
               "日圓 (JPY)", "歐元 (EUR)", "人民幣 (CNY)")

ui <- dashboardPage(
  dashboardHeader(title = "臺銀外匯資料"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("moneytype", "請選擇幣別：", moneytype),
      textInput("bought", "已購買外幣總額："),
      textInput("old.cur", "購買時匯率（NTD）："),
      textInput("int.gain", "已獲得外幣利息總額：", "0"),
      menuItem("營業時間牌告匯率（近三個月）", tabName = "currency"),
      menuItem("存款牌告利率（不含大額美金）", tabName = "interest"),
      br(),
      HTML("系統建置：涂玉臻<br>建置時間：2017年7月<br>在<a href=\"https://github.com/corytu/CurrencyDashboard\">GitHub</a>上查看原始碼"),
      br(),
      tags$i("最新資料請以臺銀官網為準")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "currency",
        fluidRow(
          valueBoxOutput("trend"),
          valueBoxOutput("reward")
        ),
        fluidRow(plotlyOutput("cur.plot")),
        br(), br(),
        fluidRow(dataTableOutput("cur.table"))
      ),
      tabItem(
        "interest",
        fluidRow(plotlyOutput("int.plot"))
      )
    )
  )
)

server <- function(input, output) {
  # Source the crawler and load the data
  source("data/crawler.R")
  load("data/latest_data.RData")
  
  # Selected money type (e.g. USD)
  target.cur <- reactive({
    currencies[[match(input$moneytype, moneytype)]]
  })
  target.int <- reactive({
    subset(interests, 幣別 == input$moneytype)
  })
  
  my.text <- reactive({
    paste0("交易種類：", target.cur()$交易種類, "\n",
           "掛牌日期：", strftime(target.cur()$掛牌日期, "%Y-%m-%d"), "\n",
           "匯率：", target.cur()$匯率)
  })
  
  output$cur.plot <- renderPlotly({
    g <- ggplot(target.cur(),
                aes(掛牌日期, 匯率)) +
      geom_line(aes(color = 交易種類)) +
      scale_x_date(date_labels = "%Y-%m-%d") +
      scale_color_manual(values = c("#dfc27d", "#a6611a", "#80cdc1", "#018571")) +
      labs(y = "匯率（NTD）")
    p <- plotly_build(g) %>%
      # The "text" will be apply on all traces (factors)
      # If traces = 1:4, my.text()[1] goes to all the first data points of the four lines
      # The numbers of traces are the same with the coding of factors
      style(text = my.text()[seq(1, length(my.text()), by = 4)],
            hoverinfo = "text", traces = 1) %>%
      style(text = my.text()[seq(2, length(my.text()), by = 4)],
            hoverinfo = "text", traces = 2) %>%
      style(text = my.text()[seq(3, length(my.text()), by = 4)],
          hoverinfo = "text", traces = 3) %>%
      style(text = my.text()[seq(4, length(my.text()), by = 4)],
            hoverinfo = "text", traces = 4)
    p
  })
  
  output$int.plot <- renderPlotly({
    g <- ggplot(target.int(), aes(存款類型, 年息)) + geom_col() +
      labs(y = "年息（%）")
    ggplotly(g)
  })
  
  output$cur.table <- renderDataTable({target.cur()})
  
  output$trend <- renderValueBox({
    buy <- subset(target.cur(), 交易種類 == "即期賣出")
    lastday <- buy$掛牌日期[nrow(buy)]
    comparedday <- lastday - 7
    changevalue <- round((buy$匯率[nrow(buy)] - buy$匯率[buy$掛牌日期 == comparedday])/
      buy$匯率[buy$掛牌日期 == comparedday] * 100, 2)
    valueBox(
      value = paste0(changevalue, "%"),
      subtitle = "即期賣出本日與上週比較",
      color = if (changevalue >= 0) {"green"} else {"red"},
      icon = icon("line-chart")
    )
  })
  
  output$reward <- renderValueBox({
    sell <- subset(target.cur(), 交易種類 == "即期買入")
    total.cost <- as.numeric(input$bought)*as.numeric(input$old.cur)
    total.back <- (as.numeric(input$bought)+as.numeric(input$int.gain))*sell$匯率[nrow(sell)]
    reward.pro <- round((total.back - total.cost)/total.cost * 100, 1)
    decision <- if (is.na(reward.pro) | reward.pro >= 0) {"thumbs-o-up"} else {"thumbs-o-down"}
    valueBox(
      value = if (is.na(reward.pro)) {NA} else {paste0(reward.pro, "%")},
      subtitle = "以即期買入價脫手之報酬率",
      color = if (is.na(reward.pro) | reward.pro >= 0) {"green"} else {"red"},
      icon = icon(decision)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)