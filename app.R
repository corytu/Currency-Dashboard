library(shiny)
library(shinydashboard)
library(httr)
library(rvest)
library(magrittr)
library(reshape2)
library(ggplot2)
library(plotly)

# ------ Get currency data
# Take the urls and make sure their retrieving methods (all GET here)
cur.url <- list(usd = "http://rate.bot.com.tw/xrt/quote/ltm/USD",
                aud = "http://rate.bot.com.tw/xrt/quote/ltm/AUD",
                zar = "http://rate.bot.com.tw/xrt/quote/ltm/ZAR",
                jpy = "http://rate.bot.com.tw/xrt/quote/ltm/JPY",
                eur = "http://rate.bot.com.tw/xrt/quote/ltm/EUR",
                cny = "http://rate.bot.com.tw/xrt/quote/ltm/CNY")

GET.cur <- function(site) {
  # site is an url
  # I don't want to use "url" because its a built-in function
  currency <- GET(site) %>%
    content("text", encoding = "UTF-8") %>%
    read_html(encoding = "UTF-8") %>%
    html_nodes(xpath = "//table") %>%
    # The default webpage headers are messed up
    html_table(header = FALSE, fill = TRUE) %>%
    # . means this item
    .[[1]] %>%
    .[-(1:2), -(7:9)]
  column.names <- c("掛牌日期", "幣別", "現金買入", "現金賣出",
                    "即期買入", "即期賣出")
  names(currency) <- column.names
  currency[,1] <- as.Date(currency[,1], "%Y/%m/%d")
  currency[,3:6] <- apply(currency[,3:6], 2, as.numeric)
  currency <- melt(currency, measure.vars = 3:6,
                   variable.name = "交易種類", value.name = "匯率")
  return(currency[order(currency$掛牌日期),])
}

currencies <- lapply(cur.url, GET.cur)
currencies$zar$匯率[currencies$zar$匯率 == 0] <- NA

# ------ Get interest data
interests <- GET("http://rate.bot.com.tw/ir?Lang=zh-TW") %>%
  content(as = "text", encoding = "UTF-8") %>%
  read_html(encoding = "UTF-8") %>%
  html_nodes(xpath = "//table") %>%
  html_table(header = FALSE, fill = TRUE) %>%
  .[[1]] %>%
  .[-c(1,2,4), -(11:12)]
names(interests) <- c("幣別", "活期", "定期7天", "定期14天", "定期21天",
                      "定期1月", "定期3月", "定期6月", "定期9月", "定期1年")
interests[,-1] <- apply(interests[-1], 2, as.numeric)
interests <- melt(interests, id.vars = "幣別",
                  variable.name = "存款類型", value.name = "年息") %>%
  .[order(.$幣別),]

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
      HTML("系統建置：涂玉臻<br>建置時間：2017年7月<br>在<a href=\"https://github.com/corytu/CurrencyDashboard\">GitHub</a>上查看原始碼")
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