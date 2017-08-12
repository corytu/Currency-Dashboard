# Currency-Dashboard
Shiny dashboard showing the latest currency information and calculating the ROI

## 主要功能（R爬蟲）
1. 以互動式圖表呈現近三個月的匯率資料
2. 以互動式圖呈現外幣存款牌告利率
3. 比較當日與前一週的即期賣出價格
4. 如果手上已經持有外幣，可以計算現在脫手的投資報酬率

## 重要使用套件
1. httr、rvest：抓取網頁資料並轉為R資料格式
2. plotly、ggplot2：互動式視覺化
3. shiny、shinydashboard：互動式介面

## 爬蟲過程與使用函數
以美金近三個月匯率為例：

```r
# 複製美金匯率網址
cur.url <- "http://rate.bot.com.tw/xrt/quote/ltm/USD"

# 以下coding有使用pipe operator
# GET the information from the webpage
currency <- httr::GET(cur.url) %>%
    # Retrieve the content of the request as text 
    httr::content("text", encoding = "UTF-8") %>%
    # Read html
    xml2::read_html(encoding = "UTF-8") %>%
    # Extract html element (table in this case)
    rvest::html_nodes(xpath = "//table") %>%
    # Parse the html table into a data frame
    # The default webpage headers are messed up
    rvest::html_table(header = FALSE, fill = TRUE) %>%
    # . means this item
    .[[1]] %>%
    .[-(1:2), -(7:9)]
```
