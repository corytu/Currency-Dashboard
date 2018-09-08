if (!file.exists("data/last_crawled_time.R")) {
  last_crawled <- Sys.time() - as.difftime(1, units = "days")
} else {
  source("data/last_crawled_time.R")
}

if (Sys.time() - last_crawled > as.difftime(30, units = "mins")) {
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
  
  # Save needed data and time of last crawling
  save(list = c("currencies", "interests"), file = "data/latest_data.RData")
  last_crawled <- Sys.time()
  dump("last_crawled", "data/last_crawled_time.R")
}