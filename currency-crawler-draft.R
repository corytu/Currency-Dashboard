library(httr)
library(rvest)
library(magrittr)
library(ggplot2)
library(plotly)

setwd("Dropbox/GitHub/Currency/")

# Take the urls and make sure their retrieving methods (all GET here)
url.list <- list(usd = "http://rate.bot.com.tw/xrt/quote/ltm/USD",
                 aud = "http://rate.bot.com.tw/xrt/quote/ltm/AUD",
                 zar = "http://rate.bot.com.tw/xrt/quote/ltm/ZAR")
int.url <- "http://rate.bot.com.tw/ir?Lang=zh-TW"
# url.usd <- "http://rate.bot.com.tw/xrt/quote/ltm/USD"

# GET data
# Make sure that the data are retrieved successfully
# Retrieve the data table
GET.table <- function(site) {
  # site is an url
  # I don't want to use "url" because its a built-in function
  currency <- GET(site) %>%
    content("text", encoding = "UTF-8") %>%
    read_html(encoding = "UTF-8") %>%
    html_nodes(xpath = "//table") %>%
    # The default webpage headers are messed up
    html_table(header = FALSE, fill = TRUE) %>%
    .[[1]] %>%
    .[-(1:2), -(7:9)]
  column.names <- c("掛牌日期", "幣別", "現金買入", "現金賣出",
                    "即期買入", "即期賣出")
  names(currency) <- column.names
  currency[,1] <- as.Date(currency[,1], "%Y/%m/%d")
  currency[,3:6] <- apply(currency[,3:6], 2, as.numeric)
  return(currency)
}
interests <- GET("http://rate.bot.com.tw/ir?Lang=zh-TW") %>%
  content(as = "text", encoding = "UTF-8") %>%
  read_html(encoding = "UTF-8") %>%
  html_nodes(xpath = "//table") %>%
  # The default webpage headers are messed up
  html_table(header = FALSE, fill = TRUE) %>%
  .[[1]]

currencies <- lapply(url.list, GET.table)

# ggplotly has a great disadvantage here
# The hovertext of x data will show exact days after 1970-01-01
# g <- ggplot(currencies[[1]], aes(掛牌日期, 現金買入))
# g + geom_line() + scale_x_date(date_labels = "%Y/%m/%d")
# ggplotly(g + geom_line() + scale_x_date(date_labels = "%Y/%m/%d"))
# So instead we take the ggplot2 object to build another plotly object
# and style it as the following shows:

g <- ggplot(currencies[[1]], aes(掛牌日期, 現金買入)) +
  geom_line() + scale_x_date(date_labels = "%Y/%m/%d")
p <- plotly_build(g)
my.text <- paste0("掛牌日期：", rev(strftime(currencies[[1]]$掛牌日期, "%Y/%m/%d")),
                  "\n", "現金買入：", rev(currencies[[1]]$現金買入))
style(p, text = my.text, hoverinfo = "text")


# ------ Single example draft
# usd <- GET(url.usd)
# print(usd)
# usd.text <- content(usd, "text", encoding = "UTF-8")
# usd.table <- read_html(usd.text, encoding = "UTF-8") %>%
#   html_nodes(xpath = "//table") %>%
#   html_table(header = FALSE, fill = TRUE) %>%
#   .[[1]] %>%
#   .[-(1:2), -(7:9)]
# column.names <- c("掛牌日期", "幣別", "現金買入", "現金賣出",
#                   "即期買入", "即期賣出")
# names(usd.table) <- column.names
# usd.table[,1] <- as.Date(usd.table[,1], "%Y/%m/%d")
# usd.table[,3:6] <- apply(usd.table[,3:6], 2, as.numeric)