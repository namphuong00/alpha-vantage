
if (!requireNamespace("alphavantager", quietly = TRUE)) install.packages("alphavantager")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats")
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("TTR", quietly = TRUE)) install.packages("TTR")
if (!requireNamespace("xts", quietly = TRUE)) install.packages("xts")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")

library(alphavantager)
library(dplyr)
library(writexl)
library(rvest)
library(stringr)
library(forcats)
library(quantmod)
library(tibble)
library(ggplot2)
library(plotly)
library(TTR)
library(xts)
library(readxl)
library(httr)
library(jsonlite)
library(openxlsx)
#Set Alpha Vantage API key
av_api_key("U3PO0M5C6YCG85DB")  

##############Alphadata#######################

symbols <- c("UNH", "GS", "MSFT", "HD", "CRM", "MCD", "V", "AMGN", "HON", "CAT",
             "JNJ", "AAPL", "TRV", "MMM", "PG", "IBM", "VZ", "CSCO", "BA", "JPM",
             "AXP", "WMT", "MRK", "INTC", "KO", "DOW", "NKE", "AMZN", "CVX", "DIS")

#Khởi tạo khung dữ liệu trống để lưu trữ dữ liệu kết hợp
combined_data <- data.frame()
#Lặp qua từng biểu tượng và lấy dữ liệu
for (symbol in symbols) {
  data <- av_get(symbol = symbol, av_fun = "TIME_SERIES_DAILY", apikey = av_api_key(),
                 datatype = "json", outputsize = "full",
                 start_date = "2019-01-01", end_date = "2023-12-31")
  #Xử lý và kết hợp dữ liệu
  if (!is.null(data)) {
    colnames(data)[1] <- "Date"
    data$Date <- as.Date(data$Date)
    filtered_data <- subset(data, Date >= "2019-01-01" & Date <= "2023-12-31")
    filtered_data$Symbol <- symbol
    combined_data <- rbind(combined_data, filtered_data)
  }
}
#Xuất dữ liệu tổng hợp sang file excel
write_xlsx(combined_data, "D:/Goi phan mem ung dung trong tai chinh 2/Alphadata2019.xlsx")

####################Adjusted####################

#Hàm lấy dữ liệu giá cổ phiếu từ Yahoo Finance
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  tryCatch({
    #Lấy dữ liệu giá cổ phiếu từ Yahoo Finance
    stock_prices_xts <- getSymbols(Symbols = ticker, src = "yahoo", from = "2019-01-01", to = "2023-12-31", auto.assign = FALSE, ...)
    #Kiểm tra xem dữ liệu có rỗng không
    if (is.null(stock_prices_xts)) {
      stop("Error: No data available for the specified ticker symbol '", ticker, "'. Please check the symbol and try again.")
    }
    #Kiểm tra xem dữ liệu trả về có phải là đối tượng xts không
    if (!inherits(stock_prices_xts, "xts")) {
      stop("Error: Retrieved data is not in expected format.")
    }
    #Kiểm tra xem có bao nhiêu cột dữ liệu
    if (ncol(stock_prices_xts) != 1) {
      message("Warning: Retrieved data contains more than one column.")
      if (return_format == "tibble") {
        #Chuyển đổi sang định dạng tibble và trả về cột đầu tiên
        stock_prices <- as_tibble(stock_prices_xts[, 1, drop = FALSE])
        colnames(stock_prices) <- "Adjusted"
        stock_prices$Date <- index(stock_prices_xts)
        return(stock_prices)
      } else {
        #Trả về cột đầu tiên của đối tượng xts
        return(stock_prices_xts[, 1, drop = FALSE])
      }
    }
    #Đổi tên cột thành "Adjusted"
    colnames(stock_prices_xts) <- "Adjusted"
    #Chuyển sang định dạng tibble nếu được chỉ định
    if (return_format == "tibble") {
      stock_prices <- as_tibble(stock_prices_xts)
      stock_prices$Date <- index(stock_prices_xts)
      return(stock_prices)
    } else {
      return(stock_prices_xts)
    }
  }, error = function(e) {
    stop("Error: ", e$message)
  })
}
#Đọc danh sách cổ phiếu từ Wikipedia
dow <- read_html("https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select('Symbol', 'Company', 'Industry') %>%
  as_tibble()
#Định dạng lại tên cột
names(dow) <- dow %>% 
  names() %>% 
  str_to_lower() %>%  # Sử dụng hàm str_to_lower() từ thư viện stringr
  make.names()
#Thêm cột symbol vào dataframe
ticker_symbols <- dow$symbol
# Tạo một danh sách trống để lưu kết quả
results_list <- list()
#Lặp qua mỗi ký hiệu chứng khoán
for (symbol in ticker_symbols) {
  #Gọi hàm và lưu kết quả
  result <- get_stock_prices(symbol, return_format = "tibble")
  #Thêm cột "symbol" với giá trị là mã chứng khoán tương ứng
  result <- result %>% mutate(symbol = symbol)
  #Lưu kết quả vào danh sách
  results_list[[symbol]] <- result
}
#Kết hợp các kết quả thành một dataframe
combined_result <- do.call(rbind, results_list)
#Đổi tên cột "symbol" thành "Symbol"
combined_result <- combined_result %>%
  rename(Symbol = symbol)
#Ghi kết quả kết hợp vào một tệp Excel
write_xlsx(combined_result, "D:/Goi phan mem ung dung trong tai chinh 2/Adjusted_with_symbol2.xlsx")

###################Industry###################

#Đọc dữ liệu từ file Excel
data <- read_excel("D:/Goi phan mem ung dung trong tai chinh 2/Alphadata2019.xlsx")
#Tạo khung dữ liệu ánh xạ ngành
industry_mapping <- data.frame(
  Symbol = c("UNH", "GS", "MSFT", "HD", "CRM", "MCD", "V", "AMGN", "HON", "CAT",
             "JNJ", "AAPL", "TRV", "MMM", "PG", "IBM", "VZ", "CSCO", "BA", "JPM",
             "AXP", "WMT", "MRK", "INTC", "KO", "DOW", "NKE", "AMZN", "CVX", "DIS"),
  Industry = c("Healthcare", "Financials", "Technology", "Retail", "Technology", 
               "Consumer Services", "Financials", "Healthcare", "Industrials", "Industrials",
               "Healthcare", "Technology", "Financials", "Industrials", "Consumer Goods", 
               "Technology", "Telecommunication Services", "Technology", "Industrials", "Financials",
               "Financials", "Consumer Goods", "Healthcare", "Technology", "Consumer Goods", 
               "Basic Materials", "Consumer Goods", "Consumer Services", "Consumer Goods", "Energy")
)

#Hợp nhất dữ liệu với bản đồ ngành
data_with_industry <- merge(data, industry_mapping, by.x = "Symbol", by.y = "Symbol", all.x = TRUE)
#Ghi dữ liệu có cột Ngành đã thêm vào file Excel mới
write_xlsx(data_with_industry, "D:/Goi phan mem ung dung trong tai chinh 2/Alphadata2019_with_industry.xlsx")

##################merged###################

#Đọc dữ liệu từ các tệp Excel
data1 <- read_excel("D:/Goi phan mem ung dung trong tai chinh 2/Alphadata2019.xlsx")
data1$Date <- as.Date(data1$Date)
data2 <- read_excel("D:/Goi phan mem ung dung trong tai chinh 2/Adjusted_with_symbol2.xlsx")
data2$Date <- as.Date(data2$Date)
data3 <- read_excel("D:/Goi phan mem ung dung trong tai chinh 2/Alphadata2019_with_industry.xlsx")
data3$Date <- as.Date(data3$Date)
#Gộp dữ liệu dựa trên 'Date' và 'Symbol'
merged_data <- inner_join(data1, data2, by = c("Date", "Symbol"))
#Sắp xếp data3 theo cột "Symbol"
data3 <- arrange(data3, Symbol)
#Thêm cột "Industry" từ data3 vào merged_data, theo thứ tự của Symbol
merged_data2 <- cbind(merged_data, Industry = data3$Industry[match(merged_data$Symbol, data3$Symbol)])
#Ghi dữ liệu có cột Ngành đã thêm vào file Excel mới
write_xlsx(merged_data2, "D:/Goi phan mem ung dung trong tai chinh 2/merged.xlsx")

######################BCTC#######################

#Alpha Vantage API key
API_KEY <- "U3PO0M5C6YCG85DB"
COMPANIES <- c("UNH", "GS", "MSFT", "HD", "CRM", "MCD", "V", "AMGN", "HON", "CAT",
               "JNJ", "AAPL", "TRV", "MMM", "PG", "IBM", "VZ", "CSCO", "BA", "JPM",
               "AXP", "WMT", "MRK", "INTC", "KO", "DOW", "NKE", "AMZN", "CVX", "DIS")
fetch_and_combine_data <- function(api_function, companies) {
  results <- list()
  #Lặp qua từng biểu tượng công ty
  for (COMPANY in companies) {
    #Thiết lập URL yêu cầu lấy dữ liệu trong 5 năm qua (2019-2023)
    url <- paste0("https://www.alphavantage.co/query?function=", api_function, "&symbol=", COMPANY, "&apikey=", API_KEY, "&datatype=json&outputsize=full")
    #Thực hiện yêu cầu GET
    response <- GET(url)
    #Chuyển đổi kết quả thành JSON, chỉ định mã hóa UTF-8
    data <- content(response, "text", encoding = "UTF-8") %>%
      fromJSON()
    #Kiểm tra xem dữ liệu có được trả về không
    if (!is.null(data$annualReports)) {
      #Trích xuất dữ liệu từ khóa "annualReports"
      annual_reports <- data$annualReports
      #Lọc dữ liệu các năm 2019 đến 2023
      annual_reports <- annual_reports[which(as.numeric(substr(annual_reports$fiscalDateEnding, 1, 4)) >= 2019 & as.numeric(substr(annual_reports$fiscalDateEnding, 1, 4)) <= 2023), ]
      #Chuyển đổi dữ liệu thành khung dữ liệu
      df <- as.data.frame(annual_reports)
      #Thêm cột ký hiệu vào khung dữ liệu ở vị trí đầu tiên
      df <- cbind(Symbol = COMPANY, df)
      #Lưu trữ khung dữ liệu trong danh sách kết quả
      results[[COMPANY]] <- df
    } else {
      print(paste("No data returned from the API for", COMPANY, "using function", api_function))
    }
  }
  #Kết hợp tất cả các khung dữ liệu thành một nếu có bất kỳ dữ liệu nào được thu thập
  if (length(results) > 0) {
    combined_df <- bind_rows(results)
  } else {
    combined_df <- data.frame() 
    print("No data collected for any company.")
  }
  #Trả về khung dữ liệu kết hợp
  return(combined_df)
}
Income_statement_df <- fetch_and_combine_data("Income_Statement", COMPANIES)
write.xlsx(Income_statement_df, file = "Income_Statement.xlsx", overwrite = TRUE)
write.xlsx(Income_statement_df, file = "D:/Goi phan mem ung dung trong tai chinh 2/Financial_Reports_20192023.xlsx", overwrite = TRUE)



