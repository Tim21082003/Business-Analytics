library(quantmod)
library(dplyr)
library(ggplot2)
library(writexl)

# List of stock symbols
symbols <- c('AAPL', 'MSFT', 'GOOGL', 'META', 'NVDA',  # Technology
             'AMD', 'CSCO', 'ORCL',                    # Technology
             'JANX', 'NVAX', 'VKTX', 'AMGN', 'GILD',   # Biotechnology
             'AMC', 'DIS', 'NFLX', 'CMCSA',            # Entertainment
             'TSLA', 'TM', 'F', 'GM', 'HMC',           # Automotive
             'WMT', 'TGT', 'AMZN', 'COST', 'HD',       # Retail
             'XOM', 'CVX', 'COP', 'BP', 'SLB',         # Energy
             'JPM', 'BAC', 'WFC', 'C', 'GS',           # Financials
             'SNAP', 'PINS', 'MTCH')                   # Social Media

# Industry mapping
industry_mapping <- c('Technology', 'Technology', 'Technology', 'Social Media', 'Technology',
                      'Technology', 'Technology', 'Technology',
                      'Biotechnology', 'Biotechnology', 'Biotechnology', 'Biotechnology', 'Biotechnology',
                      'Entertainment', 'Entertainment', 'Entertainment', 'Entertainment',
                      'Automotive', 'Automotive', 'Automotive', 'Automotive', 'Automotive',
                      'Retail', 'Retail', 'Retail', 'Retail', 'Retail',
                      'Energy', 'Energy', 'Energy', 'Energy', 'Energy',
                      'Financials', 'Financials', 'Financials', 'Financials', 'Financials',
                      'Social Media', 'Social Media', 'Social Media')

fetch_stock_data <- function(symbols, industries, start_date = NULL) {
  all_data <- data.frame()  # Initialize an empty data frame
  
  for (i in seq_along(symbols)) {  # Loop over each Symbol
    Symbol <- symbols[i]
    industry <- industries[i]
    
    tryCatch({
      # Fetch data from Yahoo Finance
      stock_data <- getSymbols(Symbol, src = "yahoo", from = start_date, auto.assign = FALSE)
      
      # Convert to data frame and select necessary columns
      df <- data.frame(Date = index(stock_data), coredata(stock_data)) %>%
        mutate(Symbol = Symbol, Industry = industry) %>%
        rename(
          Open = paste0(Symbol, ".Open"),
          High = paste0(Symbol, ".High"),
          Low = paste0(Symbol, ".Low"),
          Close = paste0(Symbol, ".Close"),
          Volume = paste0(Symbol, ".Volume"),
          Adjusted = paste0(Symbol, ".Adjusted")
        ) %>%
        # Select necessary columns
        select(Date, Open, High, Low, Close, Adjusted, Volume, Symbol, Industry)
      
      # Combine with the main data frame
      all_data <- bind_rows(all_data, df)
    }, error = function(e) {
      warning(paste("Failed to fetch data for Symbol:", Symbol, "-", e$message))
    })
  }
  
  return(all_data)  # Return the combined data frame
}

calculate_cumulative_returns <- function(stock_data) {
  # Calculate daily returns for each stock
  stock_returns <- stock_data %>%
    group_by(Symbol) %>%
    mutate(Daily_Return = Close / lag(Close) - 1) %>%
    na.omit()
  
  # Calculate cumulative returns for each stock
  cumulative_returns <- stock_returns %>%
    group_by(Symbol) %>%
    summarise(Cumulative_Return = prod(1 + Daily_Return) - 1)
  
  return(cumulative_returns)
}

calculate_eps_pe_ratio <- function(stock_data) {
  # Calculate Earnings Per Share (EPS) and Price-to-Earnings (P/E) Ratio
  stock_eps_pe <- stock_data %>%
    group_by(Symbol) %>%
    summarise(EPS = mean(Adjusted / Volume), PE_Ratio = mean(Adjusted / Close))
  
  return(stock_eps_pe)
}

# Enter your desired start date (format: "YYYY-MM-DD")
start_date <- "2014-01-01"

# Fetch stock data with start date and industry mapping
stock_data <- fetch_stock_data(symbols, industry_mapping, start_date)

# Print the fetched data
print(stock_data)

# Calculate and print the cumulative returns for each stock
cumulative_returns <- calculate_cumulative_returns(stock_data)
print(cumulative_returns)

# Calculate and print EPS and P/E ratio for each stock
eps_pe_ratio <- calculate_eps_pe_ratio(stock_data)
print(eps_pe_ratio)

# Export the data to a CSV file
write.csv(stock_data, "stock_data_industry.csv", row.names = FALSE)

# Export the cumulative returns to a CSV file
write.csv(cumulative_returns, "cumulative_returns.csv", row.names = FALSE)

# Export EPS and P/E ratio to a CSV file
write.csv(eps_pe_ratio, "eps_pe_ratio.csv", row.names = FALSE)

# Plot the data for each industry
unique_industries <- unique(industry_mapping)
for (industry in unique_industries) {
  industry_data <- stock_data %>% filter(Industry == industry)
  plot <- ggplot(industry_data, aes(x = Date, y = Close, color = Symbol)) +
    geom_line() +
    labs(title = paste(industry, "Companies Stock Prices"), x = "Date", y = "Close Price") +
    scale_color_discrete(name = "Symbols")
  
  # Save the plot
  ggsave(filename = paste(industry, "_stock_prices.png", sep = ""), plot = plot)
}
