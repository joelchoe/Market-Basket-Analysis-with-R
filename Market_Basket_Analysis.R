library(tidyverse)
library(arules)
library(arulesViz)
library(data.table)
library(readxl)

## load data
sheet_names <- excel_sheets(path = "RetailData.xlsx")
list_data <- lapply(sheet_names, function(x) read_xlsx("RetailData.xlsx", sheet = x))
data <- do.call("rbind", list_data)

## preprocess data
data <- data %>%
  mutate(InvoiceDate = as.Date(InvoiceDate, "%Y-%m-%d %H:%M:%S"))

## Identify Unique Countries
unique(data$Country)

## filter united kingdom
united_kingdom <- data %>% 
  filter(Country == "United Kingdom")

length(unique(united_kingdom$Description))
length(unique(united_kingdom$Invoice))

## produce transactions dataset
invoiced_items <- dcast(setDT(united_kingdom %>% group_by(Invoice) %>% select(Invoice, Description) %>% distinct(Description, .keep_all = TRUE)), Invoice~rowid(Invoice)) %>%
  select(!Invoice)
write.csv(invoiced_items,"invoiced_items.csv", quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")
transaction <- read.transactions('invoiced_items.csv', format = 'basket', sep=',')

## market basket analysis
itemFrequencyPlot(transaction, topN=10, type='absolute')
rules <- apriori(transaction, parameter = list(supp=0.01, conf=0.7, maxlen = 3))
rules <- sort(rules, by='confidence', decreasing = TRUE)
View(inspect(rules))
plot(rules)
plot(rules, method = "graph")
plot(rules, method = "grouped")
plot(rules, method = "graph", engine = "interactive", shading = NA)

## create dataframe for rules
rules_dataframe <- as(rules, 'data.frame')
View(rules_dataframe)




