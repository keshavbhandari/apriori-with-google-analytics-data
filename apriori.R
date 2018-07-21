# Apriori

# Data Preprocessing
transform_ga_data <- function(dataset_name,output_name){
  
  cat("Reading Data","\n")
  ga_data <- read.csv(dataset, header = TRUE, stringsAsFactors = F)[,1:2]
  transformed_data <- list()
  
  cat("Transforming Data","\n")
  for (i in unique(ga_data$Transaction.ID)) {
    ID <- as.character(i)
    products <- as.list(ga_data[ga_data$Transaction.ID==i,1])
    transformed_data[[ID]] <- products
  }
  
  cat("Writing Data To New File","\n")
  invisible(lapply(transformed_data, function(x) write.table(data.frame(x), output_name, append= T, sep=',',row.names = F,col.names = F)))
  cat("Done","\n")
}

transform_ga_data(dataset_name = "Apriori_Dataset_GA.csv",output_name = "output.csv")
                   
if (!require('arules')) install.packages('arules'); library('arules')
if (!require('arulesViz')) install.packages('arulesViz'); library('arulesViz')                   

dataset <- read.transactions('output.csv', sep = ',', quote = "", rm.duplicates = TRUE)

summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
rules = apriori(data = dataset)
rules = apriori(data = dataset, parameter = list(minlen = 2, support = 0.002, confidence = 0.1))
rules = apriori(data = dataset, parameter = list(minlen = 2, support = 0.0025, confidence = 0.1))                   
rules = apriori(data = dataset, parameter = list(minlen = 3, support = 0.001, confidence = 0.1))

#Removing redundant rules
redundant_rules = is.redundant(rules)
summary(redundant_rules)
rules
rules <- rules[!redundant_rules]

#Removing inverse rules
gi <- generatingItemsets(rules)
duplicate <- which(duplicated(gi))
rules[-duplicate]
rules <- rules[-duplicate]
                   
# Viewing the results
inspect(sort(rules, by = 'lift')[1:10])

#Looking at a specific product
rules_product = apriori(data = dataset, 
                        parameter = list(minlen = 2, support = 0.001, confidence = 0.1),
                        appearance = list(default = "rhs", lhs = "\"8 pc Android Sticker Sheet\""))
inspect(sort(rules_product, by = 'lift')[1:5])

#Visualisation the rules
plot(rules, method = "graph")
plot(rules, method = "graph", engine='interactive')

## Capture it, and extract rhs
out = capture.output(inspect(rules[1:5]))

everything = labels(rules)
print(everything)

