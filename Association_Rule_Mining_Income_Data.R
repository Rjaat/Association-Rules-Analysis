library(tidyverse)
#library for Association rules
library(arules) 
#reading dataset data
data<- read.csv("adult.csv")
#look on data
head(data)
#general visualization of data
ggplot(data, aes(age)) + geom_bar()
ggplot(data, aes(education.num)) + geom_bar()
ggplot(data, aes(hours.per.week)) + geom_bar()
ggplot(data, aes(workclass)) + geom_bar()
#considering columns which seems useful of dropping unuseful columns
data<- data[,c(1,2,4,6,9,10,11,12,13,14,15)]
#data summary
summary(data)
table(data$age)
table(data$race)
#discretization of data
levels(data$income)
levels(data$income)[1] <-"less"
levels(data$income)[2] <-"more"
data_discretized_age <- data %>% mutate(
  age= discretize(age, breaks = 10, method="interval")
)
table(data_discretized_age$age)
trans <- transactions(data_discretized_age)
trans
data_discretized_capital.gain <- data %>% mutate(
  capital.gain= discretize(capital.gain, breaks = 2, method="interval")
)
table(data_discretized_capital.gain$capital.gain)
trans <- transactions(data_discretized_capital.gain)
trans
data_discretized_capital.loss <- data %>% mutate(
  capital.loss= discretize(capital.loss, breaks = 3, method="interval")
)
table(data_discretized_capital.loss$capital.loss)
trans <- transactions(data_discretized_capital.loss)
trans
data_discretized_hours.per.week  <- data %>% mutate(
  hours.per.week = discretize(hours.per.week , breaks = 2, method="interval")
)
table(data_discretized_hours.per.week $hours.per.week)
trans <- transactions(A_discretized_hours.per.week )
trans
age<-data[,1]
capital.gain<-data$capital.gain
capital.loss<-data$capital.loss
hours.per.week<-data$hours.per.week

# inspection of transaction data
summary(trans)
#Showing column names of transaction data 
colnames(trans)
#showing column names in original data we preprocessed
colnames(data)
#matrix representation
as(trans, "matrix")[1:3,]
inspect(trans[1:3])
#plotting relative frequency 
itemFrequencyPlot(trans,topN = 15)
ggplot(
  tibble(
    Support = sort(itemFrequency(trans, type = "absolute"), decreasing = TRUE),
    Item = seq_len(ncol(trans))
  ), aes(x = Item, y = Support)) + geom_line()
sapply(data_discretized_age, class)
data_factors <-data_discretized_age  %>% mutate_if(is.logical, factor)

sapply(data_factors, class)
summary(data_factors)
trans_factors <- transactions(data_factors)
trans_factors
#vartical layout
itemFrequencyPlot(trans_factors, topN = 15)
vertical <- as(trans, "tidLists")
as(vertical, "matrix")[1:10, 1:5]
#frequent itemset
2^ncol(trans)
its <- apriori(trans, parameter=list(target = "frequent"))
its
5/nrow(trans)
its <- apriori(trans, parameter=list(target = "frequent", support = 0.5))
its
its <- sort(its, by = "support")
inspect(head(its, n = 10))
its <- sort(its, by = "support")
inspect(head(its, n = 10))
ggplot(tibble(`Itemset Size` = factor(size(its))), aes(`Itemset Size`)) + geom_bar()
inspect(its[size(its) > 10])
#maximal frequent itemset
#None of its immediate superset are frequent
its_max <- its[is.maximal(its)]
its_max
inspect(head(its_max, by = "support"))
#closest frequent itemset
#none of it's immediate superset has same support
its_closed <- its[is.closed(its)]
its_closed
inspect(head(its_closed, by = "support"))
counts <- c(
  frequent=length(its),
  closed=length(its_closed),
  maximal=length(its_max)
)

ggplot(as_tibble(counts, rownames = "Itemsets"),
       aes(Itemsets, counts)) + geom_bar(stat = "identity")
#Association rule minning
rules <- apriori(trans, parameter = list(support = 0.6, confidence = 0.8))
#Showing total no of rules generated
length(rules)
inspect(head(rules))
quality(head(rules))
#Sorting rules by the lift value
rules <- sort(rules, by = "lift")
inspect(head(rules, n = 15))
#applying apriori algorithm
r <- apriori(trans_factors)
print(object.size(r), unit = "Mb")
inspect(r[1:15])
inspect(head(r, n = 15, by = "lift"))
#Library for rules visualization
library(arulesViz)
inspectDT(rules)
plot(rules, engine = "html")
plot(rules, method = "matrix", engine = "html") 
plot(rules, method = "graph", engine = "html")
#additional intrest measure
interestMeasure(rules[1:15], measure = c("phi", "gini"),
                trans = trans)
quality(rules) <- cbind(quality(rules),
                        interestMeasure(rules, measure = c("phi", "gini"),
                                        trans = trans))
inspect(head(rules, by = "phi"))
type <- grep("type=", itemLabels(trans), value = TRUE)
type
rules_type <- apriori(trans, appearance= list(rhs = type))
inspect(head(sort(rules_type, by = "lift")))
plot(rules)
plot(rules, control = list(jitter = 0))
plot(rules, shading = "order")
plot(rules, method = "grouped")
plot(rules, method = "graph")
plot(head(rules, by = "phi", n = 100), method = "graph")

summary(data)
data_trans <- transactions(data)
inspect(head(data_trans))
rules <- apriori(data_trans, parameter = list(support = 0.6, confidence = 0.8))
rules
#lift ->> probability of togatherness
#gini --> info provide 
#phi --> binary correlation
