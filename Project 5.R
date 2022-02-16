# Project 6  || High value customers identification for an E-Commerce company || Niladri Sekhar Sardar
#  Use the clustering methodology to segment customers into groups:

library(plyr)
library(dplyr)
library(ggplot2)
library(NbClust)
library(scales)
library("scatterplot3d")


ecom_data <- read.csv("Ecommerce.csv",header = T)

# data exploration
class(ecom_data)
View(ecom_data) 
str(ecom_data)
summary(ecom_data)
head(ecom_data)
dim(ecom_data)

# Removing redundant column X
ecom_data_subset <- subset(ecom_data, select = -X)
View(ecom_data_subset)


# Checking for missing values
length(unique(ecom_data_subset$CustomerID))
sum(is.na(ecom_data_subset$CustomerID))

mean(is.na(ecom_data_subset))

length(unique(ecom_data_subset$InvoiceNo))
length(unique(ecom_data_subset$CustomerID))

pos_quant <- ecom_data_subset[ecom_data_subset$Quantity > 0,]
nrow(pos_quant)

ecom_data_subset$InvoiceDate <- as.Date(ecom_data_subset$InvoiceDate, format = "%d-%b-%y") 
str(ecom_data_subset$InvoiceDate)

ecom_data_subset$InvoiceNo <- as.integer(ecom_data_subset$InvoiceNo)
table(ecom_data_subset$Country)

ecom_data_subset$item.return <- grepl("C", ecom_data_subset$InvoiceNo, fixed=TRUE)
ecom_data_subset$purchase.invoice <- ifelse(ecom_data_subset$item.return == "TRUE", 0,1)
customers <- as.data.frame(unique(ecom_data_subset$CustomerID))
names(customers) <- "CustomerID"
ecom_data_subset$recency <- as.Date("2017-12-08") - as.Date(ecom_data_subset$InvoiceDate)
temp <- subset(ecom_data_subset, purchase.invoice == 1)
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)
remove(temp)

customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE)
remove(recency)

customers$recency <- as.numeric(customers$recency)

customer.invoices <- subset(ecom_data_subset, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) <- NULL

annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)
remove(customer.invoices, annual.invoices)

range(customers$frequency)
table(customers$frequency)

customers <- subset(customers, frequency > 0)

ecom_data_subset['amount_spent'] = ecom_data_subset['Quantity'] * ecom_data_subset['UnitPrice']
total.sales <- aggregate(amount_spent ~ CustomerID, data = ecom_data_subset, FUN=sum, na.rm=TRUE)
names(total.sales)[names(total.sales)=="amount_spent"] <- "monetary"
customers <- merge(customers, total.sales, by="CustomerID", all.x=TRUE, sort=TRUE)
remove(total.sales)
hist(customers$monetary)

customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) 
hist(customers$monetary)

customers <- customers[order(-customers$monetary),]
high.cutoff <- 0.8 * sum(customers$monetary)
customers$high <- ifelse(cumsum(customers$monetary) <= high.cutoff, "Top 20%", "Bottom 80%")
customers$high <- factor(customers$high, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$high)
round(prop.table(table(customers$high)), 2)
remove(high.cutoff)

customers <- customers[order(customers$CustomerID),]

customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 

customers$monetary.log <- log(customers$monetary.log)
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)

View(customers)

plot1 <- ggplot(customers, aes(x = frequency, y = monetary)) + geom_point(aes(colour = recency, shape =high)) + scale_shape_manual(name = "80/20 Designation", values=c(17, 16)) + scale_colour_gradient(name="Recency\n(Days since Last Purchase))") + scale_y_continuous(label=dollar) + xlab("Frequency (Number of Purchases)") + ylab("Monetary Value of Customer (Annual Sales)")
plot1

plot2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log)) + geom_point(aes(colour = recency.log, shape =high)) + scale_shape_manual(name = "80/20 Designation", values=c(17, 16)) + scale_colour_gradient(name="Log-transformed Recency") + xlab("Log-transformed Frequency") + ylab("Log-transformed Monetary Value of Customer")
plot2

plot3 <- ggplot(customers, aes(x = frequency.z, y = monetary.z)) + geom_point(aes(colour = recency.z, shape =high)) + scale_shape_manual(name = "80/20 Designation", values=c(17, 16)) + scale_colour_gradient(name="Z-scored Recency") + xlab("Z-scored Frequency") + ylab("Z-scored Monetary Value of Customer")
plot3

#Use the following clustering algorithms:
#1.K means


preprocessed <- customers[,9:11]
clustmax <-  10 
models <- data.frame(k=integer(),tot.withinss=numeric(),betweenss=numeric(), totss=numeric(), rsquared=numeric())
for (k in 1: clustmax) 
 {
  print(k)
  output <- kmeans(preprocessed, centers = k, nstart = 20)
  var.name <- paste("cluster", k, sep="_")
  customers[,(var.name)] <- output$cluster
  customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
  cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
  cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
  colors <-c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
  cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
  title <- paste("k-means Solution with", k, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)
  print(title)
  cluster_centers <- ddply(customers,(customers[,(var.name)]), summarize,monetary=round(median(monetary),2),frequency=round(median(frequency),1),recency=round(median(recency), 0))
  names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  print(cluster_centers)
  cat("\n")
  cat("\n")
  models[k,("k")] <- k
  models[k,("tot.withinss")] <- output$tot.withinss
  models[k,("betweenss")] <- output$betweenss
  models[k,("totss")] <- output$totss
  models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) 
  assign("models", models, envir = .GlobalEnv)
}

r2_graph <- ggplot(models, aes(x = k, y = rsquared)) + geom_point(color="red") + geom_line(color="blue") + scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = 1:clustmax) + xlab("k (Number of Clusters)") + ylab("Variance Explained")
r2_graph

ss_graph <- ggplot(models, aes(x = k, y = tot.withinss)) + geom_point(color="red") + geom_line(color="blue") + scale_x_continuous(breaks = 1:clustmax) + scale_y_continuous(labels = scales::comma) + xlab("k (Number of Clusters)") + ylab("Total Within SS")
ss_graph


set.seed(1)
nc <- NbClust(preprocessed, min.nc=2, max.nc=10, method="kmeans")

table(nc$Best.n[1,])
nc$All.index

barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by Criteria")




#Use the following clustering algorithms:
#2.Hierarchical


use = scale(models[,-c(1)], center = TRUE, scale = TRUE)

dist = dist(use)  

d <- dist(as.matrix(dist))   # find distance matrix 
seg.hclust <- hclust(d)     # apply hirarchical clustering 
plot(seg.hclust)

groups.3 = cutree(seg.hclust,3)
table(groups.3)

groups.3



# • Identify the right number of customer segments.
# • Provide the number of customers who are highly valued.
# • Identify the clustering algorithm that gives maximum accuracy and explains robust clusters.
# • If the number of observations is loaded in one of the clusters, break down that cluster further using the clustering algorithm. 


df<- ecom_data
df

df %>% filter(CustomerID != 'NA') %>% group_by(CustomerID, InvoiceNo) %>% count()

df %>% filter(CustomerID != 'NA') %>% arrange(desc(Quantity)) %>% head(10) %>% 
  ggplot() + geom_col(aes(x = InvoiceNo, y= Quantity), fill = "blue", color ="black")

df %>% filter(Country != 'United Kingdom') %>%  group_by(Country) %>%  count() %>% arrange(desc(n)) %>%  head(10) %>%
  ggplot() + geom_col(aes(x= reorder(Country,n), y = n), fill ="red", color = "green") +
  geom_label(aes(x = reorder(Country,n), y = n, label = n)) +coord_flip()+labs(title = 'No of items brought in each country',x = 'Country',y = 'Total purchases')

df %>% filter(Description == "thrown away") %>% arrange(Quantity)
df %>% filter(Quantity <= 0) %>% arrange(Quantity)
df %>% filter (UnitPrice == 0) %>% arrange(UnitPrice) %>% View()

df %>% mutate(Customer = as.character(CustomerID))%>% filter(Customer != 'NA') %>% mutate(total_amount = Quantity * UnitPrice) %>% 
  group_by(Customer) %>% dplyr::summarise(total_bill = sum(total_amount)) %>% arrange(desc(total_bill)) %>%
  head(10)%>% ggplot()+ geom_col(aes(x = reorder(Customer, total_bill), y= total_bill), color= "green", fill= "orange") +
  geom_label(aes(x = reorder(Customer,total_bill), y= total_bill, label= total_bill)) + labs(x = 'InvoiceNo',y= 'Total bill amount') + coord_flip()

df %>% filter(InvoiceDate == '7-Dec-17')%>%  group_by(Country = "United Kingdom")%>%  arrange(desc(Quantity))%>%  head(10)%>%  View()

df%>%  filter(InvoiceDate == '7-Dec-17')%>%   group_by(Country = "United Kingdom")%>%   mutate(Purchases = Quantity)%>%   arrange(desc(Quantity))%>%   head(10)%>%   View()

df%>%  filter(InvoiceDate == '7-Dec-17', UnitPrice > -2500,  UnitPrice < 50,   Quantity < 200)%>%
  ggplot() + geom_point(aes(x = UnitPrice, y = Quantity, color = Country))+  labs(title = 'Total purchases on 07-Dec-2017', x = 'Countries', y = 'Quantity')

df%>%  filter(Description == "MOROCCAN TEA GLASS")%>%  group_by(Country)%>%  arrange(desc(Quantity))%>%  head(10)%>% 
  ggplot()+ geom_col(aes(x= reorder(InvoiceDate,Quantity), y= Quantity), color = "black", fill= "grey")+  geom_label(aes(x = reorder(InvoiceDate,Quantity), y = Quantity, label = Quantity))+
  labs(title = 'No of Moroccan tea glasses sold', x = 'DATE OF SALE',       y = 'Quantity sold')+  coord_flip()

df %>%  group_by(InvoiceNo) %>%  count()%>%  arrange (desc(n))%>%  head(10) %>%
  ggplot() + geom_col(aes(x= reorder(InvoiceNo,n), y= n), fill = "Blue", color ="red")+  geom_label(aes(x = reorder(InvoiceNo,n), y = n, label = n)) +
  labs(title = 'Top 10 purchases', x = 'InoviceNo', y = 'Total purchases')+  coord_flip()

df%>%  select(InvoiceNo, Description, Quantity:Country)%>%  View()

df %>%  filter(CustomerID != 'NA')%>%  group_by(CustomerID)%>%  dplyr::summarise(mean_quantity = mean(Quantity))%>%  View()

df1 = df%>%
  mutate(Date = as.Date(InvoiceDate,"%d-%b-%y"),
         total_sale = UnitPrice * Quantity)
dfd <-aggregate(df1$total_sale,list(df1$Date),sum)
ggplot(dfd,aes(x=Group.1,y=x))+geom_point()+geom_line()+geom_smooth()








