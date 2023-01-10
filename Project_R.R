
#reding the files
orders <- read.csv("~/Documents/UIUC/Courses/PDA-BADAM577/Project/orders.csv")
products <- read.csv("~/Documents/UIUC/Courses/PDA-BADAM577/Project/products.csv")
orders_products_prior <- read.csv("~/Documents/UIUC/Courses/PDA-BADAM577/Project/order_products_prior.csv")
aisles <- read.csv("~/Documents/UIUC/Courses/PDA-BADAM577/Project/aisles.csv")
departments <- read.csv("~/Documents/UIUC/Courses/PDA-BADAM577/Project/departments.csv")

library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(knitr)

orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

#merge all the dataset into one
order_products_prior_df <- left_join(orders_products_prior, products)
order_products_prior_df <- left_join(order_products_prior_df, aisles)
order_products_prior_df <- left_join(order_products_prior_df, departments)
order_products_prior_df <- left_join(order_products_prior_df, orders)


head(order_products_prior_df)

## order count for hour of the day ##
order_count_hod <-  order_products_prior_df %>%
  group_by(order_hour_of_day) %>%
  summarise(count = n_distinct(order_id))


order_count_hod %>% 
  ggplot(aes(x=order_hour_of_day, y = count)) + 
  geom_line()+
  geom_point()

## Aisle wise order percent ##
aisle_count_hod <-  order_products_prior_df %>%
  group_by(aisle) %>%
  summarise(order_count = n_distinct(order_id))

aisle_percent <-  aisle_count_hod %>%
  mutate(freq = (order_count / sum(order_count))*100) %>% 
  arrange(desc(freq))

p <-  top_n(aisle_percent, n=40, freq) %>%  ggplot(aes(x=reorder(aisle, -freq) ,y=freq))+
  geom_bar(stat="identity", fill="steelblue")
  ##geom_text(aes(label=freq), vjust=1.6, color="white", size=3.5)+
  ##theme_minimal()

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## displaying aisle with 85% of the orders ##
aisle_percent[,"cum_freq"] <- cumsum(aisle_percent$freq)
sub <- subset(aisle_percent,cum_freq < 86 )

q <- sub %>%  ggplot(aes(x=reorder(aisle, +cum_freq) ,y=cum_freq, group = 1))+
  geom_line()+
  geom_point()

q + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ##geom_text(aes(label=cum_freq), vjust=1.6, color="white", size=3.5)+

## Days since prior order ##
days_prior_order <-  order_products_prior_df %>%
  group_by(days_since_prior_order) %>%
  summarise(count = n_distinct(order_id))

days_prior_order %>% 
  ggplot(aes(x=days_since_prior_order, y = count)) + 
  geom_line()+
  geom_point()

##
# products - banana 

days_prior_order_banana <-  order_products_prior_df[order_products_prior_df$product_name == 'Banana',] %>%
  group_by(days_since_prior_order) %>%
  summarise(count = n_distinct(order_id))

days_prior_order_banana %>% 
  ggplot(aes(x=days_since_prior_order, y = count)) + 
  geom_line()+
  geom_point()

# Product spnich and straw -> days since prior order

## Orders density in the hour of the day and day of the week ##
order_dow_hod <-  order_products_prior_df %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(count = n_distinct(order_id))

order_dow_hod %>% 
  ggplot(aes(order_hour_of_day, order_dow, fill= count)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdPu", direction = +1) +
  theme_ipsum()

 ## top 20 products ##
product_count <-  order_products_prior_df %>%
  group_by(product_name) %>%
  summarise(count = n_distinct(order_id)) %>%
  arrange(desc(count))

top_n(product_count, n=20, count) %>% 
  ggplot(aes(x=reorder(product_name, -count) ,y=count))+
  geom_bar(stat="identity", fill="steelblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

## Most reordered depts ##

depts_reorder <-  order_products_prior_df %>%
  group_by(department) %>%
  summarise(mean_re = mean(reordered)) %>%
  arrange(desc(mean_re))

depts_reorder %>% 
  ggplot(aes(x=reorder(department, -mean_re) ,y=mean_re))+
  geom_bar(stat="identity", fill="steelblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  


## Most reordered Aisle ##
aisle_reorder <-  order_products_prior_df %>%
  group_by(aisle) %>%
  summarise(mean_re = mean(reordered)) %>%
  arrange(desc(mean_re))

top_n(aisle_reorder, n=60, mean_re) %>% 
  ggplot(aes(x=reorder(aisle, -mean_re) ,y=mean_re, group = 1))+
  geom_line(color = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

## how many products people generally buy ##
number_items <- order_products_prior_df %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order))

number_items %>% 
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red")  +
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

## number of reorders ##
# making a factor for ease #
order_products_prior_df <- order_products_prior_df %>% 
  mutate(reordered = as.factor(reordered))

tmp <- order_products_prior_df %>%
  group_by(reordered) %>% 
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))

# 59% of the ordered items are reorders.
tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

## These 10 products have the highest probability of being reordered.
prob_products_reorder <-orders_products_prior %>% 
  group_by(product_id) %>% 
  summarize(temp = sum(reordered) , proportion_reordered = mean(reordered), n=n()) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

top_n(prob_products_reorder, n=10, proportion_reordered)  %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

## Which item do people put into the cart first? ##
First_product <- order_products_prior_df[order_products_prior_df$add_to_cart_order == 1, ] %>% 
  group_by(product_name) %>% 
  summarize(count =  n_distinct(order_id)) %>% 
  arrange(desc(count))

Other_product <- order_products_prior_df %>% 
  group_by(product_name) %>% 
  summarize(count_rest =  n_distinct(order_id)) %>% 
  arrange(desc(count_rest))

product_add_cart <- full_join(First_product, Other_product)
product_add_cart <- product_add_cart %>% 
  mutate(percent = count/count_rest) %>%
  arrange(desc(percent))

product_add_cart <- left_join(product_add_cart, products)
product_add_cart <- left_join(product_add_cart, aisles)
product_add_cart <- left_join(product_add_cart, departments)

depts_add_cart <-product_add_cart %>% 
  group_by(department) %>% 
  summarize( count_depts = n()) %>% 
  arrange(desc(count_depts))

depts_add_cart %>% 
  ggplot(aes(x=reorder(department, -count_depts),y=count_depts))+
  geom_bar(stat="identity",fill="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

## Add to cart order vs avg reorder
add_to_cart_order <- order_products_prior_df %>% 
  group_by(add_to_cart_order) %>% 
  summarize(avg_reorder =  mean(reordered)) %>% 
  arrange(add_to_cart_order)

add_to_cart_order %>% 
  ggplot(aes(x=add_to_cart_order, y=avg_reorder, group = 1))+
  geom_line(color = "steelblue") +
  coord_cartesian(xlim=c(0,50))

## Association between time of last order and probability of reorder ##
## We can see that if people order again on the same day, they order the same product more often. 
## Whereas when 30 days have passed, they tend to try out new things in their order. 
## As reorder number is less  but, ordered paced on the 30th day are more



orders_products_prior %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="red")

## Viewing the 

library(treemap)

org <- order_products_prior_df %>% 
  group_by(department, aisle) %>%
  summarize(ordercount = n_distinct(order_id)) %>%
  arrange(ordercount)

treemap(org,index=c("department","aisle"),vSize="ordercount",title="",palette="Set3",border.col="#FFFFFF")


####

mydata<-orders_products_prior[,1:2] #picking up first two rows
mydata<-merge(mydata,products,by="product_id") #merging
mydata<-arrange(mydata, order_id) # ascending order
mydata<-mydata[,c(2,3)] #dropping other columns
mydata[1:10,] # sneak peek

dt <- split(mydata$product_name, mydata$order_id)
dt2 = as(dt,"transactions")
rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8, minlen = 3))
plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")

rules3 = as(rules, "data.frame")
inspect( subset( rules, subset = rhs %pin% "Banana" ))


###











