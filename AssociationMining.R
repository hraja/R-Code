library('dplyr')
library('tidyr')


retail.raw = readLines('http://fimi.ua.ac.be/data/retail.dat')
head(retail.raw)

retail.list = strsplit(retail.raw," ")

names(retail.list) = paste("Trans",1:length(retail.list),sep="")

str(retail.list)

rm(retail.raw)
library(car)
library(arules)
retail.trans = as(retail.list,"transactions")
summary(retail.trans)


rm(retail.list)

retail.rules = apriori(retail.trans,parameter = list(supp=0.001,conf = 0.4))

library(arulesViz)
plot(retail.rules,interactive = TRUE)


retail.h1 = head(sort(retail.rules,by="lift"),50)
inspect(retail.h1)



setwd('/Users/hraja/Documents/DataProjects/Warehouse')

orders = read.csv('orders.csv',header=TRUE,sep='|',stringsAsFactors = FALSE)
rm(orders)

orders = filter(orders,ORDERSTATUS == "COMPLETE")


ordersbytrans = aggregate(PRODUCT ~ ORDERNO, data = orders, paste, collapse = ",")
ordersbytrans = ordersbytrans[-1,]
library(arules)
warehouse.list = strsplit(ordersbytrans$PRODUCT,",")
names(warehouse.list ) = paste("Trans",1:length(warehouse.list),sep="")
warehouse.trans = as(warehouse.list,"transactions")
summary(warehouse.trans)
library(arulesViz)
retail.rules = apriori(warehouse.trans,parameter = list(supp=0.0008,conf = 0.5))






# looking at sessions data
library('dplyr')
library('splitstackshape')
library(arules)
library(arulesViz)

sessions = read.csv('sessions_updated.csv',header=TRUE,sep='|',stringsAsFactors = FALSE)
skuCategories = read.csv('Category_Clean.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)

sessions_modified= sessions %>% inner_join(.,skuCategories,c("sku"="Sku.ID"))

sessionsTimeSeriesAnalysis = select(sessions_modified,c(session_id,Product.Name,Category,Timestamp))

write.csv(sessionsTimeSeriesAnalysis,"ViewedProductsbyDay.csv",row.names = FALSE)

sessionsbytrans = aggregate(Category ~ session_id, data = sessions_modified, paste, collapse = ",")
sessionsbytrans = sessionsbytrans[-1,]
sessions.list = strsplit(sessionsbytrans$Category,",")
names(sessions.list ) = paste("Session",1:length(sessions.list),sep="")
sessions.trans = as(sessions.list,"transactions")
session.rules = apriori(sessions.trans,parameter = list(supp=0.0001,conf = 0.6))

plot(session.rules)
session.h1 = head(sort(session.rules,by="lift"),50)

inspect(session.h1)




#reading time between orders for warehouse
#TimeBtwOrdersRaw
timebtwordersraw = read.csv('TimeBtwOrdersRaw.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)

highvaluecust = read.csv('High Value Cusgtomers.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)

usersbyemail = read.csv('UsersbyEmail.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)

timebtworders = timebtwordersraw %>% inner_join(.,usersbyemail,c("user_id"="User.Id")) %>%
                inner_join(.,highvaluecust,by="Email")

timebtwordersgrped = unique(timebtworders[,c(1,2,3)]) %>% inner_join(.,timebtworders,by="user_id")
timebtwordersgrped = timebtworders %>% group_by(.,user_id,Timestamp) %>% summarise(.,Lead = max(LeadVal))


daysbyemail = read.csv('DaysbyEmail.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)
twoOrMoreOrders = read.csv('TwoOrMoreOrders.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)
finaltimebtwOrders = daysbyemail %>% inner_join(.,twoOrMoreOrders,by="Email") %>%
                      inner_join(.,highvaluecust,by="Email")
write.csv(finaltimebtwOrders,"finaltimebtworders.csv",,row.names = FALSE)