library('dplyr')
library('tidyr')

setwd('/Users/hraja/Documents/DataProjects/Nike/Purchase Data/Datasets')


#loading oct - nov data
oct_nov = read.csv('oct-nov.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)
nov_dec = read.csv('nov-dec.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)
dec_jan = read.csv('dec-jan.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)


oct_nov = oct_nov[,-20]

combinedOrders = rbind(oct_nov,nov_dec)
combinedOrders = rbind(combinedOrders,dec_jan)


# how many distinct orders (184,498)
orders = unique(combinedOrdersFiltered$Order.No)


#take out all DN rows
combinedOrdersFiltered = filter(combinedOrders,Type == 'SN' | Type == 'SS')

#Orders with Coupons
orderswithcoupons = filter(combinedOrdersFiltered,Item.Code == 'COUPONDISCOUNT') %>%
                    select(.,Order.No,Item.Code,Amount)

orderswithcoupons = mutate(orderswithcoupons,Coupon = 'C')
#unique orders with coupons 46317
couponOrderCount  = unique(orderswithcoupons$Order.No)


# All orders without coupons
orderswithoutCoupons = combinedOrdersFiltered %>% left_join(orderswithcoupons,by='Order.No') %>%
                        filter(.,is.na(Coupon))


#number of orders without coupons #138181
withoutCouponCount = unique(orderswithoutCoupons$Order.No)


# remove returns
rm(basketAnalysis)
basketAnalysisWithoutCoupons = orderswithoutCoupons %>% filter(.,Amount.x > 0) %>%
                 group_by(.,Order.No) %>% summarise(.,BasketSize = n())

basketAnalysisWithCoupons = combinedOrdersFiltered %>% inner_join(.,orderswithcoupons,by='Order.No') %>%
                            filter(.,Amount.x > 0) %>%
                            filter(.,Item.Code.x != 'COUPONDISCOUNT') %>% group_by(.,Order.No) %>% summarise(.,BasketSize = n())


write.csv(basketAnalysisWithCoupons,'basketAnalysisWithCoupons.csv',row.names = FALSE)





#combine all sports bottles
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
                gsub("123011-030MISC","SportWaterBottle",x)
                    }))
#123011-427MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("123011-427MISC","SportWaterBottle",x)
}))

#123011-710MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("123011-710MISC","SportWaterBottle",x)
}))

#341009-403MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("341009-403MISC","SportWaterBottle",x)
}))

#341009-442MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("341009-442MISC","SportWaterBottle",x)
}))
#341009-511MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("341009-511MISC","SportWaterBottle",x)
}))

#341009-602MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("341009-602MISC","SportWaterBottle",x)
}))

#341009-649MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("341009-649MISC","SportWaterBottle",x)
}))
#341009-901MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("341009-901MISC","SportWaterBottle",x)
}))



#FC0152-442MISC
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("FC0152-442MISC","SportWaterBottle",x)
}))

#FC0152-632MISC

combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("FC0152-632MISC","SportWaterBottle",x)
}))


#FC0152-710MISC

combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("FC0152-710MISC","SportWaterBottle",x)
}))


# Replace Distance Shorts
#642804-010L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-010L","Distance Short",x)
}))
#642804-010M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-010M","Distance Short",x)
}))
#642804-010XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-010XL","Distance Short",x)
}))
#642804-0112XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-0112XL","Distance Short",x)
}))
#642804-011L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-011L","Distance Short",x)
}))

#642804-011M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-011M","Distance Short",x)
}))
#642804-011S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-011S","Distance Short",x)
}))
#642804-011XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-011XL","Distance Short",x)
}))
#642804-3922XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-3922XL","Distance Short",x)
}))

#642804-392L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-392L","Distance Short",x)
}))
#642804-392M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-392M","Distance Short",x)
}))
#642804-392S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-392M","Distance Short",x)
}))
#642804-392XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-392XL","Distance Short",x)
}))
#642804-4292XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-4292XL","Distance Short",x)
}))
#642804-429L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-429L","Distance Short",x)
}))
#642804-429M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-429M","Distance Short",x)
}))
#642804-429S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-429S","Distance Short",x)
}))
#642804-429XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-429XL","Distance Short",x)
}))
#642804-4322XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-4322XL","Distance Short",x)
}))
#642804-432L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-432L","Distance Short",x)
}))
#642804-432M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-432M","Distance Short",x)
}))
#642804-432S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-432S","Distance Short",x)
}))
#642804-432XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-432XL","Distance Short",x)
}))
#642804-4812XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-4812XL","Distance Short",x)
}))
#642804-481L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-481L","Distance Short",x)
}))
#642804-481M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-481M","Distance Short",x)
}))
#642804-481S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-481S","Distance Short",x)
}))
#642804-481XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-481XL","Distance Short",x)
}))
#642804-687L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-687L","Distance Short",x)
}))
#642804-687M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-687M","Distance Short",x)
}))
#642804-687S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-687S","Distance Short",x)
}))
#642804-687XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642804-687XL","Distance Short",x)
}))

#NEW RECORDS

#642807-0102XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-0102XL","Distance Short",x)
}))
#642807-010L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-010L","Distance Short",x)
}))
#642807-010M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-010M","Distance Short",x)
}))
#642807-010S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-010S","Distance Short",x)
}))
#642807-010XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-010XL","Distance Short",x)
}))
#642807-023M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-023M","Distance Short",x)
}))

#642807-023XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-023XL","Distance Short",x)
}))
#642807-027M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-027M","Distance Short",x)
}))

#642807-4102XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-4102XL","Distance Short",x)
}))
#642807-410L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-410L","Distance Short",x)
}))
#642807-410M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-410M","Distance Short",x)
}))
#642807-410S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-410S","Distance Short",x)
}))
#642807-410XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-410XL","Distance Short",x)
}))
#642807-4292XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-4292XL","Distance Short",x)
}))
#642807-429L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-429L","Distance Short",x)
}))
#642807-429M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-429M","Distance Short",x)
}))
#642807-429S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-429S","Distance Short",x)
}))
#642807-429XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-429XL","Distance Short",x)
}))
#642807-4302XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-4302XL","Distance Short",x)
}))
#642807-430L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-430L","Distance Short",x)
}))

#642807-430M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-430M","Distance Short",x)
}))
#642807-430S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-430S","Distance Short",x)
}))
#642807-430XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-430XL","Distance Short",x)
}))
#642807-4802XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-4802XL","Distance Short",x)
}))
#642807-480L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-480L","Distance Short",x)
}))
#642807-480M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-480M","Distance Short",x)
}))
#642807-480S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-480S","Distance Short",x)
}))
#642807-480XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-480XL","Distance Short",x)
}))
#642807-658L
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-658L","Distance Short",x)
}))
#642807-658M
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-658M","Distance Short",x)
}))
#642807-658S
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-658S","Distance Short",x)
}))
#642807-658XL
combinedOrdersFiltered  = data.frame(lapply(combinedOrdersFiltered,function(x) {
  gsub("642807-658XL","Distance Short",x)
}))






combinedOrdersFiltered = data.frame(lapply(combinedOrdersFiltered, as.character),stringsAsFactors = FALSE)

PurchaseMining = combinedOrdersFiltered %>% filter(.,Item.Code != 'COUPONDISCOUNT') %>%
                                            filter(.,Amount > 0) %>% filter(.,Item.Code != 'GIFT')


nike.raw =  aggregate(Item.Code ~ Order.No, data = PurchaseMining, paste, collapse = ",")


nike.list = strsplit(nike.raw$Item.Code,",")
names(nike.list ) = paste("Trans",1:length(nike.list),sep="")

library(arules)
library(arulesViz)
nike.trans = as(nike.list,"transactions")
summary(nike.trans)

nike.rules = apriori(nike.trans,parameter = list(supp=0.0002,conf = 0.99))
plot(nike.rules)
retail.h1 = head(sort(nike.rules,by="lift"),100)
inspect(retail.h1)

View(filter(combinedOrdersFiltered,grepl('341009-901MISC',Item.Code)))

#Writing combinedOrders to file
write.csv(combinedOrdersFiltered,'combinedOrdersFiltered.csv',row.names = FALSE)

#writing nike.raw to file
write.csv(nike.raw,'nike-raw.csv',row.names = FALSE)
