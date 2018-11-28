library('dplyr')
library('tidyr')


setwd("/Users/hraja/Documents/DataProjects/Loreal")
lancome = read.csv('Lancome_SCV.csv',sep='|',stringsAsFactors = FALSE)
kiehls = read.csv('Kiehls_SCV.csv',sep='|',stringsAsFactors = FALSE)
ysl = read.csv('YSL_SCV.csv',sep='|',stringsAsFactors = FALSE)

lancomeKiehlsOverlap = lancome %>% 
                       inner_join(.,kiehls,by="EMAIL")

# ysl and lancome overlap
lancomeYSLOverlap = lancome %>% 
  inner_join(.,ysl,by="EMAIL")

#kiehls and ysl overlap
kiehlsYSLOverlap = kiehls %>%
                   inner_join(.,ysl,by="EMAIL")

LancomeYSLKiehlsOverlap = lancome %>%
                          inner_join(.,kiehls,by="EMAIL") %>%
                          inner_join(.,ysl,by="EMAIL")





#Lancome subscriber analysis

setwd('/Users/hraja/Documents/DataProjects/Loreal/Lancome Data')
lancome_opens = read.csv('Lancome_Opens.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)
lancome_clicks = read.csv('Lancome_Clicks.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)
lancome_unsubs = read.csv('Lancome_Unsubcribes.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)
lancome_sends = read.csv('Sends by Email Name.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)
subscriberStats = read.csv('SubscriberStats-2.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)

lancomeSkus = read.csv('Lancome SKUs.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)
lancomeMars = read.csv('Lancome_Mars_Transactions.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)

categoryMapping = read.csv('Category Mapping.csv',header=TRUE,sep=",",stringsAsFactors = FALSE)


lancome_opens_mod = lancome_opens %>%
                    inner_join(.,categoryMapping,by=c("EmailName","EmailSubject"))
lancome_opens_mod = select(filter(lancome_opens_mod,Category != ''),c(1,3,6))

lancome_opens_summary = lancome_opens_mod %>%
                        group_by(.,SubscriberID,Category) %>%
                        summarise (TotalOpens = n())

lancome_clicks_mod = lancome_clicks %>%
  inner_join(.,categoryMapping,by=c("EmailName","EmailSubject"))

lancome_clicks_mod = select(filter(lancome_clicks_mod,Category != ''),c(1,3,6))
lancome_clicks_summary = lancome_clicks_mod %>%
  group_by(.,SubscriberID,Category) %>%
  summarise (TotalClicks = n())


lancome_sends_filtered = filter(categoryMapping,Category != '')

#creating a table of click to opens to get a ctor by subscribers
lancome_clicks_to_open = lancome_opens_summary %>% left_join(.,lancome_clicks_summary,by=c("SubscriberID","Category"))

#filter out all subscribers who have unsubscribed
subscriberStatsWithoutUnsub = filter(subscriberStats,TotalUnsubscribes == 0)

lancome_clicks_to_open = lancome_clicks_to_open %>% inner_join(.,subscriberStatsWithoutUnsub,by="SubscriberID")


lancome_clicks_to_open = select(lancome_clicks_to_open,SubscriberID,EmailAddress,Category,TotalOpens.x,TotalClicks.x)


#replace NAs by zeros
lancome_clicks_to_open[is.na(lancome_clicks_to_open)] = 0

#filter out subscribers who have clicks more than opens.

lancome_clicks_to_open_filtered = filter(lancome_clicks_to_open,TotalClicks.x <= TotalOpens.x)


#filter SCV to only keep email address and interests

lancome_SCV_filtered = select(lancome,EMAIL,COSMETICS,FRAGRANCE,SKIN_CARE)


# Add interests to lancomes click to open
lancome_clicks_to_open_filtered = lancome_clicks_to_open_filtered %>%
                                  inner_join(.,lancome_SCV_filtered,by=c("EmailAddress"="EMAIL"))



#filter out subscribers with no interests

lancome_clicks_to_open_filtered = filter(lancome_clicks_to_open_filtered,COSMETICS == 'Y' | FRAGRANCE == 'Y' | SKIN_CARE == 'Y')


CosmeticSubs = filter(lancome_clicks_to_open_filtered,COSMETICS == 'Y' & FRAGRANCE != 'Y' & SKIN_CARE != 'Y' )


CosmeticSubs = mutate(CosmeticSubs,CTOR = TotalClicks.x/TotalOpens.x)

SkincareSubs = filter(lancome_clicks_to_open_filtered,SKIN_CARE == 'Y' & COSMETICS != 'Y' & FRAGRANCE != 'Y' )


SkincareSubs = mutate(SkincareSubs,CTOR = TotalClicks.x/TotalOpens.x)

# Checking which emails get most unsubscribes

lancome_unsubs_filtered = lancome_unsubs %>% inner_join(.,lancome_sends_filtered,by=c("EmailName","EmailSubject"))


#9039 subscribers that have unsubcribed to either Makeup, Skincare or fragrance emails

lancome_unsubs_SCV = lancome_unsubs_filtered %>% 
                      inner_join(.,subscriberStats,by="SubscriberID") %>% 
                      inner_join(.,lancome,by=c("EmailAddress"="EMAIL"))


# filtering customers subscribed to only one interest
lancome_unsubs_SCV_filtered = filter(lancome_unsubs_SCV,COSMETICS == 'Y' | FRAGRANCE == 'Y' | SKIN_CARE == 'Y')

cosmeticUnsubs = filter(lancome_unsubs_SCV_filtered,COSMETICS == 'Y' & FRAGRANCE != 'Y' & SKIN_CARE != 'Y')
fragranceUnsubs = filter(lancome_unsubs_SCV_filtered,COSMETICS != 'Y' & FRAGRANCE == 'Y' & SKIN_CARE != 'Y')
skincareUnsubs = filter(lancome_unsubs_SCV_filtered,COSMETICS != 'Y' & FRAGRANCE != 'Y' & SKIN_CARE == 'Y')

lancome_unsubs_SCV_filtered = select(lancome_unsubs_SCV_filtered,SubscriberID,EmailAddress,Category,COSMETICS,FRAGRANCE,SKIN_CARE)



# looking at sends and categorising emails by sends
lancome_sends_raw = read.csv('Lancome_Sends.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)


lancome_sends_raw_filtered = lancome_sends_raw %>% inner_join(.,lancome_sends_filtered,by=c("EmailName","EmailSubject"))


lancome_sends_summary = lancome_sends_raw_filtered %>%
  group_by(.,SubscriberID,Category) %>%
  summarise (TotalSends = n())


lancome_email_summary = lancome_sends_summary %>%
                        left_join(.,lancome_opens_summary,by=c("SubscriberID","Category")) %>%
                        left_join(.,lancome_clicks_summary,by=c("SubscriberID","Category")) %>%
                        left_join(.,lancome_bounces_summary,by=c("SubscriberID","Category"))

lancome_email_summary[is.na(lancome_email_summary)] = 0

# reading the bounces
lancome_bounces = read.csv('Lancome_Bounces.csv',header=TRUE,sep="|",stringsAsFactors = FALSE)

lancome_bounces_mod = lancome_bounces %>%
  inner_join(.,categoryMapping,by=c("EmailName","EmailSubject"))

lancome_bounces_mod = select(filter(lancome_bounces_mod,Category != ''),c(1,3,6))

lancome_bounces_summary = lancome_bounces_mod %>%
  group_by(.,SubscriberID,Category) %>%
  summarise (TotalBounces = n())


# only keep subscribers that havent unsubscribed

lancome_email_summary_final = lancome_email_summary %>% inner_join(.,subscriberStatsWithoutUnsub,by="SubscriberID") %>% select(.,c(1,2,3,4,5,6,7))

# first make sure all NAs are zeros
lancome_email_summary_final[is.na(lancome_email_summary_final)] = 0

# filter out subscribers where no of sends  =  no of bounces

lancome_email_summary_final = filter(lancome_email_summary_final,TotalSends.x != TotalBounces.x)



#keep subscribers who have stated explicit interests
#152,831
lancome_SCV_withInterests = filter(lancome_SCV_filtered,COSMETICS=='Y' | FRAGRANCE == 'Y' | SKIN_CARE == 'Y')



lancome_final_data = lancome_email_summary_final %>% inner_join(.,lancome_SCV_withInterests,by=c("EmailAddress"="EMAIL"))

# Add open rate, click rate to final data
lancome_final_data = mutate(lancome_final_data,OpenRate = TotalOpens.x/(TotalSends.x-TotalBounces.x))
lancome_final_data = mutate(lancome_final_data,ClickRate = TotalClicks.x/(TotalSends.x-TotalBounces.x))



#subscribers who are only interested in cosmetics
cosmeticSubscribers = filter(lancome_final_data,COSMETICS=='Y' & FRAGRANCE=='' & SKIN_CARE=='')


# cosmetic subscribers behaviour with makeup emails
cosmeticSubscribersMakeup = filter(cosmeticSubscribers,Category=='Makeup')

#> mean(cosmeticSubscribersMakeup$OpenRate)
#[1] 0.2257586
#> mean(cosmeticSubscribersMakeup$ClickRate)
#[1] 0.01937804


#cosmetic subscribers with fragrance emails
cosmeticSubscribersFragrance = filter(cosmeticSubscribers,Category=='Fragrance')



# subscribers who are only interested in fragrance
fragranceSubscribers = filter(lancome_final_data,COSMETICS=='' & FRAGRANCE=='Y' & SKIN_CARE=='')

# fragrance subscribers behaviour with makeup emails
fragranceSubscribersMakeup = filter(fragranceSubscribers,Category=='Makeup')


#fragrance subscribers behaviour with fragrance emails
fragranceSubscribersFragrance = filter(fragranceSubscribers,Category=='Fragrance')

#fragrance subscribers behaviour with skincare emails
fragranceSubscribersSkinCare = filter(fragranceSubscribers,Category=='Skincare')


#subscribers with explicit one interest
SubwithOneInterest = filter(lancome_final_data,(COSMETICS=='Y' & FRAGRANCE=='' & SKIN_CARE=='') |
                            (COSMETICS=='' & FRAGRANCE=='Y' & SKIN_CARE=='') | 
                            (COSMETICS=='' & FRAGRANCE=='' & SKIN_CARE=='Y'))



#Emails that match their interest
EmailMatchInterest = filter(SubwithOneInterest,(Category == 'Makeup' & COSMETICS == 'Y') |
                              (Category == 'Fragrance' & FRAGRANCE == 'Y') |
                              (Category=='Skincare' & SKIN_CARE == 'Y'))

#Email does not match their interest
EmailNotMatchInterest = filter(SubwithOneInterest,(Category == 'Makeup' & COSMETICS == '') |
                              (Category == 'Fragrance' & FRAGRANCE == '') |
                              (Category=='Skincare' & SKIN_CARE == ''))









# modeling churn
# 15,718 unsubscribes in 12 months
unsubs = filter(subscriberStats,TotalUnsubscribes == 1)
lancome_new = read.csv('Lancome_SCV_New.csv',sep='|',stringsAsFactors = FALSE)
unsubsSCV = lancome_new %>% inner_join(.,unsubs,by=c("EMAIL"="EmailAddress"))

write.csv(unsubsSCV,"unsubAnalysis.csv",row.names=FALSE)
