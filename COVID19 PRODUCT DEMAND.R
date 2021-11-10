####LOAD THE DPLYR PACKAGE
require(dplyr)

##LET'S LOAD THE DATASET
store <-read.csv("MY DEPARTMENTAL STORE.csv")
#VIEW THE DATASET
View(store)

###1.1.GET THE INFORMATION OF THE PRODUCT WHERE TO PRODUCT_TYPE IS 'foodgrains&spices'

store1<-filter(store, PRODUCT_TYPE=="foodgrains&spices")
View(store1)

###1.2. GET THE INFORMATION OF THE PRODUCT WHERE IT BELONGS TO COMPANY S&B
store1 <- filter(store, COMPANY %in% c("S", "M"))
View(store1)

###1.3. GET THE INFORMATION OF THE PRODUCT WHERE PRODUCT_CATEGORY IS 'Dry Fruits'
store1<-filter(store, PRODUCT_CATEGORY=="Dry Fruits")
View(store2)


####2.select() 
###2.1. GET THE INFORMATION OF COLUMNS SELLING_PRICE,COMPANY
store1 <- select(store, SELLING_PRICE, COMPANY)
View(store2)

###2.2.GET THE INFORMATION OF COLUMNS 2 & 5
store1 <- select(store, c(2,5))
View(store1)

###2.3. GET THE INFORMATION OF COLUMNS 2 TO 5
store1 <- select(store, 2:5)
View(store1)

###2.4. GET THE INFORMATION OF COLUMNS STARTING WITH P
store1 <- select(store, starts_with("P") )
View(store1)

###2.5. GET THE INFORMATION OF COLUMNS ENDING WITH E
store1<- select(store, ends_with("E"))
View(store1)

###2.6. GET THE INFORMATION OF COLUMNS CONTAINING E
store1<- select(store, contains("E"))
View(store1)

###3.mutate()
###3.1. ADD A COLUMN TO SHOW THE PROFIT

store1<- mutate(store, PROFIT= SELLING_PRICE-COST_PRICE)
View(store1)

###3.2. ADD A COLUMN TO SHOW THE PROFIT PERCENT
store2 <- mutate(store1, PROFIT_PERCENT= PROFIT/COST_PRICE*100)
View(store2)

###3.3. ADD A COLUMN TO SHOW THE NET PROFIT
store3<- mutate(store2,NET_PROFIT=PROFIT*QUANTITY_DEMANDED )
View(store3)

###3.4.LET'S SAVE THE UPDATED FILE
write.table(store3, file = "FINAL DEPARTMENTAL STORE.csv", sep=",")

store <- read.csv("FINAL DEPARTMENTAL STORE.csv")
View(store)

####4.arrange()
###4.1.ARRANGE THE DATASET IN ASCENDING ORDER OF QUANTITTY DEMANDED
store1<-arrange(store, QUANTITY_DEMANDED)
View(store1)

###4.2. ARRANGE THE DATASET IN DESCENDING ORDER OF SELLING PRICE
store1 <- arrange(store, desc(SELLING_PRICE))
View(store1)


#####5.summarize()
##5.1.Find the AVERAGE/mean
summarise(store, AVERAGE= mean(PROFIT,na.rm=TRUE))

##5.2.Find the SUMMATION
SUM_NET_PROFIT <- summarise(store1,SUM=sum(NET_PROFIT,  na.rm=TRUE))
View(SUM_NET_PROFIT)

###5.3.Find the MINIMUM
summarise(store, MINIMUM= min(PROFIT,na.rm=TRUE))

###5.4. Find the median 
summarise(store1, MEDIAN=median(PROFIT, na.rm=TRUE))


###5.5. Find the variance 
summarise(store3, VARIANCE=var(PROFIT, na.rm=TRUE))


###5.6. Find the standard deviation 
summarise(store1, STANDARD_DEVIATION=sd(PROFIT, na.rm=TRUE))

##5.7. Find the MAXIMUM
summarise(store, MAXIMUM= max(PROFIT,na.rm=TRUE))


## DATA VISUALIZATION USING ggplot2 
###THE TYPES OF PLOTS WE WILL DO ARE: SCATTER PLOT, LINE PLOT, COLUMN PLOT & HISTOGRAM.
###6.SCATTER PLOT

require(ggplot2)

##6.1.PLOT FOR NET_PROFIT & COMPANY WHERE COST PRICE  >10
store %>%filter(COST_PRICE>10)%>%
  ggplot(aes(x=COMPANY, y=NET_PROFIT, color=PRODUCT_CATEGORY))+geom_point()

##6.2.PLOT FOR PROFIT  & QUANTITY_DEMANDED WHERE PRODUCT TYPE == "hygiene"
store %>% filter(PRODUCT_TYPE=='hygiene') %>%
  ggplot(aes(x=QUANTITY_DEMANDED, y=PROFIT, color=PRODUCT_CATEGORY))+geom_point()

##6.3.PLOT FOR PROFIT  & QUANTITY_DEMANDED WHERE PRODUCT_TYPE == "beauty products"
store %>% filter(PRODUCT_TYPE == "beauty products") %>%
  ggplot(aes(x=QUANTITY_DEMANDED, y=PROFIT, color=PRODUCT_CATEGORY))+geom_point()

##6.4.PLOT FOR SELLING_PRICE & QUANTITY_DEMANDED
store %>%
  ggplot(aes(x=QUANTITY_DEMANDED, y=SELLING_PRICE, color=PRODUCT_CATEGORY))+geom_point()


####7.COLUMN PLOT
#7.1.PLOT FOR AVERAGE_NET_PROFIT & PRODUCT_TYPE

store %>% group_by(PRODUCT_TYPE) %>%
  summarise(AVERAGE_QUANTITY=mean(QUANTITY_DEMANDED))%>%
  ggplot(aes(x= PRODUCT_TYPE, y=AVERAGE_QUANTITY))+ geom_col(width = 0.6, fill='blue')+theme(text=element_text(size=9))


#7.2.PLOT FOR AVERAGE_QUANTITY & PRODUCT_TYPE
store %>% group_by(PRODUCT_TYPE) %>%
  summarise(AVERAGE_NET_PROFIT=mean(NET_PROFIT))%>%
  ggplot(aes(x=PRODUCT_TYPE,y=AVERAGE_NET_PROFIT))+geom_col(width=0.6, fill='light blue')+theme(text=element_text(size=10))
  

#7.3.PLOT FOR AVERAGE_NET_PROFIT & COMPANY
store %>% group_by(COMPANY) %>% 
  
  summarise(AVERAGE_NET_PROFIT=mean(NET_PROFIT)) %>%
  ggplot(aes(x=COMPANY, y=AVERAGE_NET_PROFIT))+geom_col(fill="light blue")+theme(text = element_text(size = 10))




#7.4.PLOT FOR AVERAGE_PROFIT_PERCENT & PRODUCT_TYPE
store %>% group_by(PRODUCT_TYPE) %>% 
  
  summarise(AVERAGE_PROFIT_PERCENT=mean(PROFIT_PERCENT, na.rm=TRUE)) %>%
  ggplot(aes(x=PRODUCT_TYPE, y=AVERAGE_PROFIT_PERCENT))+geom_col(fill="light blue")+theme(text = element_text(size = 6))


#8.LINE PLOT
### We use group=1 when we will use only one variable for grouping
#8.1.PLOT FOR AVERAGE_PROFIT & PRODUCT_CATEGORY
store%>% group_by(QUANTITY_DEMANDED)%>%
  summarise(AVERAGE_SELLING_PRICE=mean(SELLING_PRICE, na.rm=TRUE))%>%
  ggplot(aes(x=QUANTITY_DEMANDED, y= AVERAGE_SELLING_PRICE, group=1))+geom_line(color='BLUE')

#8.2. PLOT FOR AVERAGE_QUANTITY & PRODUCT_CATEGORY
store %>% group_by(PRODUCT_CATEGORY) %>% 
  summarise(AVERAGE_QAUNTITY=mean(SELLING_PRICE, na.rm=TRUE)) %>%
  ggplot(aes(x=PRODUCT_CATEGORY, y=AVERAGE_QAUNTITY, group=1))+geom_line(color="BLUE")+theme(text=element_text(size=4.5))



#######8.3. FOLLOWING PRICE-DEMAND RELATIONSHIP (AVERAGE_SELLING_PRICE VS QUANTITY_DEMANDED)
store %>% group_by(QUANTITY_DEMANDED) %>% 
  summarise(AVERAGE_SELLING_PRICE=mean(SELLING_PRICE, na.rm=TRUE)) %>%
  ggplot(aes(x=QUANTITY_DEMANDED, y=AVERAGE_SELLING_PRICE, group=1))+geom_line(color="blue")



###8.4.Different line for each COMPANY
#PLOT FOR AVERAGE_NET_PROFIT & COMPANY

store %>% group_by(PRODUCT_TYPE, COMPANY) %>% 
  summarise(AVERAGE_NET_PROFIT=mean(NET_PROFIT, na.rm=TRUE)) %>%
  ggplot(aes(x=PRODUCT_TYPE, y=AVERAGE_NET_PROFIT, group=COMPANY, color=COMPANY))+
  geom_line()



###8.5.PLOT FOR AVERAGE_NET_PROFIT & PRODUCT_TYPE WHERE NET_PROFIT>100
store %>% group_by(PRODUCT_TYPE) %>% 
  filter(NET_PROFIT>100) %>%
  summarise(AVERAGE_1=mean(NET_PROFIT, na.rm=TRUE)) %>%
  ggplot(aes(x=PRODUCT_TYPE, y=AVERAGE_1, group=1))+geom_line(color="blue")


### 9.HISTOGRAM 
#9.1. HISTOGRAM FOR PROFIT_PERCENT OF PRODUCT_CATEGORY

store%>%
  ggplot(aes(x=PROFIT_PERCENT,fill=PRODUCT_CATEGORY))+geom_histogram(binwidth = 5)



#9.2. HISTOGRAM FOR QUANTITY_DEMANDED OF PRODUCT_CATEGORY WHERE PRODUCT_TYPE IS "snacks"
store %>%
  filter(PRODUCT_TYPE=='snacks')%>%
  ggplot(aes(x= QUANTITY_DEMANDED, fill=PRODUCT_CATEGORY))+geom_histogram(binwidth =15)


#9.3.HISTOGRAM FOR SELLING_PRICE OF PRODUCT_CATEGORY WHERE PRODUCT_TYPE IS "snacks"
store %>%
  filter(PRODUCT_TYPE == "foodgrains&spices") %>%
  ggplot(aes(x=SELLING_PRICE, fill=PRODUCT_CATEGORY))+geom_histogram(binwidth = 15)
