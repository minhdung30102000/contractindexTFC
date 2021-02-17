library(reshape2)
library(tidyverse)
library(ggplot2)

## Melt Data
data<-Data_Supplier_Freshconnection
newdata<-melt(data=Data_Supplier_Freshconnection,id.vars = c("Product","Supplier","Delivery Reliability"),measure.vars = c("4 hours","1 day","2 days","1 week"),variable.name = "Delivery_Window",value.name = "Contract_Index")

### Mono Packaging Materials
monodata<-tanewda%>%filter(Supplier=="Mono Packaging Materials")
Reliability<-monodata$`Delivery Reliability`
Index<-monodata$`Contract Index`
Delivery_Window<-monodata$`Delivery Window`
ggplot(data = monodata,aes(x=Reliability,y=Index,color=Delivery_Window))+geom_line(size=1.5)+labs(x="Reliability",y="Contract Index",color="Delivery Window")+ggtitle("Mono Packaging Materials ")+
  theme(plot.title = element_text(hjust = 0.5))

### Plantin Pet
platindata<-newdata%>%filter(Supplier=="Plantin Pet")
Index2<-platindata$`Contract Index`
ggplot(data = platindata,aes(x=Reliability,y=Index2,color=Delivery_Window))+geom_line(size=1.5)+labs(x="Reliability",y="Contract Index",color="Delivery Window")+ggtitle("Plantin Pet")+
  theme(plot.title = element_text(hjust = 0.5))

### Trio Pet PLC
triodata<-newdata%>%filter(Supplier=="Trio Pet PLC")
Index3<-triodata$`Contract Index`
ggplot(data = triodata,aes(x=Reliability,y=Index3,color=Delivery_Window))+geom_line(size=1.5)+labs(x="Reliability",y="Contract Index",color="Delivery Window")+ggtitle("Trio Pet PLC")+
  theme(plot.title = element_text(hjust = 0.5))

### Wimberley-Park Packaging
wimberdata<-newdata%>%filter(Supplier=="Wimberley-Park Packaging")
Index4<-wimberdata$`Contract Index`
ggplot(data = wimberdata,aes(x=Reliability,y=Index4,color=Delivery_Window))+geom_line(size=1.5)+labs(x="Reliability",y="Contract Index",color="Delivery Window")+ggtitle("Wimberley-Park Packaging")+
  theme(plot.title = element_text(hjust = 0.5))

###	Cabana International
cabanadata<-newdata%>%filter(Supplier=="Cabana International")
Index5<-cabanadata$`Contract Index`
ggplot(data = cabanadata,aes(x=Reliability,y=Index5,color=Delivery_Window))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Delivery Window")+
  ggtitle("Cabana International")+
  theme(plot.title = element_text(hjust = 0.5))

###Jugo Puro
jugodata<-newdata%>%filter(Supplier=="Jugo Puro")
Index6<-jugodata$`Contract Index`
ggplot(data = jugodata,aes(x=Reliability,y=Index6,color=Delivery_Window))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Delivery Window")+
  ggtitle("Jugo Puro")+
  theme(plot.title = element_text(hjust = 0.5))

###Mossi Mango
mossidata<-newdata%>%filter(Supplier=="Mossi Mango")
Index7<-mossidata$`Contract Index`
ggplot(data = mossidata,aes(x=Reliability,y=Index7,color=Delivery_Window))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Delivery Window")+
  ggtitle("Mossi Mango")+
  theme(plot.title = element_text(hjust = 0.5))

###	Tampa Fruits
tampadata<-newdata%>%filter(Supplier=="Tampa Fruits")
Index8<-tampadata$`Contract Index`
ggplot(data = mossidata,aes(x=Reliability,y=Index8,color=Delivery_Window))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Delivery Window")+
  ggtitle("Tampa Fruits")+
  theme(plot.title = element_text(hjust = 0.5))

###Seitan Vitamins
seitandata<-newdata%>%filter(Supplier=="Seitan Vitamins")
Index9<-seitandata$`Contract Index`
ggplot(data = seitandata,aes(x=Reliability,y=Index9,color=Delivery_Window))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Delivery Window")+
  ggtitle("Seitan Vitamins")+
  theme(plot.title = element_text(hjust = 0.5))

###YoBoMa
yobomadata<-newdata%>%filter(Supplier=="YoBoMa")
Index10<-yobomadata$`Contract Index`
ggplot(data = yobomadata,aes(x=Reliability,y=Index10,color=Delivery_Window))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Delivery Window")+
  ggtitle("YoBoMa")+
  theme(plot.title = element_text(hjust = 0.5))

###Compare Mango
mangodata<-filter(newdata,Product=="Mango"&Delivery_Window=="4 hours")
ggplot(data = mangodata,aes(x=mangodata$`Delivery Reliability`,y=mangodata$`Contract Index`,color=mangodata$Supplier))+geom_line(size=1.5)+
  labs(x="Reliability",y="Contract Index",color="Suppliers")+
  ggtitle("Mango suppliers (delivery window 4 hours)")+
  theme(plot.title = element_text(hjust = 0.5))


