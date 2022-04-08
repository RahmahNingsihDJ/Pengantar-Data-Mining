#mengambil data
data_grocery=read.csv("D:/Zulkarnain/Semester 6/Pengantar Data Mining/GroceryStoreDataBaru.csv", colClasses="factor")
View(data_grocery)
str(data_grocery)

# Menggunakan library arules
library(arules)
datatransaksi=apriori(data_grocery)
datatransaksi
summary(datatransaksi)

#reduce smaller number of rules
datatransaksi1=apriori(data_grocery, parameter=list(minlen=2,maxlen=3,supp=.7))
inspect(datatransaksi1)

#finding interesting rules
summary(data_grocery)
datatransaksi2=apriori(data_grocery, parameter=list(minlen=2,maxlen=3,conf=.7),
                      appearance = list(rhs=c("BREAD=Yes"),default="lhs"))
inspect(datatransaksi2)

#finding interesting rules
datatransaksi3=apriori(data_grocery, parameter=list(minlen=2,maxlen=3,supp=.1,conf=.5),
                      appearance = list(rhs=c("BREAD=Yes"),lhs=c("MILK=Yes","BISCUIT=Yes","CORNFLAKES=Yes","TEA=Yes","BOURVITA=Yes","JAM=Yes","MAGGI=Yes","COFFE=Yes","COCK=Yes","SUGER=Yes"),default="none"))
quality(datatransaksi3)=round(quality(datatransaksi3),digits=3)
inspect(datatransaksi3)
plot(datatransaksi3, method="paracoord", control=list(reorder=TRUE))
