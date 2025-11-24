library(stats)
library(cluster)

df <- USArrests
df <- na.omit(df)
df <- scale(df)



# -------------------------------------------------------------------------


d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method = "complete")
plot(hc1, cex = 0.6, hang = -1)



# -------------------------------------------------------------------------

hc_single <- agnes(df, method = "single")
hc_complete <- agnes(df, method = "complete")
hc_average <- agnes(df, method = "average")

print(hc_single$ac) # 0.6276128

print(hc_complete$ac) # 0.8531583 = best

print(hc_average$ac) # 0.7379371

pltree(hc_complete, cex = 0.6, hang = -1, main = "Dendrogram of agnes")


# -------------------------------------------------------------------------


# using hclust which is better for visualization
df <- USArrests
df <- scale(df)

d <- dist(df, method = "euclidean")
hc_complete <- hclust(d, method = "complete")
plot(hc_complete, cex = 0.6)
rect.hclust(hc_complete, k = 5, border = 1:4)


# ---------------NOTE THAT YOU CAN CHANGE OTHER ONES TO HCLUST WITH AS.HCLUST 
pltree(ag.ag)
ck3 <- cutree(ag.ag, k=3)
ch6 <- cutree(as.hclust(ag.ag), h=6)
stopifnot(identical(unname(ch6),ck3))



# trying diana ------------------------------------------------------------

hc_diana <- diana(df)
hc_diana$dc # 0.8514345
pltree(hc_diana, cex = 0.6, hang = -1, main = "Dendrogram of diana")





  
  