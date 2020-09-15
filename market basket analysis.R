#Probabilistic modelling: Market Basket Analysis for the 100 most purchased items

#exploratory analysis

df_7_rfm_clean

df_7_rfm_clean$TIC_HOUR <- as.factor(df_7_rfm_clean$TIC_HOUR)
df_7_rfm_clean %>%
  ggplot(aes(x=TIC_HOUR)) + 
  geom_histogram(stat="count",fill="indianred")

df_7_rfm_clean$ID_SCONTRINO <- as.factor(df_7_rfm_clean$ID_SCONTRINO)
a <- df_7_rfm_clean %>% 
  group_by(ID_SCONTRINO) %>% 
  summarise(n_items = n()) %>%
  filter(n_items> 0)  
a %>% ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

df_7_rfm_clean$ID_ARTICOLO <- as.factor(df_7_rfm_clean$ID_ARTICOLO)

most_purchased <- df_7_rfm_clean %>%
  group_by(ID_ARTICOLO) %>% summarise(count = n()) %>% arrange(desc(count))

#association rule

retail_sorted <- df_7_rfm_clean[order(df_7_rfm_clean$ID_CLI),]
library(plyr)
itemList <- ddply(df_7_rfm_clean,c("ID_CLI","TIC_DATETIME"), 
                  function(df1)paste(df1$ID_ARTICOLO, 
                                     collapse = ","))
itemList$ID_CLI <- NULL
itemList$TIC_DATETIME <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=100, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

plot(rules[1:10], method="graph")
#plot(rules[1:10], method = "grouped")
