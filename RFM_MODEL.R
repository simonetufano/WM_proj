#RFM MODEL
#How recently does the customer purchase the last time?
#How often does the customer purchase?
#How much money does the customer spend?

#Poichè ci sono dei duplicati con stesso id scontrino e stesso id cliente,
#ma che differiscono soltanto per lo sconto applicato, è necessario tenere
#solo quelle osservazioni che presentano un importo lordo minore a cui è stato
#applicato lo sconto.

#aggiungere direzione

df_7_rfm_clean <- df_7_tic_clean_final %>% filter(DIREZIONE == 1) %>%
  group_by(ID_CLI, ID_ARTICOLO) %>% top_n(1, IMPORTO_LORDO) %>%
  mutate(IMPORTO = IMPORTO_LORDO - SCONTO)

#per calcolare la recency, confrontiamo nuovamente l'ultima data presente
#e valutiamo i giorni passati dall'ultimo acquisto per ogni cliente

summary(df_7_rfm_clean$TIC_DATE)

analysis_date <- lubridate::as_date("2019-04-30")
df_RFM <- df_7_rfm_clean %>% 
  group_by(ID_CLI) %>% 
  summarise(recency=as.numeric(analysis_date-max(TIC_DATE)),
            frequency =n_distinct(ID_ARTICOLO), monetary= sum(IMPORTO))

summary(df_RFM)

#Scoring
#R_score
df_RFM$R_Score[df_RFM$recency>208.0]<-1
df_RFM$R_Score[df_RFM$recency>107.0  & df_RFM$recency<=208.0 ]<-2
df_RFM$R_Score[df_RFM$recency>36.0 & df_RFM$recency<=107.0 ]<-3
df_RFM$R_Score[df_RFM$recency<=36.0]<-4
#F_score
df_RFM$F_Score[df_RFM$frequency<3.00]<-1
df_RFM$F_Score[df_RFM$frequency>=3.00 & df_RFM$frequency<8.00]<-2
df_RFM$F_Score[df_RFM$frequency>=8.00 & df_RFM$frequency<19.00 ]<-3
df_RFM$F_Score[df_RFM$frequency>=19.00]<-4
#M_score
df_RFM$M_Score[df_RFM$monetary< 75.34]<-1
df_RFM$M_Score[df_RFM$monetary>=75.34 & df_RFM$monetary<220.23]<-2
df_RFM$M_Score[df_RFM$monetary>=220.23 & df_RFM$monetary<609.35   ]<-3
df_RFM$M_Score[df_RFM$monetary>=609.35  ]<-4
#RFM_score
df_RFM<- df_RFM %>% mutate(RFM_Score = 100*R_Score + 10*F_Score + M_Score)

#Dopo aver calcolato i vari punteggi di scoring rispetto alle 3 dimensioni
#si procede con la segmentazione dei clienti

summary(df_RFM$RFM_Score)
#Customer Segmentation
#save(df_RFM, file = 'RFM.RData')


df_RFM$segmentRFM<-0
best_customers <- c(444)
loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
potential_loyalist <- c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
recent_customers <- c(411)
promising <- c(311, 312, 313, 331)
needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
about_to_sleep <- c(211)
at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
cant_lose <- c(134,143,144,234,242,243,244)
hibernating <- c(141)
lost <- c(111)

df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% best_customers)] = "Best Customers"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% loyal_customers)] = "Loyal Customers"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% potential_loyalist)] = "Potential Loyalist"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% recent_customers)] = "Recent customers"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% promising)] = "Promising"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% needing_attention)] = "Customer Needing Attention"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% about_to_sleep)] = "About to Sleep"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% at_risk)] = "At Risk"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% cant_lose)] = "Can't Lose Them"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% hibernating)] = "Hibernating"
df_RFM$segmentRFM[which(df_RFM$RFM_Score %in% lost)] = "Lost"

#Segment size

df_RFM %>%
  count(segmentRFM) %>%
  arrange(desc(n)) %>%
  rename(segmentRFM = segmentRFM, Count = n)

ggplot(data = df_RFM) +
  aes(x = segmentRFM, fill = segmentRFM) +
  geom_bar(fill="steelblue") +
  labs(title = "Customer Segmentation",
       x = "Segment",
       y = "Total Customer") +coord_flip()+
  theme_minimal()

#La segmentazione ha originato 11 tipologie di clienti, caratterizzati dai
#loro comportamenti di acquisto. La maggior parte di essi è a rischio, ossia 
#presentano valori bassi di Recency e Frequency. Significa che sono clienti che non
#comprano da tanto e solitamente non comprano spesso. Poichè sono una grossa
#fetta potrebbe essere ragionevole intraprendere delle azioni mirate come per gli stessi.
#Fortunatamente, la seconda porzione di clienti è caratterizzata da clienti
#'loyal', ossia clienti con punteggi medio-alti su tutte le dimensioni. Questi
#clienti sono tra i più importanti ed è necessario fidelizzarli e richiedere
#recensioni sull'azienda e sui prodotti acquistati per creare engagement.

ggplot(data = df_RFM,
       aes(x = frequency, y = monetary, colour = segmentRFM)) +
  geom_point() +
  theme_dark() +
  labs(title = "amount spent vs. frequency", colour = "cluster")

ggplot(data = df_RFM,
       aes(x = recency, y = monetary, colour = segmentRFM)) +
  geom_point() +
  theme_dark() +
  labs(title = "amount spent vs. recency", colour = "cluster")

ggplot(data = df_RFM,
       aes(x = recency, y = frequency, colour = segmentRFM)) +
  geom_point() +
  theme_dark() +
  labs(title = "recency vs. frequency", colour = "cluster")

#boxplot for explore distribution of cluster
library(dplyr)
library(ggplot2)

df_RFM %>% ggplot( aes(y=segmentRFM, x=recency) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("recency") 

df_RFM %>% ggplot( aes(y=segmentRFM, x=frequency )) +
  geom_boxplot(fill="#69b3a2") +
  xlab("frequency")
  
df_RFM %>%  ggplot( aes(y=segmentRFM,x=monetary)) +
    geom_boxplot(fill="#69b3a2") +
    xlab("monetary")
