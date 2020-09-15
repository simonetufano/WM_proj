#CHURN MODEL
#Analisi esplorative e creazione della variabile target
max(df_7_tic_clean_final$TIC_DATE)
#ultima data: 30/04/2019

df<- df_7_tic_clean_final

df <- df[order(df$ID_CLI,rev(df$TIC_DATE)),]
dft2 <- df %>%
  group_by(ID_SCONTRINO) %>% 
  summarise(ID_CLI = max(ID_CLI),DATETIME=max(TIC_DATE))
dft2 <- dft2[order(dft2$ID_CLI,rev(dft2$DATETIME)),]

#Calcoliamo il numero di acquisti per ogni cliente
dft3 <- dft2 %>% group_by(ID_CLI) %>% summarise(tot = n()) %>% filter(tot>1)

#Si aggiunge id_scontrino & datetime
dft3 <- left_join(dft3,dft2,by="ID_CLI") 
dft4 <- dft3 %>% 
  arrange(desc(DATETIME)) %>% 
  group_by(ID_CLI) %>% 
  summarise(last=nth(DATETIME,1),secondl=nth(DATETIME,2))

#p <- ggplot(dft4, aes(x= as.numeric(last - secondl))) + 
#  geom_histogram(color="black", fill="lightblue") +
#  geom_vline(aes(xintercept = 60), color="blue", linetype="dashed", size=1) +
#  labs(title = "Ultimo acquisto - penultimo acquisto", x = "days", y = "frequency") +
#  scale_x_continuous(breaks=seq(0,300,30)) +
#  theme_minimal()

#Ipotizzando un periodo di churn pari a 2 mesi (o 60 giorni) si ha una percentuale di 
#di clienti churner pari a circa il 20% e, ovviamente, considerandola come variabile target
#siamo di fronte a un problema con classi sbilanciate.

q <- ggplot(dft4, aes(as.numeric(last-secondl), cumsum(stat(count)/nrow(dft4)))) +
  geom_freqpoly(binwidth = 8,alpha=0.8,col="black") +
  labs(title = "Percentuale cumulativa di riacquisto", x = "days", y = "Cumulative Percentage of Repurchase") +
  geom_line(data = data.frame(days=1:365,const=0.77),aes(days,const),col="blue") +
  geom_line(data = data.frame(y=seq(0,1,0.1),x=60),aes(x,y),col="blue") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_classic()
q

#Si crea quindi la variabile target da inserire all'interno del dataframe

df_churn <- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(TIC_DATE),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n())   %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE < as.Date("2019-03-01"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)

table(df_churn$CHURN)
#aggiungiamo una variabile in riferimento alla caratterizzazione del giorno ed estrapoliamo
#i mesi e l'anno dalla data, in modo da rimuoverla come variabile per l'analisi e iniziare
#un primo approccio in modo tale da avere abbastanza informazione e dettaglio

df_churn <- df_churn %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(LAST_PURCHASE_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", LAST_PURCHASE_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

df_churn$Mese <- as.factor(months(df_churn$LAST_PURCHASE_DATE))
df_churn$Anno <- as.factor(year(df_churn$LAST_PURCHASE_DATE))

#Si procede con la creazione del dataset per le analisi utilizzando anche i precedenti
#e unendoli attraverso operazioni di join

df_tot <- df_1_cli_fid_clean %>%
  select(ID_CLI, FIRST_ID_NEG, LAST_TYP_CLI_FID, LAST_COD_FID, LAST_STATUS_FID) %>%
  left_join(df_2_cli_account_clean 
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  select(-ID_ADDRESS)

df_tot_churn <- df_churn %>%
  left_join(df_tot, by="ID_CLI")%>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION))

#df_tot_churn <- df_tot_churn %>% left_join(df_RFM[,c('ID_CLI','recency','frequency','monetary')], 
#                                           by='ID_CLI')

#df_tot_churn <- df_tot_churn %>% left_join(df_churn[,c('ID_CLI','TIC_DATE_WEEKDAY',
#                                                       'TIC_DATE_TYP','Mese','Anno')],by='ID_CLI')

#save(df_tot_churn, file = 'dataset_for_modeling.RData')

#controllo dati mancanti
sapply(df_tot_churn, function(x)(sum(is.na(x)/nrow(df_tot_churn))))

#rimuoviamo le variabili non significative per l'analisi 
df_tot_churn <- df_tot_churn[,c('CHURN', "TOTAL_PURCHASE","NUMBER_OF_PURCHASE",
                                "FIRST_ID_NEG","LAST_TYP_CLI_FID", "LAST_COD_FID",
                                "LAST_STATUS_FID", "W_PHONE","TYP_CLI_ACCOUNT","TYP_JOB",
                                "EMAIL_PROVIDER_CLEAN","REGION","FLAG_PRIVACY_1","FLAG_PRIVACY_2",
                                "FLAG_DIRECT_MKT")]



#trasformazione delle variabili char in factor
df_tot_churn$LAST_COD_FID <- as.factor(df_tot_churn$LAST_COD_FID)
df_tot_churn$FIRST_ID_NEG <- as.factor(df_tot_churn$FIRST_ID_NEG)

df_tot_churn$CHURN <- as.factor(df_tot_churn$CHURN)
str(df_tot_churn)

isfactor <- sapply(df_tot_churn, function(x) is.factor(x))
isfactor
factordata <- df_tot_churn[, isfactor]
str(factordata)
numeric <- sapply(df_tot_churn, function(x) is.numeric(x))
numeric <-df_tot_churn[, numeric]
str(numeric)

#analisi della correlazione
library(caret)

correlatedPredictors = findCorrelation(cor(numeric), cutoff = 0.8, names = TRUE)
correlatedPredictors

library(corrgram)
corrgram(numeric, lower.panel = panel.cor, cex=1, cex.labels = 1)

#si procede creando la partizione tra train e test

table(df_tot_churn$CHURN)

#dataset sbilanciato, 0.4093266 (churn = 0) vs  0.5906734 (churn = 1)

split <- createDataPartition(df_tot_churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- df_tot_churn[split,]
test <- df_tot_churn[-split,]

#undersampling per bilanciare il campione

churn0 <- train %>% filter(CHURN == 0) # 60780 
churn1 <- train %>% filter(CHURN == 1) # 87708 

bilancia <- churn1[sample(nrow(churn1), nrow(churn0)),]

train <- rbind(bilancia, churn0)
table(train$CHURN)
table(test$CHURN)
##MODELLI
#Decision Tree
#il primo serve a valutare l'importanza delle variabili e capire se è ragionevole
#eliminarne alcune per dimuinuire la dimensione del dataset o perchè effettivamente
#non sono discriminanti per l'analisi

library(rpart)
library(rpart.plot)

default.ct <- rpart(CHURN ~ ., data = train)

deeper.ct <- rpart(CHURN ~ ., data = train, 
                   method = "class", cp = 0, minsplit = 1)

rpart.plot(deeper.ct, type = 4, extra = 1)
deeper.ct

a=data.frame(deeper.ct$frame$var)
table(a)
vi=data.frame(deeper.ct$variable.importance)
vi
deeper.ct$cptable

which.min(deeper.ct$cptable[,"xerror"])
# cp = 0.000090490293 nsplit = 6 rel_error = 0.0032576505 xerror = 0.003290556
pruned <- prune(deeper.ct, 
                 cp = deeper.ct$cptable[which.min(deeper.ct$cptable[,"xerror"]),"CP"])
a=data.frame(pruned$frame$var)
table(a)
vi=data.frame(pruned$variable.importance)
vi

rpart.plot(pruned, type = 4, extra = 1)
prp(pruned, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10, 
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white')) 

save(train,test,df_tot_churn, file = 'partizioni_def.RData')


confusionMatrix(pred_tree, test$CHURN)
pred_tree <- predict(pruned, test[,-1],type = "class") #0.6288

#Random Forest
library(dplyr)
level_key <- c('1' = "c1", '0' = "c0")
train$CHURN <- recode_factor(train$CHURN, !!!level_key)
test$CHURN <- recode_factor(test$CHURN, !!!level_key)

unique(train$CHURN)
library(randomForest)
tree_rf <- randomForest(CHURN ~ ., data= train, ntree = 100)
print(tree_rf)

pred_rf <- predict(tree_rf, test[,-1],type = "class") #0.6373
confusionMatrix(pred_rf, test$CHURN)

library(e1071)
#Naive Bayes
nb <- naiveBayes(CHURN ~ ., train )
print(nb)

pred_nb <- predict(nb, test[,-1],type = "class")
confusionMatrix(pred_nb, test$CHURN) #0.6272 

#Logistic model
logistic_model <- glm(CHURN ~ . , data = train, family="binomial")
print(logistic_model)
pred_lm <- predict(logistic_model, test[-1], type='response')
pred_lm <- as.factor(if_else(pred_lm > 0.5,'c1','c0')) #soglia di default
confusionMatrix(pred_lm, test$CHURN) #0.3521



#bagging
library(ipred)
bag <- bagging(CHURN ~ ., train, nbagg = 25)
print(bag)
pred_bag <- predict(bag, test[-1], type='class')
confusionMatrix(pred_bag, test$CHURN) #0.6085  




#previsti
B <- data.frame(c(1:63636) ,0)
B$pred_tree <- as.numeric(pred_tree)
B$pred_rf <- as.numeric(pred_rf)
B$pred_nb <- as.numeric(pred_nb)
B$pred_lm <- as.numeric(pred_lm)
B$pred_bag <- as.numeric(pred_bag)



#confronto
library(pROC)
length1=roc(test$CHURN ~ pred_tree, data = B)
length2=roc(test$CHURN ~ pred_rf, data = B)
length3=roc(test$CHURN ~ pred_lm, data = B)
length4=roc(test$CHURN ~ pred_nb, data = B)
length5=roc(test$CHURN ~ pred_bag, data = B)

plot(length1) #tree
plot(length2,add=T,col="red") # foresta
plot(length3,add=T,col="blue") # logistico
plot(length4,add=T,col="orange") # nb
plot(length5,add=T,col="green") # bagging

#gain lift

library(funModeling)
test2 <- test
test2$rf = predict(tree_rf,test2, "prob")[,2]
gain_lift(data = test2, score = 'rf', target = 'CHURN')
test2$lm = predict(logistic_model,test2, "response")
gain_lift(data = test2, score = 'lm', target = 'CHURN')

#vince random forest 

