#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_dist_importosconto_reparto <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_reparto <- df7_dist_importosconto_reparto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_reparto

## plot aggregate
plot_df7_dist_importo_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto, aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill = 'white') +
    theme_minimal()
)

plot_df7_dist_importo_reparto

## plot aggregate
plot_df7_dist_sconto_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto, aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill = 'white') +
    theme_minimal()
)

plot_df7_dist_sconto_reparto

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

articolo_distribution <- df_7_tic_clean_final %>% 
                count(ID_ARTICOLO) %>% arrange(desc(n))

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

df7_dist_importosconto_cliente <- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cliente <- df7_dist_importosconto_cliente %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_cliente

## plot aggregate
plot_df7_dist_importo_cliente <- (
  ggplot(data=df7_dist_importosconto_cliente, aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill = 'white') +
    theme_minimal()
)

plot_df7_dist_importo_cliente

## plot aggregate
plot_df7_dist_sconto_cliente <- (
  ggplot(data=df7_dist_importosconto_cliente, aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill = 'white') +
    theme_minimal()
)

plot_df7_dist_sconto_cliente
# compute the distribution of customers by number of purchases (as described in the slides)

customer_distribution <- df_7_tic_clean_final %>% filter(DIREZIONE == 1) %>%
  count(ID_CLI) %>% arrange(desc(n))



breaks <- c(0,3,10,20,50,100,1000,1995)
tags <- c("[0-3)","[3-10)","[10-20)", "[20-50)","[50-100)", "[100-1000)", "[1000-1995)")

group_tags <- cut(customer_distribution$n, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE,
                  labels = tags)
# inspect bins
summary(group_tags)

ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f", ..count../length(group_tags))), vjust=-0.5) +
  labs(x='number of purchases') +
  theme_minimal() 

# compute the days for next purchase curve (as described in the slides)

data_for_next_purchase <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>% select(ID_CLI,ID_ARTICOLO,TIC_DATE, DIREZIONE) %>%
  arrange(ID_CLI)
data_for_next_purchase

#sample of data_for_next_purchase
#sample_data <- data_for_next_purchase[1:100000]

df_np <- data_for_next_purchase %>%
  group_by(ID_CLI) %>%
  mutate(Diff = TIC_DATE - lag(TIC_DATE))

df_np_gb <- df_np %>% group_by(ID_CLI) %>% na.omit() %>%
  summarize(AVG_next_purchase = mean(Diff)) %>%
  arrange(AVG_next_purchase)

ggplot(df_np_gb, aes(x=AVG_next_purchase, y=cumsum(as.numeric(AVG_next_purchase, units= 'days')))) + 
  geom_line() + geom_point() + labs(x="Average days to next purchases", y = "Number of customers")

#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)


#aggiornamento datamart

dm5 <- inner_join(dm4, df_7_tic_clean_final[,-1], by='ID_CLI')
