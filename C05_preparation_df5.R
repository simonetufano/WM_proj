#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
unique(df_5_camp_cat_clean$TYP_CAMP)

