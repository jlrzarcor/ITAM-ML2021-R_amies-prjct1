
#colnames(canc_data_entr) <- canc_data_entr_clean_colnames(canc_data_entr_colnames)
problematic_rows_trn <- problems(hotel_cr_train)$row

#hotel_cr_train <- hotel_cr_train %>% mutate_at(c(16),list(canc_clean_data_ent))
hotel_cr_train <- readr::type_convert(hotel_cr_train, na = 'NULL', trim_ws = TRUE)
hotel_cr_train <- readr::type_convert(hotel_cr_train)

problematic_rows_tst <- problems(hotel_cr_test)$row

#hotel_cr_test <- hotel_cr_test %>% mutate_at(c(16),list(canc_clean_data_ent))
hotel_cr_test <- readr::type_convert(hotel_cr_test, na = 'NULL', trim_ws = TRUE)
hotel_cr_test <- readr::type_convert(hotel_cr_test)


## Using we can infere and declare categorical vars. a priori
#canc_data_entr  <- canc_data_entr %>% 
#  mutate_at(c(1,3:9,15,16,18),list(as.factor))
