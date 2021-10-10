library(readr)
## ID_F1 --> This function aim to load data if It doesn´t exist
load_tr <- function(){
  hotel_cr_train <- read_csv('../data/entrena.csv', na = 'XXXXXXX')
  #saveRDS(hotel_cr_train, "../data/hotel_cr_train")
  return(hotel_cr_train)
}

load_ts <- function(){
  hotel_cr_test <- read_csv('../data/prueba.csv', na = 'XXXXXXX')
  return(hotel_cr_test)
}


## ID_F2 --> This functions aims to clean data

#autos85_clean_colnames <- function(x){
#  str_replace_all(tolower(x),"/| ",'_')
#  str_replace_all(tolower(x),"-",'_')
#}

clean_data <- function(x){
  # to review --> str_replace_all(tolower(x),"/",'_')
  return(x)
}



# ID_F3 --> NOMBRES DE VARIABLES LIMPIOS Y EN MINÚSCULAS
# Es un vector con los nombres de las columnas, se utiliza en varias funciones

#autos85_colnames_min <- autos85_clean_colnames(autos85_colnames)



# ID_F4 --> DATAFRAME WITH VARIABLE NAMES AND TYPE OF VARIABLE
# Useful to discriminate type of graphing construction

hotel_cr_train_get_coltypes <- function(hotel_cr_train) {
  df_var_type <- as.data.frame(sapply(hotel_cr_train, class))
  df_var_type <- rownames_to_column(.data = df_var_type, "varname")
  colnames(df_var_type) <- c('varname','vartype')
  df_var_type
}



# ID_F5 --> UNIVARIATE PLOTS: 1 NUMERIC VARS, 2 FCTRS VARS
#This function aim to plot all vars from df by numeric.
#For Numeric vars plots --> boxplot; factor vars --> plot bars.

graf_univ_num_h <- function(data, num_bin_his) {
  plot(ggplot(stack(data), aes(x = values)) +
         geom_histogram(fill = "lightgray", col = "steelblue", bins = num_bin_his) +
         facet_wrap(~ind, scale="free", ncol = 5))
  
}
graf_univ_num_b <- function(data) {
  plot(ggplot(stack(data), aes(x = ind, y = values)) +
         geom_boxplot(outlier.colour = "red", outlier.size = 2, varwidth = TRUE) +
         facet_wrap(~ind, scale="free", ncol = 7))
}
graf_univ_fct <- function(data) {
  ##return(data)
  plot(ggplot(data, aes(x = reorder(categ_value,count), y = count)) +
         geom_bar(fill = "lightgray", col = "steelblue", stat="identity") + 
         coord_flip() +
         ##theme_hc() + 
         ylab('conteo') + 
         xlab('Valores Variable Categ.') +
         facet_wrap(~categorical, scale="free", ncol = 5) )
}









##********************* UNDER CONSTRUCION *********************##






# ID_F6 --> BIVARIATE PLOTS: 1 NUMERIC VARS, 2 FCTRS VARS
#This function aim to plot all vars from df by numeric.
#For Numeric vars --> ; factor vars --> .

graf_bi_splom <- function(data){
  ggpairs(data = data,
          title="Precio Autos '85 vs Características Mecánicas", 
          upper = list(contious='smooth_loess'),
          diag=list(continuous='densityDiag'), axisLabels='none',
          progress = FALSE, proportions = "auto")
}




##********************* CLEANING: IMPUTATION *********************##

moda <- function(x) {
  # Función para obetener la moda de variable categorica
  # Inputs:
  # x: columna de donde se quiere extraer la moda
  #
  # Outputs
  # Moda de columna, es decir el valor que más se repite
  z <- table(as.vector(x))
  names(z)[z == max(z)]
}

imputar_valor_central <- function(data, colnames) {
  
  # Función para imputar valores centrales, media en numéricos y moda en categoricos
  # Inputs:
  # data - El tibble de algas
  # colnames - El array de las columnas que se desea imputar
  #
  # Outputs
  # dataframe con imputaciones centrales
  #Dividir entre numericas y categoricas
  
  data_columnas <- data[colnames]
  var_numericas <- dplyr::select_if(data_columnas, is.numeric) %>% names()
  var_categoricas <- dplyr::select_if(data_columnas, is.character) %>% colnames()
  
  #Imputar
  algas_data_imputacion_central <- data %>%
    # variables numéricas (media)
    mutate_at( vars(var_numericas),
               funs(ifelse(is.na(.), median(., na.rm = T), .))) %>%
    # variables categóricas (moda)
    mutate_at(vars(var_categoricas),
              funs(as.ordered(ifelse(is.na(.), moda(.), as.character(.)))))
  return(algas_data_imputacion_central)
}
