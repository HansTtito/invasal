
generate_new_vars = function(data, months_prev = c(11, 12)){

  data$IGS <- (data$GONW_G/data$TW_G)*100

  new_year <- ifelse(data$MONTH %in% months_prev, data$YEAR + 1, data$YEAR)

  data$season <- paste0((new_year - 1),"-", new_year)

  return(data)

}


clean_data = function(data){

  data_inicial = data

  years_eliminados = sum(is.na(data$YEAR))

  if(years_eliminados > 0){
    cat("Hay", years_eliminados, "registros que no cuentan con dato de año, serán eliminados\n\n")
  }

  sex_NA_eliminados = sum(is.na(data$SEX))

  if(sex_NA_eliminados > 0){
    cat("Hay", sex_NA_eliminados, "registros con sexo NA, pasará a ser Indeterminado\n\n")
  }

  sexo_eliminados = table(data[!(data$SEX %in% c("Hembra","Macho")),]$SEX)

  if(length(sexo_eliminados) > 0){
    for(i in 1:length(sexo_eliminados)){
      cat("Hay", as.numeric(sexo_eliminados[i]), "registros con sexo", names(sexo_eliminados[i]),", pasará a ser Indeterminado\n\n")
    }
  }

  data$SEX = ifelse(!(data$SEX %in% c("Hembra","Macho")), "Indet", data$SEX)
  data_final = data[!is.na(data$YEAR),]


  if(nrow(data_inicial) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data_inicial) - nrow(data_final)),"filas de los datos\n\n")
  }


  if(sum(is.na(data_final$FL_MM))!=sum(is.na(data_final$FL_CM))){
    cat("Hay", sum(is.na(data_final$FL_MM)), "registros de NA en FL_MM y", sum(is.na(data_final$FL_CM)), " registros de NA en FL_CM; se recomienda revisar\n\n")
  }

  if(sum(is.na(data_final$TL_MM))!=sum(is.na(data_final$TL_CM))){
    cat("Hay", sum(is.na(data_final$TL_MM)), "registros de NA en TL_MM y", sum(is.na(data_final$TL_CM)), " registros de NA en TL_CM; se recomienda revisar\n\n")
  }

  return(data_final)

}


filter_only_male_female = function(data) {

  sex_NA_eliminados = sum(is.na(data$SEX))

  if(sex_NA_eliminados > 0){
    cat("Hay", sex_NA_eliminados, "registros con sexo NA, serán eliminados\n\n")
  }

  sexo_eliminados = table(data[!(data$SEX %in% c("Hembra","Macho")),]$SEX)

  if(length(sexo_eliminados) > 0){
    for(i in 1:length(sexo_eliminados)){
      cat("Hay", as.numeric(sexo_eliminados[i]), "registros con sexo", names(sexo_eliminados[i]),", serán eliminados\n\n")
    }
  }

  data_final = data[data$SEX %in% c("Macho","Hembra"),]

  if(nrow(data) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data) - nrow(data_final)),"filas de los datos\n\n")
  }

  return(data_final)

}


filter_only_length = function(data, umbral_length = 150) {

  if(any(data$TL_CM > umbral_length, na.rm = TRUE)){
    cat("Hay", sum(data$TL_CM > umbral_length, na.rm  = TRUE) ,"tallas de más de",umbral_length, "cm, serán elimminadas; se recomienda revisar\n\n")
    table(data[data$TL_CM > umbral_length,]$TL_CM)
  }

  if(sum(is.na(data$TL_CM)) > 0){
    cat("Hay", sum(is.na(data$TL_CM)) ,"tallas con valores NA, serán elimminadas; se recomienda revisar\n\n")
  }

  data = data[data$TL_CM <= umbral_length,]
  data_final = data[!is.na(data$TL_CM),]

  if(nrow(data) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data) - nrow(data_final)),"filas de los datos\n\n")
  }

  return(data_final)

}



filter_only_IGS = function(data, umbral_IGS = 15) {

  if(any(data$IGS > umbral_IGS, na.rm = TRUE)){
    cat("Hay", sum(data$IGS > umbral_IGS, na.rm = TRUE) ,"valores de IGS mayores a ",umbral_IGS,", serán elimminadas; se recomienda revisar\n\n")
    table(data[data$IGS > umbral_IGS,]$IGS)
  }


  if(sum(is.na(data$IGS)) > 0){
    cat("Hay", sum(is.na(data$IGS)) ,"valores de IGS con NA, serán elimminadas; se recomienda revisar\n\n")
  }

  data_final = data[data$IGS <= umbral_IGS & !is.na(data$IGS),]

  if(nrow(data) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data) - nrow(data_final)),"filas de los datos\n\n")
  }

  return(data_final)

}


processing_data_land_n = function(data, start_year){

  first_years = seq(from = start_year, length.out = (ncol(data)-1))
  second_years = seq(from = start_year + 1, length.out = (ncol(data)-1))

  seasons = paste0(first_years, "-", second_years)

  names(data) = c("fecha", seasons)

  data = data %>% gather(season, var, -fecha)

  return(data)

}
