message( paste( rep('-', 100 ), collapse = '' ) )
#Cargando Datos-------------------------------------------------------------------------------------
message("\tCargando prestaciones")
load(paste0(parametros$RData, "IESS_contexto_economico.RData"))
load(paste0(parametros$RData, "IESS_prestaciones.RData"))
load(paste0(parametros$RData, "IESS_Reg_Civil.RData"))

#Cálculo de indemnizaciones-------------------------------------------------------------------------

indemnizaciones <- prestaciones %>%
  filter( tipo_seguro == 'RT',
          tipo_prestacion == 'ID') %>%
  mutate( fecha_derecho = as.Date( fecha_derecho, "%d/%m/%Y")) %>%
  mutate( fecha_acuerdo = as.Date( fecha_acuerdo, "%d/%m/%Y") ) %>%
  dplyr::select( -novedad ) %>%
  filter( estado_prestacion =='A') %>%
  mutate( anio = year( fecha_acuerdo ) )


sbu <- sbu %>%
  dplyr::select( anio,
                 sbu )

calculo_ind_pp <- indemnizaciones %>%
  dplyr::select( asegurado,
                 anio,
                 fecha_acuerdo,
                 tipo_seguro,
                 tipo_prestacion,
                 promedio_sueldo_teorico,
                 promedio_sueldo_real,
                 coeficiente_teorico,
                 coeficiente_real,
                 #valor_pension_teorica_ajustada,
                 valor_pension_concedida ) %>%
  mutate( anio = if_else( asegurado %in% c('2100048319',
                                           '0301058616',
                                           '0923487581',
                                           '1714076377' ),
                          anio - 1,
                          anio ) ) %>%
  left_join(., sbu, by ='anio') %>%
  mutate( valor_pension_teorica =  round( coeficiente_teorico * promedio_sueldo_real * 60, 2 ) ) %>%
  mutate( max_100 = 100 * sbu ) %>%
  mutate( max_50 = 50 * sbu ) %>%
  mutate( max_40 = 40 * sbu ) %>%
  mutate( filtro = if_else( valor_pension_concedida < valor_pension_teorica,
                            1,
                            0 ) )
  

rc <- rc %>%
  dplyr::select( cedula,
                 sexo, 
                 fecha_nacimiento )

calculo_ind_pp <- calculo_ind_pp %>%
  left_join(., rc, by = c('asegurado'='cedula')) %>%
  filter( !is.na( fecha_nacimiento ) )




#Tabla de coeficientes por edad y sexo--------------------------------------------------------------

coef_ind_pp_edad_sexo <- calculo_ind_pp %>%
  mutate(edad = round(age_calc( fecha_nacimiento,
                                enddate = fecha_acuerdo,
                                units = "years",
                                precise = FALSE ) )) %>%
  group_by( sexo, edad ) %>%
  mutate( coef_ind_pp = mean( coeficiente_teorico, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( sexo, edad, .keep_all = TRUE ) %>%
  dplyr::select( sexo, edad, coef_ind_pp ) %>%
  arrange( sexo, edad )


#Tabla beneficiarios que alcanzaron máximas históricos por año y sexo-------------------------------

aux <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  filter( filtro == '1' )


aux_1 <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  filter( filtro == '1' ) %>%
  group_by( sexo, anio ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, freq) %>%
  spread(  ., sexo, value = c(freq ),  sep = "ben" ) %>%
  replace(is.na(.), 0) %>%
  mutate( benT =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))
  
aux_2 <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  filter( filtro == '1' ) %>%
  group_by( sexo, anio ) %>%
  mutate( ind_prom = mean(valor_pension_teorica, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, ind_prom) %>%
  spread(  ., sexo, value = c(ind_prom ),  sep = "ind_prom" ) %>%
  replace(is.na(.), 0)


aux_3 <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  filter( filtro == '1' ) %>%
  group_by( anio ) %>%
  mutate( ind_prom = mean(valor_pension_teorica, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, ind_prom) %>%
  replace(is.na(.), 0)

aux_4 <- data.frame( anio = 'Total',
                     sexoind_promF = mean( filter( aux, sexo == 'F')$valor_pension_teorica, na.rm = TRUE ),
                     sexoind_promM = mean( filter( aux, sexo == 'M')$valor_pension_teorica, na.rm = TRUE ),
                     ind_prom = mean( aux$valor_pension_teorica, na.rm = TRUE ) )
                     

aux_5 <- aux_2 %>%
  left_join(., aux_3, by = 'anio' ) %>%
  rbind( ., aux_4 )

aux_6 <- aux %>%
  distinct(., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, max_100 ) %>%
  mutate( anio = as.character( anio ) )
  
tab_ind_max <- aux_1 %>%
  left_join( ., aux_5, by = 'anio' ) %>%
  left_join(., aux_6, by = 'anio' )



#Tabla beneficiarios que alcanzaron máximas 50 por año y sexo---------------------------------------

aux <- calculo_ind_pp %>%
  mutate( filtro = if_else( max_50 < valor_pension_teorica,
                            1,
                            0 ) ) %>%
  filter( filtro == '1')


aux_1 <- aux %>%
  group_by( sexo, anio ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, freq) %>%
  spread(  ., sexo, value = c(freq ),  sep = "ben" ) %>%
  replace(is.na(.), 0) %>%
  mutate( benT =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))

aux_2 <- aux %>%
  group_by( sexo, anio ) %>%
  mutate( ind_prom = mean(valor_pension_teorica, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, ind_prom) %>%
  spread(  ., sexo, value = c(ind_prom ),  sep = "ind_prom" ) %>%
  replace(is.na(.), 0)


aux_3 <- aux %>%
  group_by( anio ) %>%
  mutate( ind_prom = mean(valor_pension_teorica, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, ind_prom) %>%
  replace(is.na(.), 0)

aux_4 <- data.frame( anio = 'Total',
                     sexoind_promF = mean( filter( aux, sexo == 'F')$valor_pension_teorica, na.rm = TRUE ),
                     sexoind_promM = mean( filter( aux, sexo == 'M')$valor_pension_teorica, na.rm = TRUE ),
                     ind_prom = mean( aux$valor_pension_teorica, na.rm = TRUE ) )


aux_5 <- aux_2 %>%
  left_join(., aux_3, by = 'anio' ) %>%
  rbind( ., aux_4 )

aux_6 <- aux %>%
  distinct(., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, max_50 ) %>%
  mutate( anio = as.character( anio ) )

tab_ind_max_50 <- aux_1 %>%
  left_join( ., aux_5, by = 'anio' ) %>%
  left_join(., aux_6, by = 'anio' )



#Tabla beneficiarios que alcanzaron máximas 40 por año y sexo---------------------------------------

aux <- calculo_ind_pp %>%
  mutate( filtro = if_else( max_40 < valor_pension_teorica,
                            1,
                            0 ) ) %>%
  filter( filtro == '1')

aux_1 <- aux %>%
  group_by( sexo, anio ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, freq) %>%
  spread(  ., sexo, value = c(freq ),  sep = "ben" ) %>%
  replace(is.na(.), 0) %>%
  mutate( benT =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))

aux_2 <- aux %>%
  group_by( sexo, anio ) %>%
  mutate( ind_prom = mean(valor_pension_teorica, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, ind_prom) %>%
  spread(  ., sexo, value = c(ind_prom ),  sep = "ind_prom" ) %>%
  replace(is.na(.), 0)


aux_3 <- aux %>%
  group_by( anio ) %>%
  mutate( ind_prom = mean(valor_pension_teorica, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, ind_prom) %>%
  replace(is.na(.), 0)

aux_4 <- data.frame( anio = 'Total',
                     sexoind_promF = mean( filter( aux, sexo == 'F')$valor_pension_teorica, na.rm = TRUE ),
                     sexoind_promM = mean( filter( aux, sexo == 'M')$valor_pension_teorica, na.rm = TRUE ),
                     ind_prom = mean( aux$valor_pension_teorica, na.rm = TRUE ) )

aux_5 <- aux_2 %>%
  left_join(., aux_3, by = 'anio' ) %>%
  rbind( ., aux_4 )

aux_6 <- aux %>%
  distinct(., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, max_40 ) %>%
  mutate( anio = as.character( anio ) )

tab_ind_max_40 <- aux_1 %>%
  left_join( ., aux_5, by = 'anio' ) %>%
  left_join(., aux_6, by = 'anio' )

#Tabla afectados por máxima por año y sexo----------------------------------------------------------

aux_100 <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  filter( filtro == '1' ) %>%
  group_by( sexo, anio ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, freq) %>%
  spread(  ., sexo, value = c(freq ),  sep = "b_100_" ) %>%
  replace(is.na(.), 0) %>%
  mutate( T_100 =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))


aux_50 <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  mutate( filtro = if_else( max_50 < valor_pension_teorica,
                            1,
                            0 ) ) %>%
  filter( filtro == '1' ) %>%
  group_by( sexo, anio ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, freq) %>%
  spread(  ., sexo, value = c(freq ),  sep = "b_50_" ) %>%
  replace(is.na(.), 0) %>%
  mutate( T_50 =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))

aux_40 <- calculo_ind_pp %>%
  filter( anio < 2023 ) %>%
  mutate( filtro = if_else( max_40 < valor_pension_teorica,
                            1,
                            0 ) ) %>%
  filter( filtro == '1' ) %>%
  group_by( sexo, anio ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, freq) %>%
  spread(  ., sexo, value = c(freq ),  sep = "b_40_" ) %>%
  replace(is.na(.), 0) %>%
  mutate( T_40 =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))

tab_ind_ben <- aux_100 %>%
  left_join( ., aux_50, by = 'anio' ) %>%
  left_join( ., aux_40, by = 'anio' ) 

# #Guardando en un Rdata----------------------------------------------------------------------------
message( '\tGuardando en un solo data.frame' )

save( calculo_ind_pp,
      coef_ind_pp_edad_sexo,
      tab_ind_max,
      tab_ind_max_50,
      tab_ind_max_40,
      tab_ind_ben,
      file = paste0( parametros$RData, 'IESS_tab_indemnizaciones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
