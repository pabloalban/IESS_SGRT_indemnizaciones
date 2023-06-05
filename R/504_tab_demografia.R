message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura masa salarial y afiliados' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_SGO_masa_afiliados.RData' ) )
load( file = paste0( parametros$RData, 'IESS_afi_tiempo_aportacion.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla afiliados por edad y sexo--------------------------------------------------------------------
message( '\tTabla afiliados por edad y sexo' )

a <- afi_edad_sexo

cortes_edad <- c( 15, seq( 20, 100, 10 ), 115 )

etiquetas_edad<-c(paste0("(",formatC( cortes_edad[1:length(cortes_edad)-1], 
                                        digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          " - ",formatC( cortes_edad[2:length(cortes_edad)], 
                                        digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

aux <- a %>%
  mutate(rango_edad = cut( edad, 
                            breaks = cortes_edad,                         
                            labels = etiquetas_edad,
                            include.lowest = TRUE,
                            right = TRUE ) ) %>%
  group_by( rango_edad ) %>%
  mutate( rango_m = sum( masculino, na.rm = TRUE ),
          rango_f = sum( femenino, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( rango_edad, .keep_all = TRUE ) %>%
  dplyr::select( rango_edad,
                 rango_m,
                 rango_f ) %>%
  na.omit(.) %>%
  mutate( total = rango_m + rango_f,
          fdp_m = 100 * rango_m / sum( total ),
          fdp_f = 100 * rango_f / sum( total ),
          fdp_t = fdp_m + fdp_f ) %>%
  dplyr::select( rango_edad,
                 rango_m,
                 fdp_m,
                 rango_f,
                 fdp_f,
                 total,
                 fdp_t ) %>%
  mutate( rango_edad = as.character( rango_edad ) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  mutate_at( c( 2, 4, 6 ), as.integer ) %>%
  mutate_at( c( 3, 5, 7 ), as.numeric )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rangos_afiliados_sgo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

#Tabla masa salarial--------------------------------------------------------------------------------
message( '\tTabla masa salarial' )

aux <- masa_salarial %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( incremento = masa_salarial - lag( masa_salarial ),
          tc = tasa_crecimiento ) %>%
  dplyr::select( -tasa_crecimiento )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 7 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla afiliados------------------------------------------------------------------------------------
message( '\tTabla afiliados anuales' )

aux <- afi_mensual %>%
  filter( mes == '12', anio <= 2022 ) %>%
  mutate( por_afi_f = 100 * afi_femenino / afiliados,
          por_afi_m = 100 * afi_masculino / afiliados,
          incremento = ( afiliados - lag( afiliados ) ),
          tasa_crecimiento = 100 * ( afiliados - lag( afiliados ) ) / lag( afiliados ) ) %>%
  dplyr::select( anio,
                 afi_masculino,
                 por_afi_m,
                 afi_femenino,
                 por_afi_f,
                 afiliados,
                 incremento,
                 tasa_crecimiento ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 0, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_afliados', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tiempo de aportación de afiliados------------------------------------------------------------------
message( '\tTabla de Tiempo de aportación de afiliados' )
aux <- copy( afi_tiempo_aportacion )
aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

#Tabla evolución de afiliados-----------------------------------------------------------------------
message( '\tTabla afiliados' )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
gc()
