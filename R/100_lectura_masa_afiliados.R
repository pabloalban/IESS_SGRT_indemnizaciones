message( paste( rep('-', 100 ), collapse = '' ) )

#Cargando información financiera--------------------------------------------------------------------
file_afiliados <- paste0(parametros$Data, 'IESS_afiliados_sexo_edad.xlsx' )
file_afi_mensual <- paste0(parametros$Data, 'IESS_afiliados_mensual.xlsx' )
file_masa_anual <- paste0(parametros$Data, 'IESS_masa_salarial.xlsx' )
#Carga de información-------------------------------------------------------------------------------
message( '\tLectura de los afiliados del SGO' )
afiliados_sgo_edad_sexo <- read_excel( file_afiliados,
                                       sheet = 1,
                                       col_names = TRUE,
                                       col_types = NULL,
                                       na = "",
                                       skip = 0 ) %>% clean_names()

afi_edad_sexo <- afiliados_sgo_edad_sexo %>%
  mutate( anio = year( afiliados_sgo_edad_sexo$fecha_corte) ) %>%
  filter( anio == 2022 ) %>%
  mutate( edad = if_else( edad > 115,
                          115,
                          edad ) ) %>%
  replace(is.na(.), 0) %>%
  group_by( edad ) %>%
  mutate( femenino = sum( femenino, na.rm = TRUE ),
          masculino = sum( masculino, na.rm = TRUE ),
          total = sum( total, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., edad, .keep_all = TRUE ) %>%
  mutate( fdp_f = femenino / sum( total, na.rm = TRUE ),
          fdp_m = masculino / sum( total, na.rm = TRUE ) ) %>%
  dplyr::select( anio,
                 edad,
                 masculino,
                 femenino,
                 total,
                 fdp_m,
                 fdp_f )

message( '\tLectura de la afiliados mensual' )

afi_mensual <- read_excel( file_afi_mensual,
                                sheet = 1,
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0 ) %>% clean_names()



message( '\tLectura de la masa anual' )

masa_salarial <- read_excel( file_masa_anual,
                                sheet = 1,
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0 ) %>% clean_names()


#Guardar RData--------------------------------------------------------------------------------------
message( '\tGuardando Rdatas' )
save( afi_edad_sexo,
      afi_mensual,
      masa_salarial,
      file = paste0( parametros$RData, 'IESS_SGO_masa_afiliados.RData' ) )

#Limpiar RAM----------------------------------------------------------------------------------------
message( '\tLimpiando Ram' )
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
