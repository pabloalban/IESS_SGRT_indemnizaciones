message( paste( rep('-', 100 ), collapse = '' ) )

#Lectura de bases-----------------------------------------------------------------------------------

message( '\tLeyendo base de empleadores a julio 2022' )

file_empleadores <- paste0( parametros$Data, 'KSPCOTEMPLEADORES.txt' )
file_cod_empleadores <- paste0( parametros$Data, 'tipo_empleador.xlsx' )

tipo_empleador <- read_excel(file_cod_empleadores,
                             sheet = 'Hoja1',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names() %>%
  dplyr::select( codtipemp,
                 codsec,
                 destipemp )


sector_empleador <- read_excel(file_cod_empleadores,
                               sheet = 'Hoja2',
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names()

empleadores <- (read.table(file_empleadores,
                           skip=0,
                           dec = ".",
                           header = TRUE,
                           fill = TRUE,
                           sep = ";",
                           na.strings = "NA",
                           encoding="UTF-8",
                           row.names = NULL )) %>%
  clean_names() %>%
  dplyr::select( -codsegsoc ) %>%
  left_join( ., tipo_empleador, by = 'codtipemp') %>%
  left_join( ., sector_empleador, by = 'codsec') %>%
  dplyr::select( rucemp,
                 numtrarea,
                 destipemp,
                 sector,
                 nomemp )


#Guardar en Rdatas----------------------------------------------------------------------------------
message("\tGuardando Rdatas")
save( empleadores,
      file = paste0( parametros$RData_seg, 'IESS_empleadores.RData' ) )
  
#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

