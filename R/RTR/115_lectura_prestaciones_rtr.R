message( paste( rep('-', 100 ), collapse = '' ) )

#Lectura de nomina----------------------------------------------------------------------------------

message( '\tLeyendo tabla prestaciones' )


file_prestaciones <- paste0( parametros$Data, 'PRESTACIONES_2023_01.dsv' )

prestaciones <- (read.table(file_prestaciones,
                            skip=0,
                            dec = ".",
                            header = TRUE,
                            sep = "\t",
                            na.strings = "NA",
                            encoding="UTF-8",
                            row.names = NULL )) %>% clean_names()


# #Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en un solo data.frame' )

save( prestaciones,
      file = paste0( parametros$RData, 'IESS_prestaciones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
