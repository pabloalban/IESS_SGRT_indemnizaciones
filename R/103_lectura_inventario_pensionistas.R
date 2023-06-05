message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los pensionistas del IESS' )

load( file = paste0( parametros$RData, 'IESS_inventario_pensionistas.RData' ) )

#Definir codificación de mujer y hombre-------------------------------------------------------------
male <- 'H'
female <- 'M'

#Definiendo variables-------------------------------------------------------------------------------

inventario_jubilados <- inventario_jubilados %>%
  mutate( fecha_derecho = as.Date( fecha_derecho, "%d/%m/%Y"),
          fecha_incapacidad = as.Date( fecha_incapacidad, "%d/%m/%Y" ) )

oldage_sgo <- inventario_jubilados %>%
  mutate( t = year( fecha_derecho ) ) %>%
  mutate( sexo = if_else( sexo == 'F',
                          female,
                          male ) ) %>%
  mutate( fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                      mean( fecha_nacimiento, na.rm = TRUE ),
                                     fecha_nacimiento ) ) %>%
  mutate( fecha_nacimiento = if_else( fecha_derecho < fecha_nacimiento,
                                     fecha_derecho,
                                     fecha_nacimiento ) ) %>%
  mutate( x = round(age_calc( fecha_nacimiento,
                              enddate = fecha_derecho,
                              units = "years",
                              precise = FALSE ) ) ) %>%
  group_by( asegurado, tipo_seguro, tipo_prestacion, t, x, sexo ) %>%
  mutate( oldage = n() ) %>%
  ungroup( ) %>%
  distinct( ., t, x, sexo, tipo_seguro, tipo_prestacion, .keep_all = TRUE ) %>%
  dplyr::select( t, x, sexo, tipo_seguro, tipo_prestacion, oldage )
  
  
#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en un solo data.frame' )

save( oldage_sgo,
      file = paste0( parametros$RData, 'IESS_jubilados_hist.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
