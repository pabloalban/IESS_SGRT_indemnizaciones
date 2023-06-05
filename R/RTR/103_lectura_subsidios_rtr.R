message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de pagos de subsidios de RTR' )


#Archivo pagos de subsidios-------------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_RTR_subsidios_prestaciones.xlsx' )


#Cargando pagos de subsidios -----------------------------------------------------------------------

message( '\tLeyendo subsidios de SGRT' )

file_subsidios <- paste0( parametros$Data_seg, 'IESS-DSGRT-2022-1882-M/subsidios.txt' )

subsidios_rtr <- (read.table(file_subsidios,
                              skip=0,
                              dec = ".",
                              header = TRUE,
                              sep = "\t",
                              na.strings = "NA",
                              encoding="UTF-8",
                              row.names = NULL )) %>% clean_names() %>%
  mutate( cedula = cedula_afi,
          sexo = genero,
          fecha_nacimiento = substr(fecnac, 1, 10 ),
          fecha_transferencia = substr(fecha_transferencia, 1, 10 ) ) %>%
  mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%Y/%m/%d" ),
          fecha_transferencia  = as.Date( fecha_transferencia, "%Y/%m/%d" ) ) %>%
  mutate( sexo = if_else(sexo == "Femenino                        ", "M", "H") ) %>%
  dplyr::select( -nombres_afi,
                 -fecnac,
                 -cedula_afi,
                 -estado_civil,
                 -sr_unidadmedica,
                 -sr_rucunimed,
                 -edad,
                 -genero,
                 -desreltra,
                 -sr_prov_siniestro,
                 -sr_rama_actividad,
                 -desc_rama_act,
                 -numerocuentaban,
                 -unidad_medica_prestadora,
                 -provincia_unidad_medica,
                 -nota_debito,
                 -nota_credito,
                 -fecha_periodo_pago,
                 -fechainicio,
                 -fechafin,
                 -fecha_contingencia ) %>%
  mutate( anio = year( fecha_transferencia ),
          mes = month( fecha_transferencia ) ) %>%
  filter( anio <= 2021, anio >= 2012)

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando data.frame de subsidios' )

save( subsidios_rtr,
      file = paste0( parametros$RData_seg, 'IESS_RTR_subsidios.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()