message(paste(rep("-", 100), collapse = ""))
#Cargando Rdatas------------------------------------------------------------------------------------
message("\tCargando mora patronal del SGRT")
load(paste0(parametros$RData_seg, "IESS_RTR_responsabilidad_patronal.RData"))
load(paste0(parametros$RData_seg, "IESS_empleadores.RData"))

#Tabla rp por estado de acuerdos y sector-----------------------------------------------------------
message("\tTablas de responsabilidad patronal del SGRT")
tab_acuerdo_sector <- reporte_resp_patronal %>% 
  filter( !(estado_acuerdo %in% c( 'ANULADO', 'APROBADO' ) ) ) %>%
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  mutate( tipo_de_empresa = if_else( tipo_de_empresa %in% c('SEGURO SOCIAL CAMPESINO',
                                                            'VOLUNTARIO'),
                                  'PRIVADA',
                                  tipo_de_empresa ) ) %>%
  group_by( estado_acuerdo, tipo_de_empresa ) %>%
  mutate( rp = n() ) %>%
  ungroup( ) %>%
  distinct( estado_acuerdo, tipo_de_empresa, .keep_all = TRUE ) %>%
  dplyr::select( estado_acuerdo, tipo_de_empresa, rp ) %>%
  spread(  ., tipo_de_empresa, value = c( rp ),  sep = "rp" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_rp =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) 

tab_acuerdo_sector <- reporte_resp_patronal %>% 
  filter( !(estado_acuerdo %in% c( 'ANULADO', 'APROBADO' ) ) ) %>%
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  mutate( tipo_de_empresa = if_else( tipo_de_empresa %in% c('SEGURO SOCIAL CAMPESINO',
                                                            'VOLUNTARIO'),
                                     'PRIVADA',
                                     tipo_de_empresa ) ) %>%
  group_by( estado_acuerdo, tipo_de_empresa ) %>%
  mutate( rp = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( estado_acuerdo, tipo_de_empresa, .keep_all = TRUE ) %>%
  dplyr::select( estado_acuerdo, tipo_de_empresa, rp ) %>%
  spread(  ., tipo_de_empresa, value = c( rp ),  sep = "monto" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_monto =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  left_join( tab_acuerdo_sector, . , by = 'estado_acuerdo') %>%
  mutate_at( c(2:4), as.integer ) %>%
  mutate_at( c(5:ncol(.)), as.numeric)


#Tabla estado del acuerdo por estado glosa a los acuerdos transferidos a glosa----------------------

tab_transferido_glosa_sector <- reporte_resp_patronal %>% 
  #filter( !(estado_glosa %in% c( 'CANCELADA', 'CANCELADA CON CONVENIO' ) ) ) %>%
  filter( estado_acuerdo == 'TRANSFERIDO A GLOSA' ) %>%
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  mutate( tipo_de_empresa = if_else( tipo_de_empresa %in% c('SEGURO SOCIAL CAMPESINO',
                                                            'VOLUNTARIO'),
                                     'PRIVADA',
                                     tipo_de_empresa ) ) %>%
  group_by( estado_glosa, tipo_de_empresa ) %>%
  mutate( rp = n() ) %>%
  ungroup( ) %>%
  distinct( estado_glosa, tipo_de_empresa, .keep_all = TRUE ) %>%
  dplyr::select( estado_glosa, tipo_de_empresa, rp ) %>%
  spread(  ., tipo_de_empresa, value = c( rp ),  sep = "rp" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_rp =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) 

tab_transferido_glosa_sector <- reporte_resp_patronal %>% 
  #filter( !(estado_glosa %in% c( 'CANCELADA', 'CANCELADA CON CONVENIO' ) ) ) %>%
  filter( estado_acuerdo == 'TRANSFERIDO A GLOSA' ) %>%
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  mutate( tipo_de_empresa = if_else( tipo_de_empresa %in% c('SEGURO SOCIAL CAMPESINO',
                                                            'VOLUNTARIO'),
                                     'PRIVADA',
                                     tipo_de_empresa ) ) %>%
  group_by( estado_glosa, tipo_de_empresa ) %>%
  mutate( rp = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( estado_glosa, tipo_de_empresa, .keep_all = TRUE ) %>%
  dplyr::select( estado_glosa, tipo_de_empresa, rp ) %>%
  spread(  ., tipo_de_empresa, value = c( rp ),  sep = "monto" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_monto =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  left_join( tab_transferido_glosa_sector, . , by = 'estado_glosa') %>%
  mutate_at( c(2:4), as.integer ) %>%
  mutate_at( c(5:ncol(.)), as.numeric)

#Tabla acuerdos por concepto y sector sin glosas canceladas-----------------------------------------
aux <- reporte_resp_patronal %>% 
  filter( !(estado_glosa %in% c( 'CANCELADA', 'CANCELADA CON CONVENIO' ) ) ) %>%
  filter( estado_acuerdo == 'TRANSFERIDO A GLOSA' ) %>%
  mutate( concepto = if_else( concepto %in% c('DETALLE',
                                              'null null null',
                                              'POR ACUERDO JUBILACION ACCID',
                                              'RIESGOS DEL TRABAJO'),
                              'NO ESPECIFICA',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO  SUBSIDIOS',
                                               'RIESGOS DEL TRABAJO SUBSIDIOS',
                                               'RIESGOS DEL TRABAJO  TEMPORAL 1-2 AÑOS RENTA',
                                               'RIESGOS DEL TRABAJO TEMPORAL 1-2 AÑOS RENTA' ),
                              'SUBSIDIOS',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO  INDEMNIZACIÓN',
                                               'RIESGOS DEL TRABAJO INDEMNIZACIÓN' ),
                              'INDEMNIZACIÓN',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO  MONTEPIO',
                                               'RIESGOS DEL TRABAJO MONTEPIO' ),
                              'MONTEPÍO',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO  PERMANENTE',
                                               'RIESGOS DEL TRABAJO PERMANENTE' ),
                              'INCAPACIDAD PERMANENTE PARCIAL',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO  PERMANENTE  PARCIAL',
                                               'RIESGOS DEL TRABAJO PERMANENTE PARCIAL' ),
                              'INCAPACIDAD PERMANENTE PARCIAL',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO PERMANENTE TOTAL',
                                               'RIESGOS DEL TRABAJO  PERMANENTE  TOTAL' ),
                              'INCAPACIDAD PERMANENTE TOTAL',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO PERMANENTE ABSOLUTA' ),
                              'INCAPACIDAD PERMANENTE ABSOLUTA',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO ATENCIONES MEDICAS' ),
                              'ATENCIONES MEDICAS',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO SANCIONES Y MULTAS',
                                               'RIESGOS DEL TRABAJO SANCIONES Y MULTAS POR MULTA SIN OTRA CAUSA DE RP',
                                               'RIESGOS DEL TRABAJO SANCIONES Y MULTAS POR PRESTACIONES',
                                               'RIESGOS DEL TRABAJO SANCIONES Y MULTAS POR SEGUIMIENTOS' ),
                              'SANCIONES Y MULTAS',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO SANCIONES Y MULTAS POR INOBSERVANCIA DE MEDIDAS DE PREVENCIÓN' ),
                              'POR INOBSERVANCIA DE MEDIDAS DE PREVENCIÓN',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO SANCIONES Y MULTAS POR OBSTACULIZAR LOS PROCESOS' ),
                              'POR OBSTACULIZAR LOS PROCESOS',
                              concepto ) ) %>%
  mutate( concepto = if_else( concepto %in% c( 'RIESGOS DEL TRABAJO SANCIONES Y MULTAS POR NO ACATAR LOS DICTÁMENES DE CAMBIO DE PUESTO DE TRABAJO' ),
                              'POR NO ACATAR LOS DICTÁMENES DE CAMBIO DE PUESTO DE TRABAJO',
                              concepto ) ) %>%
  
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  mutate( tipo_de_empresa = if_else( tipo_de_empresa %in% c('SEGURO SOCIAL CAMPESINO',
                                                            'VOLUNTARIO'),
                                     'PRIVADA',
                                     tipo_de_empresa ) )



tab_acuerdo_concepto <- aux %>%
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  group_by( concepto, tipo_de_empresa ) %>%
  mutate( rp = n() ) %>%
  ungroup( ) %>%
  distinct( concepto, tipo_de_empresa, .keep_all = TRUE ) %>%
  dplyr::select( concepto, tipo_de_empresa, rp ) %>%
  spread(  ., tipo_de_empresa, value = c( rp ),  sep = "rp" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_rp =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) 

tab_acuerdo_concepto <- aux %>%
  mutate( tipo_de_empresa = as.character( tipo_de_empresa ) ) %>%
  group_by( concepto, tipo_de_empresa ) %>%
  mutate( rp = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( concepto, tipo_de_empresa, .keep_all = TRUE ) %>%
  dplyr::select( concepto, tipo_de_empresa, rp ) %>%
  spread(  ., tipo_de_empresa, value = c( rp ),  sep = "monto" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_monto =  rowSums(.[2:ncol(.)]) ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  left_join( tab_acuerdo_concepto, . , by = 'concepto') %>%
  mutate_at( c(2:4), as.integer ) %>%
  mutate_at( c(5:ncol(.)), as.numeric )

#Tabla acuerdos por concepto, porcentaje y monto promedio sin glosas canceladas---------------------

tab_concepto_prom <- aux %>%
  group_by( concepto ) %>%
  mutate( rp = n() ) %>%
  mutate( monto = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( concepto, .keep_all = TRUE ) %>%
  mutate( rp_por = 100* rp / sum( rp ) ) %>%
  mutate( monto_prom = monto / rp ) %>%
  dplyr::select( concepto, rp, rp_por, monto_prom, monto ) %>%
  replace( is.na(.), 0 ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:3], na.rm = TRUE )), 
              as.character( mean( aux$valor_pendiente, na.rm = TRUE ) ),
              as.character(colSums(.[,ncol(.)], na.rm = TRUE ))))  %>%
  mutate_at( c(2:ncol(.)), as.numeric )

#Pirámide por edad y sexo de los afiliados----------------------------------------------------------

pir_rp_edad_sexo <- reporte_resp_patronal %>% 
  filter( !(estado_acuerdo %in% c( 'ANULADO', 'APROBADO' ) ) ) %>% 
  group_by( sexo, edad ) %>%
  mutate( freq = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, edad, .keep_all = TRUE ) %>%
  mutate( fdp = freq / sum( freq ) ) %>% 
  dplyr::select( sexo, edad, freq, fdp ) %>%
  arrange( sexo, edad )
  
#Evolución histórica de la emisión de rp y montos glosados------------------------------------------

evo_rp_montos_anio <- reporte_resp_patronal %>% 
  filter( !(estado_acuerdo %in% c( 'ANULADO', 'APROBADO' ) ) ) %>% 
  mutate( anio = year( fecha_acuerdo ) ) %>%
  group_by( anio ) %>%
  mutate( rp = n() ) %>%
  mutate( monto = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, rp, monto) %>%
  arrange( anio ) %>%
  filter( anio < 2022)

#Evolución histórica de la emisión de rp y montos sin cancelar--------------------------------------

evo_rp_montos_anio_pendientes <- reporte_resp_patronal %>% 
  filter( !(estado_glosa %in% c( 'CANCELADA', 'CANCELADA CON CONVENIO' ) ) ) %>%
  filter( estado_acuerdo == 'TRANSFERIDO A GLOSA' ) %>%
  mutate( anio = year( fecha_acuerdo ) ) %>%
  group_by( anio ) %>%
  mutate( rp = n() ) %>%
  mutate( monto = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, rp, monto) %>%
  arrange( anio ) %>%
  filter( anio < 2022)

#Tabla RP por provincia y monto---------------------------------------------------------------------

a <- reporte_resp_patronal %>% 
  mutate( provincia = as.character( provincia ) ) %>%
  filter( !(estado_glosa %in% c( 'CANCELADA', 'CANCELADA CON CONVENIO' ) ) ) %>%
  filter( estado_acuerdo == 'TRANSFERIDO A GLOSA' ) %>%
  group_by( sexo, provincia ) %>%
  mutate( rp = n() ) %>%
  mutate( monto = sum( valor_pendiente, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., sexo, provincia, .keep_all = TRUE ) %>%
  dplyr::select( sexo, provincia , rp, monto)

tabla_rp_provincia <- a %>%
  dplyr::select( -monto ) %>%
  spread(  ., sexo, value = c( rp ),  sep = "" ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_rp =  rowSums(.[2:ncol(.)]) )

tabla_rp_provincia <- a %>%
  dplyr::select( -rp ) %>%
  spread(  ., sexo, value = c( monto ),  sep = "" ) %>%
  replace(is.na(.), 0) %>% 
  mutate( total_monto =  rowSums(.[2:ncol(.)]) ) %>%
  left_join( tabla_rp_provincia, ., by = 'provincia' ) %>%
  rbind( ., c("TOTAL", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  mutate_at( c(2:4), as.integer ) %>%
  mutate_at( c(5:ncol(.)), as.numeric )

#Tabla rp por montos por rango por sexo-------------------------------------------------------------

a <- reporte_resp_patronal %>% 
  mutate( provincia = as.character( provincia ) ) %>%
  filter( !(estado_glosa %in% c( 'CANCELADA', 'CANCELADA CON CONVENIO' ) ) ) %>%
  filter( estado_acuerdo == 'TRANSFERIDO A GLOSA' )

cortes_monto <- c( quantile(a$valor_pendiente, probs = seq( 0, 1, length.out = 11 ) ) )

etiquetas_monto<-c(paste0("(\\$",formatC( cortes_monto[1:length(cortes_monto)-1], 
                                        digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          " - \\$",formatC( cortes_monto[2:length(cortes_monto)], 
                                        digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

a <- a %>%
  mutate(rango_monto=cut(valor_pendiente, 
                         breaks = cortes_monto,
                         labels = etiquetas_monto,
                         include.lowest = TRUE,
                         right = TRUE)) %>%
  group_by( rango_monto, sexo ) %>%
  mutate( ben = n() ) %>%
  ungroup( ) %>%
  distinct( sexo, rango_monto, .keep_all = TRUE ) %>%
  dplyr::select( sexo,
                 rango_monto,
                 ben ) %>%
  arrange( sexo, rango_monto ) %>%
  spread(  ., sexo, value = c( ben ),  sep = "ben12" )  %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  mutate( rango_monto = as.character( rango_monto ) ) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))  %>%
  mutate_at( c(2:ncol(.)), as.numeric) %>%
  mutate( por_sexoben12M = 100 * 2 * sexoben12M / sum( total ),
          por_sexoben12H = 100* 2 * sexoben12H / sum( total ),
          por_total = 100 * 2 * total / sum( total ) ) %>%
  dplyr::select( rango_monto,
                 sexoben12M,
                 por_sexoben12M,
                 sexoben12H,
                 por_sexoben12H,
                 total,
                 por_total ) %>%
  distinct( ., rango_monto, .keep_all = TRUE )

tabla_rangos_montos_rp <- a

#Guardar en Rdatas----------------------------------------------------------------------------------
message("\tGuardando Rdatas")
save( tab_acuerdo_sector,
      tab_transferido_glosa_sector,
      tab_acuerdo_concepto,
      tab_concepto_prom,
      pir_rp_edad_sexo,
      evo_rp_montos_anio,
      tabla_rp_provincia,
      tabla_rangos_montos_rp,
      evo_rp_montos_anio_pendientes,
      file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_mora_patronal.RData' ) )
#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
