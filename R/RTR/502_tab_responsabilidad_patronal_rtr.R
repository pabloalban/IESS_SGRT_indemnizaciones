message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas de responsabilidad patronal de SGRT' )

# Carga de datos------------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_mora_patronal.RData' ) ) 

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )


#Tabla rp por estado de acuerdos y sector-----------------------------------------------------------

aux <- tab_acuerdo_sector %>%
  mutate( estado_acuerdo = if_else( estado_acuerdo == 'CANCELADO MEDIANTE TITULO',
                                    'CANCELADO MEDIANTE TÍTULO',
                                    estado_acuerdo ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 0, 3 ),  rep( 2, 3 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_acuerdo_sector', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla estado del acuerdo por estado glosa a los acuerdos transferidos a glosa----------------------

aux <- tab_transferido_glosa_sector %>%
  mutate( estado_glosa = if_else( estado_glosa == 'EN ESPERA DE NOTIFICACION',
                                    'EN ESPERA DE NOTIFICACIÓN',
                                  estado_glosa ) ) %>%
  mutate( estado_glosa = if_else( estado_glosa == 'EN ESPERA DE PUBLICACION',
                                  'EN ESPERA DE PUBLICACIÓN',
                                  estado_glosa ) ) %>%
  mutate( estado_glosa = if_else( estado_glosa == 'TRANSFERIDA A TITULO DE CREDITO',
                                  'TRANSFERIDA A TÍTULO DE CRÉDITO',
                                  estado_glosa ) ) %>%
  mutate( estado_glosa = if_else( estado_glosa == 'IMPUGNACION 2da INSTANCIA',
                                  'IMPUGNACIÓN 2da INSTANCIA',
                                  estado_glosa ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 0, 3 ),  rep( 2, 3 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_transferido_glosa_sector', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla acuerdos por concepto y sector sin glosas canceladas-----------------------------------------
aux <- tab_acuerdo_concepto 

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 0, 3 ),  rep( 2, 3 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_acuerdo_concepto', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla acuerdos por concepto, porcentaje y monto promedio sin glosas canceladas---------------------

aux <- tab_concepto_prom %>%
  mutate( rp = as.integer( rp ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 0, 1 ),  rep( 2, 3 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_concepto_prom', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


#Tabla RP por provincia y monto---------------------------------------------------------------------
aux <- tabla_rp_provincia 

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 0, 3 ),  rep( 2, 3 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tabla_rp_provincia', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla rp por montos por rango por sexo-------------------------------------------------------------

aux <- tabla_rangos_montos_rp %>%
  mutate( sexoben12M = as.integer( sexoben12M ),
          sexoben12H = as.integer( sexoben12H ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tabla_rangos_montos_rp', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )
#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()