message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de primas por escenario' )

escenario <- paste0( 'escenario_', 1:1 )
nom_esc <- c( 'Base')

pri <- NULL
for( i in 1:length( escenario ) ){
  load( paste0( parametros$RData_seg, 'IESS_RTR_primas_', escenario[i], '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', escenario[i], '.RData' ) )
  
  prima_med_niv <- prima[ t == parametros$horizonte]$pri_med_niv_apo_est_pen
  
  pri <- rbind( pri, 
                esc$hip_esc[ t == parametros$horizonte, 
                             list( i_a = 100 * i_a, 
                                   apo_cot = 100 * apo_cot, 
                                   apo_jub = 100 * apo_jub, 
                                   apo_sal = 100 * apo_sal, 
                                   apo_est = 100 * apo_est, 
                                   prima_med_niv = 100 * prima_med_niv ) ]
  )
}
pri <- cbind( nom_esc, pri )

xtb_pri <- xtable( pri, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_tab_primas.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL,
       sanitize.text.function = identity )

# Limpiando memoria RAM-----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
