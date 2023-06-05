message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando la tabla de resumen de resultados de la valuación' )

#parametrización de los escenarios -----------------------------------------------------------------
nom_esc <- c( 'Escenario 1' )
escenario <- paste0( 'escenario_', 1:length( nom_esc ) )

#Creación data.frame -------------------------------------------------------------------------------
result_list <- vector(mode = "list", length = length( escenario ) )

result <- data.table( variable = c( 'V0', 'A2_vap', 'A_pen_vap', 'A_est_vap', 'Act_vap', 'B_pen_vap', 
                                    'B_sal_vap', 'B_vap', 'G_vap', 'Pas_vap', 'V', 'pri_med_niv_apo' ),
                      desc = c( 
                        # 'Tasa actuarial (\\%)',
                        # 'Tasa de crecimiento del SBU (\\%)',
                        # 'Tasa de crecimiento del salario promedio (\\%)',
                        # 'Tasa de crecimiento de las pensiones (\\%)',
                        'Reserva inicial (USD)', 
                        'Aportes de cotizantes (USD)', 
                        'Aportes de pensionistas (USD)', 
                        'Aportes del Estado (USD)', 
                        'Activo actuarial', 
                        'Beneficios por pensiones',
                        'Prestaciones m\\\'{e}dico asistenciales ',
                        'Beneficios totales (USD)', 
                        'Gastos administrativos (USD)',
                        'Pasivo actuarial (USD)',
                        'Balance actuarial (USD)',
                        'Prima media nivelada (\\%)' ),
                      orden = 1:12
                      )

for( i in 1:length( escenario ) ){
  load( paste0( parametros$RData_seg, 'IESS_RTR_primas_', escenario[i], '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', escenario[i], '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_RTR_balances_',escenario[i],'.RData' ) )
  
  # i_a = formatC( esc$i_a*100, decimal.mark = ",", format = 'f', digits = 2),
  # i_sbu = formatC( esc$i_sbu*100, decimal.mark = ",", format = 'f', digits = 5),
  # i_sal = formatC( esc$i_r*100, decimal.mark = ",", format = 'f', digits = 5),
  # i_p = formatC( esc$i_p*100, decimal.mark = ",", format = 'f', digits = 5),
  
  pri_med_niv_apo <- 100 * prima[ t ==  parametros$horizonte ]$pri_med_niv_apo_est_pen
  
  aux1 <- balance_anual[ t == max( t ), 
                         list( V0, A2_vap, A_pen_vap, A_est_vap, Act_vap,
                               BP_vap = B9_vap + B10_vap + B11_vap + B13_vap + B14_vap + B15_vap, 
                               B_sal_vap, B_vap,
                               G_vap,
                               Pas_vap, 
                               V, 
                               pri_med_niv_apo ) ]
  
  aux1 <- melt.data.table( aux1, measure.vars = 1:ncol( aux1 ), 
                           variable.name = 'variable', value.name = paste0( 'val_esc_', i ) )
  
  result <- merge( result, aux1, by = 'variable' )
  
}

result <- result[,-1, with = FALSE ]
setorder(result, orden)
result[, orden := NULL]

#Guardar en latex-----------------------------------------------------------------------------------
xtb_result <- xtable( result, digits = c( 0, 0, rep( 2, 1 ) ) )
print( xtb_result,
       file = paste0( parametros$resultado_tablas, 'iess_resultados.tex' ),
       type = 'latex', 
       include.colnames = FALSE, 
       include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE,
       sanitize.text.function = identity,
       hline.after = c( 5, 9, 10 ) )

#Borrando los dataframes----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
