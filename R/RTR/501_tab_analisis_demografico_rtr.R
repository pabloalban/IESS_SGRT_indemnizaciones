message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis demográfico' )

# Carga de datos------------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_demografia.RData' ) ) 

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )


#1.Subsidios----------------------------------------------------------------------------------------
message( '\tTablas Subsidios' )
## 1.1 Tabla evolución de beneficiarios en subsidios------------------------------------------------

aux <- tab_evo_ben_subsidios %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_subsidios_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

##1.2 Tabla evolución de montos entregados----------------------------------------------------------

aux <- tab_evo_monto_subsidios %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 8 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_subsidios_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


##1.3 Tabla Subsidios entregado por rango de montos-------------------------------------------------

aux <- tab_rango_monto_subsidios

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2 ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_subsidios_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )


#2. Indemnizaciones---------------------------------------------------------------------------------
message("\tTablas sobre indemnizaciones del SGRT")
##2.1 Tabla evolución de beneficiarios en subsidios-------------------------------------------------

aux <- tab_evo_ben_indemnizaciones %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_indemnizaciones_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )
##2.2 Tabla evolución de montos entregados----------------------------------------------------------

aux <- tab_evo_monto_indemnizaciones %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 8 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_indemnizaciones_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

##2.3 Tabla indemnizaciones entregado por rango de montos-------------------------------------------

aux <- tab_rango_monto_indemnizaciones

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2 ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_indemnizaciones_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1,
                        nrow(aux) ),
       sanitize.text.function = identity )

#3. Tabla evolución de pensionistas-----------------------------------------------------------------
message("\tTablas evolución de pensionistas del SGRT")

## 3.1 Tabla evolución de pensionistas permanente parcial-------------------------------------------
aux <- tab_evo_ben_pp %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_pp_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

## 3.2 Tabla evolución de pensionistas incapacidad permanente total---------------------------------
aux <- tab_evo_ben_pt %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_pt_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )


## 3.3 Tabla evolución de pensionistas permanente absoluta------------------------------------------
aux <- tab_evo_ben_pa %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_pa_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

## 3.4 Tabla evolución de pensionistas de viudedad--------------------------------------------------
aux <- tab_evo_ben_vo %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_vo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

## 3.5 Tabla evolución de pensionistas de orfandad--------------------------------------------------
aux <- tab_evo_ben_of %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(0, 7), 2  ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_ben_of_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )


#4. Tabla evolución de montos entregados------------------------------------------------------------
message("\tTablas evolución de montos entregados por el SGRT")

## 4.1 Tabla evolución de montos permanente parcial-------------------------------------------
aux <- tab_evo_monto_pp %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(2, 9) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_pp_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

## 4.2 Tabla evolución de montos incapacidad permanente total---------------------------------------
aux <- tab_evo_monto_pt %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(2, 9) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_pt_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )


## 4.3 Tabla evolución de montos permanente absoluta------------------------------------------------
aux <- tab_evo_monto_pa %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(2, 9) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_pa_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

## 4.4 Tabla evolución de montos de viudedad--------------------------------------------------------
aux <- tab_evo_monto_vo %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(2, 9) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_vo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )

## 4.5 Tabla evolución de pensionistas de orfandad--------------------------------------------------
aux <- tab_evo_monto_of %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep(2, 9) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_evo_monto_of_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) ),
       sanitize.text.function = identity )


#5. Tablas de los beneficiarios por rangos de pensiones---------------------------------------------

message("\tTablas de los beneficiarios por rangos de pensiones del SGRT")

## 5.1 Tablas de los beneficiarios por rangos de permanente parcial---------------------------------
aux <- tab_rango_monto_pp

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_pp_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) - 1,
                        nrow(aux) ),
       sanitize.text.function = identity )

## 5.2 Tablas de los beneficiarios por rangos de incapacidad permanente total-----------------------
aux <- tab_rango_monto_pt

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_pt_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) - 1,
                        nrow(aux) ),
       sanitize.text.function = identity )


## 5.3 Tablas de los beneficiarios por rangos de permanente absoluta--------------------------------
aux <- tab_rango_monto_pa

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_pa_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) - 1,
                        nrow(aux) ),
       sanitize.text.function = identity )

## 5.4 Tablas de los beneficiarios por rangos de viudedad-------------------------------------------
aux <- tab_rango_monto_vo 

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_vo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) - 1,
                        nrow(aux) ),
       sanitize.text.function = identity )

## 5.5 Tablas de los beneficiarios por rangos de orfandad--------------------------------------------------
aux <- tab_rango_monto_of

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_rango_monto_of_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) - 1,
                        nrow(aux) ),
       sanitize.text.function = identity )

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()