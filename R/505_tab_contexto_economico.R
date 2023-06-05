message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura del contexto económico' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_contexto_economico.RData' ) )
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla del contexto económico-----------------------------------------------------------------------
message( '\tTablas del contexto económico' )

#Tabla de la inflación------------------------------------------------------------------------------

aux <- inflacion %>%
  filter( mes == '12',
          anio <= '2022') %>%
  dplyr::select( periodo,
                 ipc,
                 inflacion_mensual,
                 inflacion_acumulada,
                 inflacion_promedio_acumulada ) %>%
  mutate( periodo = as.character( periodo ) )


aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2 ,2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inflacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de desempleo---------------------------------------------------------------------------------

aux <- desempleo %>%
  filter( mes == '12' ) %>%
  dplyr::select( periodo,
                 desempleo_hombre,
                 desempleo_mujer,
                 desempleo_nacional,
                 empleo_adecuado_pleno_n,
                 subempleo_nacional ) %>%
  mutate( periodo = as.character( format( periodo, "%b %Y" ) ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2 ,2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_desempleo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de PIB---------------------------------------------------------------------------------------

aux <- pib_real %>%
  filter( anio >= '2000',
          anio <= '2022' ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 0 , 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pib_real', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de SBU---------------------------------------------------------------------------------------

aux <- sbu %>%
  filter( anio <= '2022') %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2 , 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sbu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de salarios promedio-------------------------------------------------------------------------

aux <- salarios %>%
  mutate( anio = as.character( anio ) ) %>%
  filter( mes == '12' ) %>%
  arrange( periodo ) %>%
  mutate( incremento = sal_prom - lag( sal_prom ) ) %>%
  mutate( tasa = 100 * (sal_prom - lag( sal_prom )) / lag( sal_prom ) ) %>%
  na.omit( . ) %>%
  dplyr::select( periodo,
                 sal_prom,
                 incremento,
                 tasa ) %>%
  mutate( periodo = as.character( format( periodo, "%b %Y" ) ) )
  

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2 , 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_salarios', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de aumento de pensiones----------------------------------------------------------------------

aux <- incre_pensiones

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 8 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_incre_pensiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de pensiones mínimas-------------------------------------------------------------------------

aux <- pension_min %>%
  filter( anio <= '2022' ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, rep( 2, 6 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pension_min', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de pensiones máximas-------------------------------------------------------------------------

aux <- pension_max %>%
  filter( anio <= '2022' ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, rep( 2, 7 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pension_max', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de tasas de interés---------------------------------------------------------------------------

aux <- tasas_interes %>%
  filter( anio <= '2022', mes == '12' ) %>%
  mutate( periodo = as.character(  format( periodo, "%b %Y") ) ) %>%
    dplyr::select( periodo,
                   tasa_activa,
                   tasa_pasiva,
                   spread )


aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 3 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tasas_interes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de ROA---------------------------------------------------------------------------------------

aux <- roa %>%
  filter( anio <= '2022' ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( across( where(is.numeric), ~ .x * 100 ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 5 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_roa', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de ROE---------------------------------------------------------------------------------------

aux <- roe %>%
  filter( anio <= '2022' ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( across( where(is.numeric), ~ .x * 100 ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 5 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_roe', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla rendimientos del BIESS-----------------------------------------------------------------------

aux <- rendimiento_biess %>%
  filter( fecha <= as.Date("31/12/2022", "%d/%m/%Y" ),
          mes == '12' ) %>%
  mutate( fecha = as.character( format( fecha, "%b %Y") ) ) %>%
  dplyr::select( -mes ) %>%
  mutate( rendimiento = 100 * rendimiento ) %>%
  na.omit( )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 7 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rendimiento_biess', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de resumen de hipótesis----------------------------------------------------------------------

aux <- Hipotesis

aux_xtab <- xtable( aux, digits = c(0, 0, 3 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_hip_macro', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de predicciones por año----------------------------------------------------------------------
message( '\tTablas del modelo predictivo de las hipótesis macro-económicas' )
aux <- tasas_macro_pred %>%
  filter( anio >= 2023 ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( tasa_activa = 100 * tasa_activa,
          tasa_pasiva = 100 * tasa_pasiva,
          inflación_prom = 100 * inflación_prom)

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 6 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tasas_macro_pred', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de coeficientes del modelo-------------------------------------------------------------------

aux <- coeficientes

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 5, 6 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_coeficientes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de prueba de normalidad----------------------------------------------------------------------

aux <- shapiro_test

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 5, 2 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

aux_xtab <- xtable( aux_xtab, digits = c(0, 0, 5, 5 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_shapiro_test', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de prueba de homocedasticidad----------------------------------------------------------------

aux <- homocedasticidad

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 5, 2 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_homocedasticidad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )



#Tabla matriz de correlaciones----------------------------------------------------------------------

aux <- ma_correlaciones

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 5, 6 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

aux_xtab <- xtable( aux_xtab, digits = c(0, 0, rep( 5, 6 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ma_correlaciones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla matriz de p-valores de la prueba de multicolinealidad----------------------------------------

aux <- ma_multi_p_valores

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 5, 6 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

aux_xtab <- xtable( aux_xtab, digits = c(0, 0, rep( 5, 6 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ma_multi_p_valores', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla matriz de covarianza-------------------------------------------------------------------------

aux <- covarianza

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 6 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ma_covarianza', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de la prueba de nulidad de coeficientes------------------------------------------------------

aux <- test_nulidad_nulidad

aux_xtab <- xtable( aux, digits = c(0, 0,2 ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_test_nulidad_nulidad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla del test de Box Ljung------------------------------------------------------------------------

aux <- box_ljung 

aux_xtab <- xtable( aux, digits = c(0, 0, 5, 0, 5 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_test_box_ljung', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de prueba de normalidad----------------------------------------------------------------------

aux <- shapiro_test

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 5, 2 ) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_shapiro_test', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Borrando data frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )
