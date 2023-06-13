message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCarga de indemnizaciones' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData,'IESS_tab_indemnizaciones.RData'  ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tTablas de afiliados afectados al tope por 60, 50 y 40 sbu' )
#Tabla de afiliados afectados al tope de 60 sbu-----------------------------------------------------

aux <- tab_ind_max %>% clean_names( )

aux_xtab <- xtable( aux, digits = c( 0, rep(0, 4), rep( 2, 4 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_ind_max', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla de afiliados afectados al tope de 50 sbu-----------------------------------------------------

aux <- tab_ind_max_50 %>% clean_names( )

aux_xtab <- xtable( aux, digits = c( 0, rep(0, 4), rep( 2, 4 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_ind_max_50', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


#Tabla de afiliados afectados al tope de 40 sbu-----------------------------------------------------

aux <- tab_ind_max_40 %>% clean_names( )

aux_xtab <- xtable( aux, digits = c( 0, rep(0, 4), rep( 2, 4 ) ) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_ind_max_40', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#-----------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

