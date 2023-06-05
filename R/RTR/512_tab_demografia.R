message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tabla información finaciera ' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg,'IESS_RTR_tablas_rp.RData'  ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#lectura tabla de edad------------------------------------------------------------------------------
message( '\tTabla edades por sexo' )
aux <- as.data.table(edad_sexo_total)  #elaborado por Darlyn
aux <- clean_names(aux)
aux_xtab <- xtable( aux, digits = c(0,0,0,0,0))
print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_edad_sexo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#lectura de valores pendientes por sexo---------------------------------------------------------------------

message( '\tTabla Valor pendiente por sexo' )
aux <- as.data.table(monto_rangos_sexo)
#aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, 0) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_valores_pendientes_sexo_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#lectura de valores pendientes por tipo de empresa y sexo---------------------------------------------------------------------

message( '\tTabla Valor pendiente por empresa y sexo' )
aux <- as.data.table(monto_rangos_sexo_empresa)
#aux[,Año:=as.character(Año)]
aux<-clean_names(aux)
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, 0,0,0,0) )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_valores_pendientes_sexo_empresa_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#-----------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

