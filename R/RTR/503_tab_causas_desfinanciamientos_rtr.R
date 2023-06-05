message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tablas de las casusas de desfinanciamiento' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_causas_desfinanciamiento.RData' ) )

# Tabla causas totales de desfinanciamiento---------------------------------------------------------
message( '\tTabla total desfinanciamiento' )
aux <- copy( causa_desf_total )
aux <- as.data.table( aux )
aux <- aux[ , print_names := c( 'Ausencia contribuci\\\'{o}n del 40\\% Estado',  
                                'Diferencia aportes C.D. 261 y C.D. 501', 
                                'Desinversiones','Total') ]
aux <- aux[ ,list(print_names,capital,lucro_cesante ,total_afectacion)]
#aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_RTR_total', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))

# Tabla aportes del estado--------------------------------------------------------------------------
message( '\tTabla deuda del estado por 40% consolidada' )
aux <- as.data.table( causa_desf_deuda_estado_consolidada )
aux[, porcentaje_cumplimiento := porcentaje_cumplimiento * 100 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_RTR_cumplimiento_deuda_estado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))



message( '\tTabla aportes del estado' )
aux <- as.data.table( causa_desf_estado %>% select( -coeficiente ) )
aux[, tasa_rendimiento := tasa_rendimiento * 100 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 2, 4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_RTR_desfi_aportes_estado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))



# Tabla CD 501--------------------------------------------------------------------------------------
message( '\tTabla aportes del estado' )
aux <- as.data.table( causa_desf_CD501 %>% select( -coeficiente ) )
aux[, tasa_rendimiento := tasa_rendimiento * 100 ]
aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_RTR_desfi_cd501', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))


# Tabla desinversiones------------------------------------------------------------------------------
message( '\tTabla desinversiones' )
aux <- ( causa_desf_desinversiones %>% filter( capital_desinvertido > 0) %>% select( -factor ) )
aux$periodo<- as.Date(aux$periodo,"%d/%m/%Y")
aux$periodo <- format(aux$periodo, "%b/%Y")
aux$rentabilidad_neta<-aux$rentabilidad_neta*100
aux$rentabilidad_mensual<-aux$rentabilidad_mensual*100
aux[nrow(aux),1] <- 'Total'
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 3, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_RTR_desinversiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))

# Tabla desinversiones------------------------------------------------------------------------------
message( '\tTabla desinversiones' )
aux <- ( causa_desf_desinversiones_anual )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2 ,2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_RTR_desinversiones_anual', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))


####################################################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
