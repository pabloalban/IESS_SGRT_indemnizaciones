message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <- paste0( parametros$RData_seg, 'IESS_RTR_inversiones.RData' )
load( file = file_inversiones )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla de la evolución del portafolio -------------------------------------------------------------
aux <- recurs_adm_biess %>%
  filter( ano >= 2012 ) %>%
  mutate( ano = as.character( ano ),
          rendimiento_neto = rendimiento_neto*100,
          rendimiento_ponderado = rendimiento_ponderado*100,
          rendimiento_neto_real = rendimiento_neto_real*100 )

aux_xtab <- xtable( aux, digits = c(0,0,rep(2,6),0) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla Resumen situación actual de las inversiones--------------------------------------------------
aux <- inver_corte
aux$rendimiento_promedio <- aux$rendimiento_promedio*100
aux$rendimiento_promedio_real <- aux$rendimiento_promedio_real*100 
aux$participacion <- 100 * aux$participacion 
aux <- as.data.table( aux )
aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_corte', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla Rendimientos con ingresos y gastos -----------------------------------------------------
aux <- rendimientos_netos
aux$rendimiento_bruto <- aux$rendimiento_bruto * 100
aux$rendimiento_neto <- aux$rendimiento_neto * 100
aux$corte_a <- format( aux$corte_a, "%b/%Y" )
aux$corte_a <- as.character( aux$corte_a )
aux_xtab <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rend_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Tabla del detalle de los ingresos que producieron las inversiones --------------------------------
aux <- ingresos %>%
  replace(is.na(.), 0) %>%
  dplyr::select( - x2012 )
  
aux_xtab <- xtable( aux, digits = c( 0, rep( 2, 11 ) ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingre_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla del detalle de los gastos que producieron las inversiones ----------------------------------
aux <- gastos_opera %>%
  replace(is.na(.), 0) %>%
  dplyr::select( - x2012 )

aux_xtab <- xtable( aux, digits = c( 0, rep( 2, 11 ) ) )
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       compress = FALSE,
       fileEncoding="UTF-8",
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla evolución Inversiones en Créditos --------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Créditos Quirografarios') %>%
  na.omit() %>%
  mutate(rdto_prom_pond=rdto_prom_pond*100,
         rend_promedio_real=rend_promedio_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento) %>%
  arrange(ano)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_creditos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Bonos del Estado') %>%
  na.omit() %>%
  mutate(rdto_prom_pond=rdto_prom_pond*100,
         rend_promedio_real=rend_promedio_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento) %>%
  arrange(ano)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


# Tabla detalle Inversiones en Bonos del Estado Ecuatoriano al corte--------------------------------
aux <- detalle_bonos %>%
  filter( tipo_de_papel  == 'Bonos' ) %>%
  dplyr::select( -tipo_de_papel ) %>%
  group_by( referencia, fecha_de_vencimiento, tasa_cupon, amortizacion ) %>%
  mutate( valor_nominal_de_compra = sum( valor_nominal_de_compra, na.rm = TRUE ),
          saldo_valor_nominal = sum( saldo_valor_nominal, na.rm =  TRUE ) ) %>%
  ungroup( ) %>%
  distinct( referencia, fecha_de_vencimiento, tasa_cupon, amortizacion, .keep_all = TRUE ) %>%
  rbind( ., c("Total",
              NA,
              NA,
              NA,
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '846.45',
              NA ) ) %>%
  mutate_at( c( 4:7 ), as.numeric ) %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) )
  
aux_xtab <- xtable( aux, digits = c( 0, rep(0, 3), rep(2, 4), 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

# Tabla colocación de bonos de IVM------------------------------------------------------------------
aux <- detalle_bonos %>%
  filter(tipo_de_papel  == 'Bonos') %>%
  dplyr::select(-tipo_de_papel) %>%
  mutate(anio = year(fecha_de_compra)) %>%
  mutate( plazo = as.integer(difftime( fecha_de_vencimiento, fecha_de_compra,  units = "days")) ) %>%
  group_by(anio) %>%
  mutate(colocacion = sum(valor_nominal_de_compra, na.rm = TRUE)) %>%
  mutate(tasa =  sum(valor_nominal_de_compra * tasa_cupon, na.rm = TRUE) /
         colocacion) %>%
  mutate( plazo =  sum(valor_nominal_de_compra * plazo, na.rm = TRUE) /
            colocacion) %>%
  ungroup() %>%
  distinct(., anio, .keep_all = TRUE) %>%
  dplyr::select( anio, colocacion, tasa, plazo ) %>%
  arrange( anio ) %>%
  rbind( ., c("Total",
              as.character( colSums( .[,2],  na.rm =TRUE ) ),
              as.character( sum(.$colocacion * .$tasa, na.rm = TRUE) / sum(.$colocacion, na.rm = TRUE ) ),
              as.character( sum(.$colocacion * .$plazo, na.rm = TRUE) / sum(.$colocacion, na.rm = TRUE ) ) ) ) %>%
  mutate_at( c( 2:4 ), as.numeric ) 


aux_xtab <- xtable( aux, digits = c( 0, 0, rep(2, 3) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_colocacion_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


# Tabla recuperación de Bonos del Estado Ecuatoriano al corte---------------------------------------
aux <- recuperacion_bonos %>%
  filter( tipo_bono == 'Bonos del Estado',
          fecha_colocacion <= as.Date( "31/12/2022", "%d/%m/%Y" ) ) %>%
  dplyr::select( -tipo_bono ) %>%
  mutate( anio = year( fecha_cupon ) ) %>%
  group_by( anio ) %>%
  mutate( capital = sum( capital, na.rm = TRUE ),
          interes   = sum( interes  , na.rm =  TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  mutate( total = capital + interes ) %>%
  dplyr::select( anio,
                 capital,
                 interes, 
                 total ) %>%
  rbind( ., c("Total",
              as.character( colSums( .[ , 2:ncol(.) ],  na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2:ncol(.) ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_recuperacion_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla Recepción de Bonos del Estado por el 40% del pago de las pensiones---------------------------
aux <- detalle_bonos_40 %>%
  mutate( tasa = tasa * 100,
          fecha_colocacion = as.character( fecha_colocacion ),
          vencimiento = as.character( vencimiento ),
          pago_del_periodo = as.character( format( detalle_bonos_40$pago_del_periodo, "%Y-%B" ) ) )
aux_xtab <- xtable( aux, digits = c(0, 0, 0, 0, 2, 2, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_repbonos40_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Tabla evolución Inversiones en CETES--------------------------------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Certificados de Tesorería - CETES') %>%
  na.omit() %>%
  mutate(rdto_prom_pond=rdto_prom_pond*100,
         rend_promedio_real=rend_promedio_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento) %>%
  arrange(ano)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cetes_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


# Tabla detalle Inversiones en CETES al corte-------------------------------------------------------
aux <- detalle_bonos %>%
  filter( tipo_de_papel  == 'CETES' ) %>%
  dplyr::select( -tipo_de_papel ) %>%
  group_by( referencia, fecha_de_vencimiento, tasa_cupon, amortizacion ) %>%
  mutate( valor_nominal_de_compra = sum( valor_nominal_de_compra, na.rm = TRUE ),
          saldo_valor_nominal = sum( saldo_valor_nominal, na.rm =  TRUE ) ) %>%
  ungroup( ) %>%
  distinct( referencia, fecha_de_vencimiento, tasa_cupon, amortizacion, .keep_all = TRUE ) %>%
  rbind( ., c("Total",
              NA,
              NA,
              NA,
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '140.30' ) ) %>%
  mutate_at( c( 4:7 ), as.numeric )  %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) )

aux_xtab <- xtable( aux, digits = c( 0, rep(0, 3), rep(2, 4), 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cetes_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

# Tabla recuperación de CETES al corte--------------------------------------------------------------
aux <- recuperacion_bonos %>%
  filter( tipo_bono == 'CETES',
          fecha_colocacion <= as.Date( "31/12/2022", "%d/%m/%Y" ) ) %>%
  dplyr::select( -tipo_bono ) %>%
  mutate( anio = year( fecha_cupon ) ) %>%
  group_by( anio ) %>%
  mutate( capital = sum( capital, na.rm = TRUE ),
          interes   = sum( interes  , na.rm =  TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  mutate( total = capital + interes ) %>%
  dplyr::select( anio,
                 capital,
                 interes, 
                 total ) %>%
  rbind( ., c("Total",
              as.character( colSums( .[ , 2:ncol(.) ],  na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2:ncol(.) ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cetes_recuperacion_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


#Tabla Evolución Inversiones en Obligaciones--------------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Obligaciones' ) %>%
  na.omit( ) %>%
  mutate( rdto_prom_pond = rdto_prom_pond * 100,
          rend_promedio_real = rend_promedio_real * 100,
          ano = as.character( ano ) ) %>%
  select( -inflacion, -instrumento ) %>%
  arrange( ano )

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_obligaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla detalle de las inversiones en obligaciones---------------------------------------------------
aux <- detalle_obligaciones %>%
  rbind( ., c("Total",
              NA,
              NA,
              '8.80',
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '1048.96',
              NA ) ) %>%
  mutate_at( c( 4:7 ), as.numeric ) %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) )

aux_xtab <- xtable( aux, digits = c( 0, rep( 0, 3 ), rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_oblig_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

#Tabla Evolución Inversiones en Titularizaciones----------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Titularizaciones' ) %>%
  na.omit( ) %>%
  mutate( rdto_prom_pond = rdto_prom_pond * 100,
          rend_promedio_real = rend_promedio_real * 100,
          ano = as.character( ano ) ) %>%
  select( -inflacion, -instrumento ) %>%
  arrange( ano )


aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 0 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titularizaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow( aux ),
       sanitize.text.function = identity )

#Tabla detalle de las inversiones en Titularizaciones-----------------------------------------------
aux <- detalle_titularizaciones %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) ) %>%
  rbind( ., c("Total",
              NA,
              NA,
              '7.15',
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '272.14',
              NA ) ) %>%
  mutate_at( c( 4:7 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, rep( 0, 3 ), rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titul_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


# Tabla evolución inversiones en Papel Comercial----------------------------------------------------
aux <- inv_instrumento %>%
  filter( instrumento == 'Papel Comercial') %>%
  na.omit( ) %>%
  mutate(rdto_prom_pond = rdto_prom_pond * 100,
         rend_promedio_real = rend_promedio_real * 100,
         ano = as.character( ano ) ) %>%
  select( -inflacion, -instrumento ) %>%
  arrange( ano )


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pc_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla detalle de las inversiones en Papel Comercial------------------------------------------------
aux <- detalle_papel_comercial %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) ) %>%
  rbind( ., c("Total",
              NA,
              NA,
              '4,89',
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '222.46',
              NA ) ) %>%
  mutate_at( c( 4:7 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, rep( 0, 3 ), rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pc_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


#Tabla detalle de las inversiones en Certificados de depósito---------------------------------------
aux <- detalle_certificado_deposito %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) ) %>%
  rbind( ., c("Total",
              NA,
              NA,
              '7.06',
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '108.37',
              NA ) ) %>%
  mutate_at( c( 4:7 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, rep( 0, 3 ), rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cdp_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )

# Tabla evolución inversiones en Certificados de Inversión------------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Certificados de Inversión') %>%
  na.omit() %>%
  mutate(rdto_prom_pond=rdto_prom_pond*100,
         rend_promedio_real=rend_promedio_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento) %>%
  arrange(ano)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cid_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )



#Tabla detalle de las inversiones en certificados de inversión--------------------------------------
aux <- detalle_certificado_inversiones %>%
  arrange( fecha_de_compra ) %>%
  mutate( fecha_de_compra = as.character( fecha_de_compra ),
          fecha_de_vencimiento = as.character( fecha_de_vencimiento ) ) %>%
  rbind( ., c("Total",
              NA,
              NA,
              '7.00',
              as.character( colSums( .[,5:6],  na.rm =TRUE ) ),
              '106',
              NA ) ) %>%
  mutate_at( c( 4:7 ), as.numeric )

aux_xtab <- xtable( aux, digits = c( 0, rep( 0, 3 ), rep( 2, 4 ), 0 ) )
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cid_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ) - 1,
                        nrow( aux ) ),
       sanitize.text.function = identity )


#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
gc()

