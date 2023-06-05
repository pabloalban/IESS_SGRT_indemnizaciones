message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de las inversiones' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_RTR_inversiones.xlsx' )


#Carga de recursos administrados por el BIESS-------------------------------------------------------
recurs_adm_biess <- read_excel( file,
                                sheet = 'recur_adm_biess',
                                col_names = TRUE,
                                col_types = NULL,
                                na = "", 
                                skip = 0) %>% clean_names( )

inver_corte <- read_excel( file,
                           sheet = 'corte_22',
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0) %>% clean_names( )

rendimientos_netos <- read_excel( file,
                                  sheet = 'rend_netos',
                                  col_names = TRUE,
                                  col_types = NULL,
                                  na = "",
                                  skip = 0 ) %>% clean_names( ) %>%
  mutate( corte_a = as.Date( corte_a, "%d/%m/%Y" ) )

rendimiento_neto_hist <- read_excel( file,
                                     sheet = 'rendimiento_neto_hist',
                                     col_names = TRUE,
                                     col_types = NULL,
                                     na = "",
                                     skip = 0 ) %>% clean_names( ) %>%
  mutate( periodo = as.Date( periodo, "%d/%m/%Y" ) )

ingresos <- read_excel( file,
                        sheet = 'ingresos',
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names( )

gastos_opera <- read_excel( file,
                            sheet = 'egresos',
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0 ) %>% clean_names( )

inv_instrumento <- read_excel( file,
                               sheet = 'instrumento',
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0 ) %>% clean_names( )

detalle_bonos <- read_excel( file,
                             sheet = 'bonos_22',
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha_de_compra = as.Date( fecha_de_compra, "%d/%m/%Y" ) ) %>%
  mutate( fecha_de_vencimiento = as.Date( fecha_de_vencimiento, "%d/%m/%Y" ) )


recuperacion_bonos <- read_excel( file,
                                  sheet = 'recuperacion_22',
                                  col_names = TRUE,
                                  col_types = NULL,
                                  na = "",
                                  skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha_colocacion  = as.Date( fecha_colocacion , "%d/%m/%Y" ) ) %>%
  mutate( fecha_cupon = as.Date( fecha_cupon, "%d/%m/%Y" ) ) %>%
  mutate( fecha_vencimiento = as.Date( fecha_vencimiento, "%d/%m/%Y" ) )

detalle_bonos_40 <- read_excel( file,
                                sheet = 'bonos40',
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0 ) %>% clean_names( ) %>%
  mutate(fecha_colocacion = as.Date( fecha_colocacion, "%d/%m/%Y" ),
         vencimiento = as.Date( vencimiento, "%d/%m/%Y" ),
         pago_del_periodo = as.Date( pago_del_periodo, "%d/%m/%Y" ) )

detalle_obligaciones <- read_excel( file,
                                    sheet = 'obli_22',
                                    col_names = TRUE,
                                    col_types = NULL,
                                    na = "",
                                    skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha_de_compra = as.Date( fecha_de_compra, "%d/%m/%Y" ) ) %>%
  mutate( fecha_de_vencimiento = as.Date( fecha_de_vencimiento, "%d/%m/%Y" ) )

detalle_titularizaciones <- read_excel( file,
                                        sheet = 'titularizacion_22',
                                        col_names = TRUE,
                                        col_types = NULL,
                                        na = "",
                                        skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha_de_compra = as.Date( fecha_de_compra, "%d/%m/%Y" ) ) %>%
  mutate( fecha_de_vencimiento = as.Date( fecha_de_vencimiento, "%d/%m/%Y" ) )

detalle_papel_comercial <- read_excel( file,
                                       sheet = 'pc_22',
                                       col_names = TRUE,
                                       col_types = NULL,
                                       na = "",
                                       skip = 0) %>% clean_names() %>%
  mutate( fecha_de_compra = as.Date( fecha_de_compra, "%d/%m/%Y" ) ) %>%
  mutate( fecha_de_vencimiento = as.Date( fecha_de_vencimiento, "%d/%m/%Y" ) )

detalle_certificado_inversiones <- read_excel( file,
                                               sheet = 'cid_22',
                                               col_names = TRUE,
                                               col_types = NULL,
                                               na = "",
                                               skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha_de_compra = as.Date( fecha_de_compra, "%d/%m/%Y" ) ) %>%
  mutate( fecha_de_vencimiento = as.Date( fecha_de_vencimiento, "%d/%m/%Y" ) )


detalle_certificado_deposito <- read_excel( file,                                               
                                            sheet = 'cdp_22',
                                            col_names = TRUE,
                                            col_types = NULL,
                                            na = "",
                                            skip = 0 ) %>% clean_names( ) %>%
  mutate( fecha_de_compra = as.Date( fecha_de_compra, "%d/%m/%Y" ) ) %>%
  mutate( fecha_de_vencimiento = as.Date( fecha_de_vencimiento, "%d/%m/%Y" ) )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando inversiones en un solo data.frame' )

save( recurs_adm_biess,
      inver_corte,
      rendimientos_netos,
      ingresos,
      gastos_opera,
      inv_instrumento,
      detalle_bonos,
      detalle_bonos_40,
      detalle_obligaciones,
      detalle_titularizaciones,
      detalle_papel_comercial,
      detalle_certificado_inversiones,
      detalle_certificado_deposito,
      recuperacion_bonos,
      rendimiento_neto_hist,
      file = paste0( parametros$RData_seg, 'IESS_RTR_inversiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()