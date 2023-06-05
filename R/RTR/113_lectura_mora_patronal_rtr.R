message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de mora patronal de SGRT' )

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/Memorando Nro. IESS-DSGRT-2021-1550-M/mora_patronal_sgrt.xlsx')


#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Responsabilidad patronal al 15/10/2021-------------------------------------------------------------
rp_sgrt <- read_excel(file,
                      sheet = 'Hoja1',
                      col_names = TRUE,
                      col_types = NULL,
                      na = "",
                      skip = 0) %>% clean_names() %>%
  dplyr::select( cedula,
                 fecha_siniestro,
                 tipo_de_empresa,
                 estado_acuerdo,
                 valrecafi,
                 valpagafi,
                 valor_pendiente,
                 valor_glosa,
                 fecha_cancelacion
                 ) %>%
  mutate( fecha_siniestro = as.Date( fecha_siniestro, "%Y-%m-%d"),
          fecha_cancelacion = as.Date( fecha_cancelacion, "%Y-%m-%d") ) %>%
  filter( !(estado_acuerdo %in% c('ANULADO', 
                                  'APROBADO',
                                  'EMITIDO' ) ) )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando responsabilidad patronal' )

save( rp_sgrt,
      file = paste0( parametros$RData, 'IESS_rp_sgrt.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
