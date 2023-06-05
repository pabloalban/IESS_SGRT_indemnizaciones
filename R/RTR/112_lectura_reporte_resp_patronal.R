message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tReporte Responsabilidades Patronales' )

col_nom <- c( 'provincia', 'ruc', 'sucursal', 'razon_social','tipo_de_empresa','resolucion','fecha_acuerdo',
              'cod-estado_acuerdo','estado_acuerdo','ruc_disp','valrecafi','valpagafi',
              'valor_pendiente','concepto','cedula','nombre','fecha_siniestro','glosa',
              'fecha_emis','estado_glosa','valor_glosa','fecha_cancel ','numero_exp','funcionario_emite',
              'fecha_anul_acu','func_anul_acu','motiv_anul_acu')
col_tip <- c( 'character', 'character', 'numeric', 'character','character','numeric','character','character','character','numeric ',
              'numeric','numeric','numeric','character','character','character','character','numeric',
              'character','character','numeric','character','numeric','character','character','character','character')

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data_seg, 'SENTENCIA CORTE CONST 1024/IESS-DSGRT-2022-0409-M/Reporte Responsabilidades Patronales Riesgos Trabajo_18-03-2022f.xlsx' )

reporte_resp_patronal <- read_xlsx( path = file, skip = 0, col_names = T) %>% clean_names()  #para nombres de columnas
reporte_resp_patronal <- reporte_resp_patronal %>% 
                          mutate(cedula=as.character(cedula),
                                 ruc=as.character(ruc)) %>% 
                          mutate(cedula= if_else(nchar(cedula)==9, paste0("0",cedula), cedula)) %>% 
                          mutate(fecha_acuerdo=as.Date(fecha_acuerdo,"%Y-%m-%d")) %>% 
                          mutate(fecha_siniestro=as.Date(fecha_siniestro,"%Y-%m-%d" )) %>% 
                          mutate(fecha_cancelacion=as.Date(fecha_cancelacion, "%Y-%m-%d")) %>% 
                          mutate(fecha_emision=as.Date(fecha_emision,"%Y-%m-%d")) %>% 
                          mutate(fecha_anulacion_acuerdo=as.Date(fecha_anulacion_acuerdo,"%Y-%m-%d")) %>% 
                          mutate(tipo_de_empresa=as.factor(tipo_de_empresa)) %>% 
                          mutate(provincia=as.factor(provincia))


file <- paste0( parametros$RData, 'IESS_Reg_Civil.RData' )

load(file)
rc <- rc %>% select(cedula,sexo,fecha_nacimiento)

reporte_resp_patronal <-  left_join(reporte_resp_patronal,rc,by="cedula") %>% #12.103
                          #filter(cod_estado_acuerdo!="ANU") %>%  # se quita los de codigo anulado
  mutate( fecha_nacimiento = if_else( is.na(fecha_nacimiento),
                                      mean( fecha_nacimiento, na.rm =TRUE),
                                      fecha_nacimiento ) ) %>%
  mutate( fecha_siniestro = if_else( is.na(fecha_siniestro),
                                     fecha_acuerdo,
                                     fecha_siniestro ) ) %>%
  group_by( concepto ) %>%
  mutate( fecha_siniestro = if_else( is.na(fecha_siniestro),
                                      mean( fecha_siniestro, na.rm =TRUE),
                                     fecha_siniestro ) ) %>%
  ungroup( ) %>%
  mutate( fecha_siniestro = if_else( is.na(fecha_siniestro),
                                     mean( fecha_siniestro, na.rm =TRUE),
                                     fecha_siniestro ) ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
                               enddate = fecha_siniestro,
                               units = "years",
                               precise = FALSE ) )) %>%
  mutate( fecha_siniestro = if_else( cedula == '1719989012',
                                     as.Date( "2015-05-14", "%Y-%m-%d" ),
                                     fecha_siniestro ) ) %>%
  filter( !is.na( ruc ) ) %>%
  filter( year( fecha_acuerdo ) < 2022 ) %>%
  mutate( sexo = if_else( is.na( sexo ),
                          'M',
                          sexo ) ) %>%
  mutate( sexo = if_else( sexo == 'M',
                          'H',
                          'M'))
       
 save( reporte_resp_patronal,
       file = paste0( parametros$RData_seg, 'IESS_RTR_responsabilidad_patronal.RData' ) )
#Limpiar Ram----------------------------------------------------------------------------------------
  message( paste( rep('-', 100 ), collapse = '' ) )
  rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
  gc()
  