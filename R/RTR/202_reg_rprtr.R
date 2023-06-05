message(paste(rep("-", 100), collapse = ""))

# Cargando información -----------------------------------------------------------------------------
message("\tCargando datos")
load( file = paste0( parametros$RData, 'IESS_SGO_masa_afiliados.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_mora_patronal.RData' ) )
load( file =  paste0( parametros$RData_seg, 'IESS_RTR_causas_desfinanciamiento.RData' ) )
# Preparando datos para la regresión ---------------------------------------------------------------

aux <- afi_mensual %>%
  group_by( anio ) %>%
  mutate( masa_salarial = sum( masa_salarial, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 masa_salarial ) %>%
  left_join( ., evo_rp_montos_anio_pendientes, by ='anio' ) %>%
  filter( anio >= 2010, anio <= 2021 ) %>%
  mutate( prima_sgrt = c( rep( 0.0055, 5 ), 0.004916667, rep(0.002, 5), 0.0038 ) ) %>%
  mutate( aporte_sgrt = prima_sgrt * masa_salarial ) %>%
  filter( anio >= 2014 )



# Regresión para encontrar la función de rp---------------------------------------------------------
message("\tEstimando alpha del modelo de rp")

mod <- lm( monto ~ (aporte_sgrt) + 0, data = aux)

coeficiente <- as.numeric( c(mod$coefficients) )

# Guardando Rdata con los modelos de beneficios ----------------------------------------------------
save(mod, 
     coeficiente,
     file = paste0(parametros$RData_seg, "IESS_RTR_modelo_rp.RData")
)
# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
