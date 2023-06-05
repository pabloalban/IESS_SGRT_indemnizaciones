message(paste(rep("-", 100), collapse = ""))
#Cargando Datos-------------------------------------------------------------------------------------
message("\tCargando ILO pension inputs")
load(paste0(parametros$RData, "ILO_pension_inputs.RData"))



#Recalculando la masa salarial con fórmula del Ilo-pension------------------------------------------
asg <- asg %>%
  mutate( asg_lag_t = lag( asg ))

SALg_t_1 <- expand.grid( t = c( 2021 ), x = c( 0:105 ), sexo = c( male, female ) ) %>%
  left_join( ., asg, by = 't' ) %>%
  left_join( ., Isal, by = c( 'sexo', 'x' ) ) %>%
  left_join( ., ITsal, by = c( 'sexo', 'x' ) ) %>%
  mutate( ITsal_lag_x = lag( ITsal ) ) %>%
  left_join( ., csact, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., csinact, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., cent, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., Tsal, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., sal, by = c( 't', 'sexo', 'x' ) ) %>%
  replace(is.na(.), 0) %>%
  mutate( SALg = Isal * (1 + asg_lag_t) * ( csact + csinact )  +  ITsal * cent ) %>%
  group_by( sexo ) %>%
  mutate( SALg_teorico = sum( SALg, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  left_join( ., salg, by = c('t', 'sexo' ) ) %>%
  mutate( corrector = salm / SALg_teorico ) %>%
  mutate( SALg = SALg * corrector ) %>%
  as_tibble( )

aux <- SALg_t_1 %>%
  group_by( sexo ) %>%
  mutate( masa = sum( SALg, na.rm = TRUE ) ) %>%
  distinct( ., sexo, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, masa )



SALg_t <- expand.grid( t = c( 2022:2060 ), x = c( 0:105 ), sexo = c( male, female ) ) %>%
  left_join( ., asg, by = 't' ) %>%
  left_join( ., Isal, by = c( 'sexo', 'x' ) ) %>%
  left_join( ., ITsal, by = c( 'sexo', 'x' ) ) %>%
  mutate( ITsal_lag_x = lag( ITsal ) ) %>%
  left_join( ., csact, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., csinact, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., cent, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., Tsal, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., sal, by = c( 't', 'sexo', 'x' ) ) %>%
  replace(is.na(.), 0) %>%
  mutate( SALg = sal * (1 + asg_lag_t) * ( csact + csinact )  +  Tsal * cent ) %>%
  group_by( t, sexo ) %>%
  mutate( SALg_teorico = sum( SALg, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  left_join( ., salg, by = c('t', 'sexo' ) ) %>%
  mutate( corrector = salm / SALg_teorico ) %>%
  mutate( SALg = SALg * corrector ) %>%
  as_tibble( )

SALg <- rbind( SALg_t_1, SALg_t )


# aux <- SALg %>%
#   group_by( t, sexo ) %>%
#   mutate( masa = sum( SALg, na.rm = TRUE ) ) %>%
#   distinct( ., sexo, .keep_all = TRUE ) %>%
#   dplyr::select( t, sexo, masa )

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando masa salarial' )

save( SALg,
      file = paste0( parametros$RData, 'IESS_proy_masa_salarial.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()