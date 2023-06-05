message(paste(rep("-", 100), collapse = ""))
#Cargando Datos-------------------------------------------------------------------------------------
message("\tCargando ILO pension inputs")
load(paste0(parametros$RData, "ILO_pension_inputs.RData"))

#Agrupando proyecciones poblacionales a 2060--------------------------------------------------------

proy_pob <- expand.grid( t = c( 2021 ), x = c( 0:105 ), sexo = c( male, female ) ) %>%
  left_join( ., q, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., ir, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., er, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., qi, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., LF, by = c( 't', 'sexo' ) ) %>%
  left_join( ., actgx, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., inactx, by = c( 't', 'sexo', 'x' ) ) %>%
  mutate( ninactx = er * ( 1 - q ) * ( 1 - ir ) * actgx ) %>%
  left_join( ., nentx, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., oldage, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., oldage_ben, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., nret, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., nretfactgx, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., nretfinactx, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., inact_oldage, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., wid, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., orph, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., dis, by = c( 't', 'sexo', 'x' ) ) %>%
  left_join( ., ndisgx, by = c( 't', 'sexo', 'x' ) ) %>%
  as_tibble( )
  

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando proyecciones poblacionales' )

save( proy_pob,
      file = paste0( parametros$RData, 'IESS_proy_demografia.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()