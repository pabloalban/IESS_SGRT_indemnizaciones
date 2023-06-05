message(paste(rep("-", 100), collapse = ""))
#Cargando Datos-------------------------------------------------------------------------------------
message("\tCargando ILO pension inputs")
load(paste0(parametros$RData, "ILO_pension_inputs.RData"))


#Salario teórico------------------------------------------------------------------------------------

Tsal_cal <- expand.grid( t = c( 2021:2060 ), x = c( 0:105 ), sexo = c( male, female ) ) %>%
  left_join( ., ITsal,  by = c( 'sexo', 'x' ) ) %>%
  left_join( ., asg , by = 't' ) %>%
  mutate( Tsal_cal = ITsal ) %>%
  arrange( x, sexo, t ) %>%
  group_by( sexo, x) %>%
  mutate( i = cumprod( 1 + asg )) %>% 
  mutate( Tsal_cal = ifelse ( t > 2021,
                              lag(i) * Tsal_cal,
                              Tsal_cal ) ) %>%
  ungroup( ) %>%
  left_join( ., Tsal, by = c( 'sexo', 'x', 't' ) ) %>%
  mutate( error = abs(Tsal_cal - Tsal )/ Tsal)


sal_cal <- expand.grid( t = c( 2021:2060 ), x = c( 0:105 ), sexo = c( male, female ) ) %>%
  left_join( ., Isal,  by = c( 'sexo', 'x' ) ) %>%
  left_join( ., asg , by = 't' ) %>%
  left_join( ., csact,  by = c( 'sexo', 'x', 't' ) ) %>%
  left_join( ., csinact,  by = c( 'sexo', 'x', 't' ) ) %>%
  left_join( ., cent,  by = c( 'sexo', 'x', 't' ) ) %>%
  left_join( ., ITsal,  by = c( 'sexo', 'x' ) ) %>%
  left_join( ., Tsal,  by = c( 'sexo', 'x', 't' ) ) %>%
  arrange( t, sexo, x ) %>%
  mutate( lag_Isal = lag( Isal ) ) %>%
  mutate( lag_ITsal = lag( ITsal ) ) %>%
  arrange( x, sexo, t ) %>%
  mutate( lag_asg = lag( asg ) ) %>%
  replace(is.na(.), 0) %>%
  mutate( sal_cal = ( lag_Isal * (1 + lag_asg) * ( csact +  csinact ) * ITsal / lag_ITsal ) / ( csact + csinact + cent ) + ( ITsal * cent ) / ( csact + csinact + cent ) ) %>%
  mutate( sal_cal = if_else( x == '15' & t == '2021',
                             ITsal,
                             sal_cal ) ) %>%
  arrange( t, sexo, x ) %>%
  mutate( lag_sal_x = lag( sal_cal ) ) %>%
  mutate( lag_Tsal = lag( Tsal ) ) %>%
  arrange( sexo, x, t ) %>%
  mutate( lag_sal_x_t = lag( lag_sal_x ) ) %>%
  arrange( x, sexo, t ) %>%
  replace(is.na(.), 0) %>%
  mutate( sal_cal = if_else( x == '15' & t > 2021,
                             Tsal,
                             sal_cal ) ) %>%
  as.data.table( )


for ( g in c(male, female ) ){
  for (T in 2022:2060) {
    for ( X in 16:105 ) {
      sal_cal[ sexo == g & t == T & x == X ]$sal_cal = ( sal_cal[ sexo == g & t == T-1 & x == X -1]$sal_cal * ( 1 + sal_cal[ sexo == g & t == T & x == X ]$lag_asg) * ( sal_cal[ sexo == g & t == T & x == X ]$csact +  sal_cal[ sexo == g & t == T & x == X ]$csinact ) * sal_cal[ sexo == g & t == T & x == X ]$Tsal / sal_cal[ sexo == g & t == T & x == X ]$lag_Tsal ) / ( sal_cal[ sexo == g & t == T & x == X ]$csact + sal_cal[ sexo == g & t == T & x == X ]$csinact + sal_cal[ sexo == g & t == T & x == X ]$cent ) + ( sal_cal[ sexo == g & t == T & x == X ]$Tsal * sal_cal[ sexo == g & t == T & x == X ]$cent ) / ( sal_cal[ sexo == g & t == T & x == X ]$csact + sal_cal[ sexo == g & t == T & x == X ]$csinact + sal_cal[ sexo == g & t == T & x == X ]$cent )
  
    }
  }
}

sal_cal <-  as_tibble( sal_cal ) %>%
  left_join( ., sal,  by = c( 'sexo', 'x', 't' ) ) %>%
  mutate( error = 100 * abs(sal_cal - sal) /sal )

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando masa salarial' )

save( SALg,
      file = paste0( parametros$RData, 'IESS_proy_salarios.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()

