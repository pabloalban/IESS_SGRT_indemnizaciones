message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la tasa de siniestralidad")
load(paste0(parametros$RData, "IESS_tab_indemnizaciones.RData"))

message("\tAjuste de coeficientes de Indemnización del SGRT")

# Interpolando la tasa de uso del seguro de desempleo-----------------------------------------------
age.grid <- c( seq( 15, 115, 1 ) )

# Hombres-------------------------------------------------------------------------------------------
coef_ind_int_m <- coef_ind_pp_edad_sexo %>%
  filter(sexo == "M")

aux <- coef_ind_int_m %>% filter( !( edad %in% c(18, 73, 76, 78, 87 ) ) )

mod <- smooth.spline(aux$edad, aux$coef_ind_pp, df = 5)

pred <- data.frame(edad = age.grid, coef_ind_pp_int = predict(mod, age.grid, deriv = 0)[["y"]])
coef_ind_int_m <- left_join(pred, coef_ind_int_m, by = "edad") %>%   mutate( sexo = "M")

# Gráfica del ajuste
plot(coef_ind_int_m$edad,coef_ind_int_m$coef_ind_pp,col="grey",xlab="Age",ylab="Wages")
lines(coef_ind_int_m$edad,
      coef_ind_int_m$coef_ind_pp_int)


# Mujer---------------------------------------------------------------------------------------------
coef_ind_int_f <- coef_ind_pp_edad_sexo %>%
  filter(sexo == "F")

aux <- coef_ind_int_f %>% filter( !( edad %in% c( 20, 21, 72, 58, 70, 72, 66, 69 ) ) )

mod <- smooth.spline(aux$edad, aux$coef_ind_pp, df = 5)

pred <- data.frame(edad = age.grid, coef_ind_pp_int = predict(mod, age.grid, deriv = 0)[["y"]])
coef_ind_int_f <- left_join(pred, coef_ind_int_f, by = "edad") %>%   mutate( sexo = "F")

# Gráfica del ajuste
plot(coef_ind_int_f$edad,coef_ind_int_f$coef_ind_pp,col="grey",xlab="Age",ylab="Wages")
lines(coef_ind_int_f$edad,
      coef_ind_int_f$coef_ind_pp_int)


coef_ind_pp_int <- rbind( coef_ind_int_m,
                          coef_ind_int_f )

# Guardar la tasa de uso interpolada en un Rdata ---------------------------------------------------
message("\tGuardando tasa interpolada de uso del Seguro de Desempleo")


save(coef_ind_pp_int,
     file = paste0(parametros$RData_seg, "IESS_coef_indemnizacion_int.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
