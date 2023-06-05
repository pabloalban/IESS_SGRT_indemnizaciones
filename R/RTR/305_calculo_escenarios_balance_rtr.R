message( '\tCalculando escenarios del balance para RTR' )

# source( 'R/rtr/302_proyeccion_poblacion_rtr_new.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/303_proyeccion_beneficios_rtr.R', encoding = 'UTF-8', echo = FALSE )

# 0. Ajustes necesarios de las transiciones de estados----------------------------------------------
# source( 'R/rtr/200_ajuste_mortalidad_pensionistas_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/201_ajuste_transicion_activo_pensionista_PA_PT_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/202_ajuste_porcentaje_incapacidad_indemn_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/203_ajuste_siniestralidad_indemnizacion_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/204_ajuste_duracion_subsidio_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/205_ajuste_siniestralidad_subsidios_rtr.R', encoding = 'UTF-8', echo = FALSE )
# ## source( 'R/rtr/206_ajuste_pensiones_PA_PT_ini_rtr.R', encoding = 'UTF-8', echo = FALSE )
# ## source( 'R/rtr/207_ajuste_pensiones_PP_ini_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/208_fdp_ingresos_huerfanos_montepio_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/209_fdp_ingresos_viudas_montepio_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/210_tasa_siniestralidad_accidentes_laborales_fatales_rtr.R', encoding = 'UTF-8', echo = FALSE )
# ## source( 'R/rtr/211_ajuste_transicion_activo_invalido_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/212_ajuste_grupo_familiar_montepio_rtr.R', encoding = 'UTF-8', echo = FALSE )
# ## source( 'R/rtr/213_ajuste_pensiones_montepio_ini_rtr.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/rtr/300_calculo_tabla_decrementos_rtr.R', encoding = 'UTF-8', echo = FALSE )

# 0. 1.  Carga de hipótesis macro 
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_modelo_rp.RData' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'Hipotesis', 'coeficiente' ) ) ] )

# 1. Escenario 1 -----------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

# 1.1. Hipótesis -----------------------------------------------------------------------------------
esc$V0 <- 1202240160.65

esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           i_a = 0.0625,
                           i_r = Hipotesis[ 4, 2 ]/100,
                           i_sbu = Hipotesis[ 5, 2 ]/100,
                           i_p = Hipotesis[ 6, 2 ]/100,
                           
                           # Tasas de aportación y gasto
                           por_gas = 0.0003, # 0,03% de la masa salarial
                           apo_est = c( rep( 0.28, 41 ) ),
                           apo_cot = c( 0.002, rep( 0.0038 , 40 ) ),
                           apo_sal = c( rep( 0, 41 ) ),
                           apo_jub = 0,  # Aporte del 2.76% de los jubilados
                           rp = coeficiente, # Valor de la rp generada, respecto al aporte del SGRT
                           # Factores de calibración
                           cal_mas = 1.06083,
                           cal_pen_pa_pt = 1.886482,
                           cal_pen_pp = 1.264204,
                           cal_indm = 1.156043,
                           cal_subs = 0.1493458,
                           cal_orf = 2.409926,
                           cal_viu = 0.9694932 )

# 1.2. Factores de calculo de montepío -------------------------------------------------------------
# No están siendo utilizadas en el cálculo del balance
esc$porc_ben_orf <- 2.67756315 # Porcentaje de benefcios de orf respecto a los pensts de PA y PT
esc$porc_ben_viud <- 3.108469539 # Porcentaje de beneficios de viudez respecto a los pensts de PA y PT

esc$mont_prop_afi_orf <- 0.118449037 # Porcentaje que representan de las prestaciones
esc$mont_prop_afi_viud <- 0.254047820

# 1.3. Calculos necesarios para el modelo actuarial ------------------------------------------------
parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/rtr/304_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', esc$nombre, '.RData' ) )
rm( esc )



# 2. Escenario 2 -----------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_2'
message( '\t\t\t', esc$nombre )

# 2.1. Hipótesis -----------------------------------------------------------------------------------
esc$V0 <- 1202240160.65

esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           i_a = 0.0625,
                           i_r = Hipotesis[ 4, 2 ]/100,
                           i_sbu = Hipotesis[ 5, 2 ]/100,
                           i_p = Hipotesis[ 6, 2 ]/100,
                           
                           # Tasas de aportación y gasto
                           por_gas = 0.0003, # 0,03% de la masa salarial
                           apo_est = c( rep( 0.28, 41 ) ),
                           apo_cot = c( 0.002, rep( 0.0038 , 40 ) ),
                           apo_sal = c( rep( 0, 41 ) ),
                           apo_jub = 0,  # Aporte del 2.76% de los jubilados
                           rp = 0, # Valor de la rp generada, respecto al aporte del SGRT
                           # Factores de calibración
                           cal_mas = 1.06083,
                           cal_pen_pa_pt = 1.886482,
                           cal_pen_pp = 1.264204,
                           cal_indm = 1.156043,
                           cal_subs = 0.1493458,
                           cal_orf = 2.409926,
                           cal_viu = 0.9694932 )

# 2.2. Factores de calculo de montepío -------------------------------------------------------------
# No están siendo utilizadas en el cálculo del balance
esc$porc_ben_orf <- 2.67756315 # Porcentaje de benefcios de orf respecto a los pensts de PA y PT
esc$porc_ben_viud <- 3.108469539 # Porcentaje de beneficios de viudez respecto a los pensts de PA y PT

esc$mont_prop_afi_orf <- 0.118449037 # Porcentaje que representan de las prestaciones
esc$mont_prop_afi_viud <- 0.254047820

# 2.3. Calculos necesarios para el modelo actuarial ------------------------------------------------
parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/rtr/304_calculo_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', esc$nombre, '.RData' ) )
rm( esc )


# 5. Cálculo de primas y análisis de ratios para todos los escenarios ------------------------------
message( '\tCalculando primas y análisis de ratios' )

# 5.1 Cálculo de primas ----------------------------------------------------------------------------
source( 'R/rtr/306_calculo_prima_rtr.R', encoding = 'UTF-8', echo = FALSE )

# 5.2 Cálculo de indicadores -----------------------------------------------------------------------
# source( 'R/rtr/307_calculo_indice_rtr.R', encoding = 'UTF-8', echo = FALSE )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
