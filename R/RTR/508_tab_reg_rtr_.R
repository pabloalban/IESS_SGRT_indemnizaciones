message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/rtr/509_tab_plantilla_modelos_rtr.R', encoding = 'UTF-8', echo = FALSE )

#Cargado Rdatas-------------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_RTR_modelo_rp.RData' ) )

#Tabla del modelo de estimación ----------------------------------------------------------------

aux <- capture.output(stargazer(mod,
                                header=FALSE,
                                se=list(NULL),
                                dep.var.caption  = "Variable dependiente:",
                                dep.var.labels.include = FALSE,
                                column.labels=c("Valor de la RP del SGRT"),
                                covariate.labels = "Aportes\\;al\\;SGRT",
                                align=FALSE, 
                                style = "all",
                                decimal.mark = ",",
                                digit.separator=".",
                                nobs=FALSE,
                                model.numbers=FALSE,
                                df=FALSE,
                                digits=5,
                                digits.extra=3,
                                keep.stat = c("ll"),
                                table.layout ="-lc-t-s-a-",
                                label = "tab:iess_mod_rp",
                                title = "Estimaci\\'{o}n del porcentaje promedio anual de las RP respecto a los aportes del SGRT",
                                #single.row=TRUE,
                                notes.align = "l",
                                #notes = "Nota:",
                                #type = "text",
                                add.lines=list(
                                  c("Observaciones", 
                                    format(nobs(mod),nsmall =2,decimal.mark = ",",big.mark = ".") ),
                                  c("$AIC$",
                                    format(AIC(mod),nsmall =2,decimal.mark = ",",big.mark = ".") ),
                                  c("$R^{2}$",
                                    format(summary(mod)$r.squared,nsmall =2,decimal.mark = ",",big.mark = ".") ),
                                  c("$R^{2}$\\;ajustado",
                                    format(summary(mod)$adj.r.squared,nsmall =2,decimal.mark = ",",big.mark = ".") ),
                                  c("$F$ - estad\\'{i}stico",
                                    paste0(format(anova(mod)$`F value`[1],nsmall =2,decimal.mark = ",",big.mark = "."),"$^{***}$") )
                                )))
aux<-aux[c(3:(NROW(aux)-2))]
star_tex_write(aux,file = paste0( parametros$resultado_tablas, 'iess_mod_rp' , '.tex' ))

#Tabla de los coeficeintes \alpha con pagos indebidos-----------------------------------------------


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
