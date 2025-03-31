message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )
# Carga de información------------------------------------------------------------------------------
# load(paste0(parametros$RData, "IESS_balance.RData"))
# load( file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
# load( file = paste0( parametros$RData, 'COESCOP_esquema_pensional.RData' ) )
# load( file = paste0( parametros$RData, 'IESS_tablas_contingencia_2.RData' ) )
# load( paste0( parametros$RData, 'COESCOP_transito.RData' ) ) #trásito
# load( paste0( parametros$RData, 'COESCOP_control.RData' ) ) #control
load( parametros$macro_rdata_info )
load( paste0( parametros$RData, 'IESS_tablas_contingencia_cte.RData' ) )

REP <- new.env()

#Parámetros del seguro------------------------------------------------------------------------------
REP$corte <- parametros$anio_ini
REP$rtr_horizonte <- parametros$rtr_horizonte
REP$anio_fin_valuacion <- parametros$anio_ini + parametros$rtr_horizonte
REP$anio_ini_valuacion <- parametros$anio_ini + 1

#Contexto macroeconómico----------------------------------------------------------------------------

REP$macro_inf_acu_2001 <- format(
  filter( inflacion, anio == 2001, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2020 <- format(
  filter( inflacion, anio == 2020, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2017 <- format(
  filter( inflacion, anio == 2017, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )


REP$macro_inf_2000 <- format(
  filter( inflacion, anio == 2000, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2019 <- format(
  filter( inflacion, anio == 2019, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_2022 <- format(
  filter( inflacion, anio == 2022, mes == 12 )$inflacion_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_prom_2022 <- format(
  filter( inflacion, anio == 2022, mes == 12 )$inflacion_promedio_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_inf_acu_prom_2024 <- format(
  filter( inflacion, anio == 2024, mes == 12 )$inflacion_promedio_acumulada, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

ipa <- inflacion %>% filter(mes ==12, anio > 2012, anio < 2023) %>%
  dplyr::select(inflacion_promedio_acumulada)

REP$macro_inf_acu_prom_13_24 <- format (
  mean(ipa$inflacion_promedio_acumulada),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f')

REP$macro_subempleo_2024 <- format(
  filter( desempleo, anio == 2024, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_m_2024 <- format(
  filter( desempleo, anio == 2024, mes == 12 )$subempleo_m, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_h_2024 <- format(
  filter( desempleo, anio == 2024, mes == 12 )$subempleo_h, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_2019 <- format(
  filter( desempleo, anio == 2019, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_2020 <- format(
  filter( desempleo, anio == 2020, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_subempleo_2012 <- format(
  filter( desempleo, anio == 2012, mes == 12 )$subempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2020 <- format(
  filter( desempleo, anio == 2020, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2021 <- format(
  filter( desempleo, anio == 2021, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2024 <- format(
  filter( desempleo, anio == 2024, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2009 <- format(
  filter( desempleo, anio == 2009, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_desempleo_2019 <- format(
  filter( desempleo, anio == 2019, mes == 12 )$desempleo_nacional, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_2000 <- format(
  filter( sbu, anio == 2000 )$sbu, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_incremento_2003 <- format(
  filter( sbu, anio == 2003 )$incremento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_incremento_2008 <- format(
  filter( sbu, anio == 2008 )$incremento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_tasa_cre_2001 <- format(
  filter( sbu, anio == 2001 )$tasa_de_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_sbu_tasa_cre_2021 <- format(
  filter( sbu, anio == 2021 )$tasa_de_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

incremento_sub_7anios <- sbu %>% filter(anio > 2015, anio < 2023) %>%
  dplyr::select(tasa_de_crecimiento)

REP$macro_sub_promedio_7anios <- format (
  mean(incremento_sub_7anios$tasa_de_crecimiento),
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f')

REP$macro_sbu_2024 <- format(
  filter( sbu, anio == 2024 )$sbu, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_salarios_prom_2022 <- format(
  filter(salarios, anio == 2022 , mes ==12 )$sal_prom, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

increm <- salarios %>% filter (mes ==12 ) %>% mutate(incremento = sal_prom-lag(sal_prom)) %>% 
  mutate( tasa_crecimiento = (sal_prom- lag(sal_prom))/lag(sal_prom)*100) 

REP$macro_increm_2022 <- format(
  filter(increm, anio == 2022)$incremento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_increm_tasa_incre_2022 <- format(
  filter(increm, anio == 2022)$tasa_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_increm_tasa_incre_2008 <- format(
  filter(increm, anio == 2008)$tasa_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_increm_tasa_incre_2020 <- format(
  filter(increm, anio == 2020)$tasa_crecimiento, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

incremento_salarios_10_anios <- increm %>% filter(anio > 2012, anio < 2023) %>%
  dplyr::select(tasa_crecimiento)

REP$macro_increm_promedio_10_anios <- format(
  mean(incremento_salarios_10_anios$tasa_crecimiento), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2016 <- format(
  abs( filter(pib_real, anio == 2016)$crecimiento_pib ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

pib_real_7_anios <- pib_real %>% filter(anio > 2016, anio <= 2023) %>% 
  dplyr::select(crecimiento_pib)

REP$macro_pib_real_prom_crec_7_anios <- format(
  mean(pib_real_7_anios$crecimiento_pib), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2020 <- format(
  abs( filter(pib_real, anio == 2020)$crecimiento_pib ), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2021 <- format(
  filter(pib_real, anio == 2021)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2022 <- format(
  filter(pib_real, anio == 2022)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_pib_real_crecimiento_2023 <- format(
  filter(pib_real, anio == 2023)$crecimiento_pib, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2000_2004 <- format(
  as.numeric(dec_tasa_activa_2000_2004 <- tasas_interes %>% filter(anio == 2000, mes == 12) %>% 
               dplyr::select(tasa_activa) - tasas_interes %>% filter(anio == 2004, mes == 12) %>% 
               dplyr::select(tasa_activa)), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2005_2016 <- format(
  as.numeric(dec_tasa_activa_2005_2016 <- tasas_interes %>% filter(anio == 2005, mes == 12) %>% 
               dplyr::select(tasa_activa) - tasas_interes %>% filter(anio == 2016, mes == 12) %>% 
               dplyr::select(tasa_activa)), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2005 <- format(
  filter(tasas_interes, anio == 2005, mes == 12)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2016 <- format(
  filter(tasas_interes, anio == 2016, mes == 12)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2020_7 <- format(
  filter(tasas_interes, anio == 2020, mes == 7)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2018_5 <- format(
  filter(tasas_interes, anio == 2018, mes == 5)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_act_2000_8 <- format(
  filter(tasas_interes, anio == 2000, mes == 8)$tasa_activa, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_2000 <- format(
  filter(tasas_interes, anio == 2000, mes == 12)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_2004 <- format(
  filter(tasas_interes, anio == 2004, mes == 12)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_2004_2016 <- format(
  as.numeric(dec_tasa_pas_2000_2004 <- tasas_interes %>% filter(anio == 2016, mes == 12) %>% 
               dplyr::select(tasa_pasiva) - tasas_interes %>% filter(anio == 2004, mes == 12) %>% 
               dplyr::select(tasa_pasiva)), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_min <- format(
  filter(tasas_interes, anio == 2005, mes == 1)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_tasas_interes_tasa_pas_max <- format(
  filter(tasas_interes, anio == 2000, mes == 4)$tasa_pasiva, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

spr_prom <- tasas_interes %>% filter(mes == 12, anio > 2015, anio <= 2024) %>% 
  dplyr::select(spread)

REP$macro_tasas_interes_spread <- format(
  mean(spr_prom$spread), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roa_ban_est_2020 <- format(
  as.numeric(a <- (filter(roa, anio == 2020)$banco_estado)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roa_ban_priv_2020 <- format(
  as.numeric(a <- (filter(roa, anio == 2020)$b_privados)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roa_cfn_2020 <- format(
  as.numeric(a <- (filter(roa, anio == 2020)$cfn)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_ban_est_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$banco_estado)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_ban_priv_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$b_privados)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_banecu_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$ban_ecuador)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$macro_roe_int_pub_2020 <- format(
  as.numeric(a <- (filter(roe, anio == 2020)$int_publicas)*100), 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

#Demografía de los agentes de la CTE----------------------------------------------------------------

REP$dem_n_m <- format( filter(tabla_prom, sexo == 'M' )$n, 
  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
  
REP$dem_n_f <- format( filter(tabla_prom, sexo == 'F' )$n, 
                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_edad_m <- format( filter(tabla_prom, sexo == 'M' )$edad, 
                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_edad_f <- format( filter(tabla_prom, sexo == 'F' )$edad, 
                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sal_m <- format( filter(tabla_prom, sexo == 'M' )$salario, 
                          nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_sal_f <- format( filter(tabla_prom, sexo == 'F' )$salario, 
                          nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_n_porc_m <- format( tabla_rangos_edad_cargos$por_sexo_M[ nrow( tabla_rangos_edad_cargos ) ], 
                       nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_n_porc_f <- format( tabla_rangos_edad_cargos$por_sexo_F[ nrow( tabla_rangos_edad_cargos ) ], 
                            nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_n_dist_max <- format( max( tabla_rangos_edad_cargos$total[ -nrow( tabla_rangos_edad_cargos )] ),
                            nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_n_dist_porc_max <- format( max( tabla_rangos_edad_cargos$por_total[ -nrow( tabla_rangos_edad_cargos ) ] ),
                              nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_n_der_max <- format( filter( tabla_rangos_edad_cargos, rango_edad == '[ 60 - 70 ]' )$total,
                              nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dem_n_der_porc_max <- format( filter( tabla_rangos_edad_cargos, rango_edad == '[ 60 - 70 ]' )$por_total,
                                  nsmall = 2, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# # Demografía de los agentes de Control--------------------------------------------------------------
# 
# ciudades <- c( 'Ambato',
#                'Duran',
#                'Esmeraldas',
#                'Guayaquil',
#                'Latacunga',
#                'Machala',
#                'Portoviejo',
#                'Quito',
#                'Santo Domingo' )
# 
# nombre <- c( 'ambato',
#              'duran',
#              'esmeraldas',
#              'guayaquil',
#              'latacunga',
#              'machala',
#              'p_viejo',
#              'quito',
#              's_domingo')
# 
# for ( i in 1:length( ciudades ) ) {
#   
#   ciudad <- ciudades[ i ]
#   
#   message( '\tGenerando auto información agentes de control de ', ciudad )
#   
#   expr <- expression( {
#     
#   REP$c_nombre_dem_1 <- format( fun_auto_dem( control, {{ciudad}} )$dem_1,
#                             digits = 2, nsmall = 0, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_1_m <- format( fun_auto_dem( control, {{ciudad}} )$dem_1_m,
#                               digits = 2, nsmall = 0, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_1_f <- format( fun_auto_dem( control, {{ciudad}} )$dem_1_f,
#                               digits = 2, nsmall = 0, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_2_m <- format( fun_auto_dem( control, {{ciudad}} )$dem_2_m,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_2_f <- format( fun_auto_dem( control, {{ciudad}} )$dem_2_f,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_3 <- format( fun_auto_dem( control, {{ciudad}} )$dem_3,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_4_1 <- format( fun_auto_dem( control, {{ciudad}} )$dem_4[ 1 ],
#                               digits = 2, nsmall = 0, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_4_2 <- format( fun_auto_dem( control, {{ciudad}} )$dem_4[ 2 ],
#                               digits = 2, nsmall = 0, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_4_3 <- format( fun_auto_dem( control, {{ciudad}} )$dem_4[ 3 ],
#                               digits = 2, nsmall = 0, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_5_1 <- format( fun_auto_dem( control, {{ciudad}} )$dem_5[ 1 ],
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_5_2 <- format( fun_auto_dem( control, {{ciudad}} )$dem_5[ 2 ],
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   
#   REP$c_nombre_dem_5_3 <- format( fun_auto_dem( control, {{ciudad}} )$dem_5[ 3 ],
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
#   })
# 
#   expr <- gsub( 'nombre', nombre[ i ], deparse( expr ) )
#   eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
# }
# 
# # Demografía de los agentes de tránsito-------------------------------------------------------------
# 
# ciudades <- c( 'Cuenca',
#                'Duran',
#                'Esmeraldas',
#                'Guayaquil',
#                'Ibarra',
#                'Latacunga',
#                'Machala',
#                'Quito',
#                'Santo Domingo',
#                'Ambato',
#                'Riobamba',
#                'Loja' )
# 
# nombre <- c( 'cuenca',
#              'duran',
#              'esmeraldas',
#              'guayaquil',
#              'ibarra',
#              'latacunga',
#              'machala',
#              'quito',
#              's_domingo',
#              'ambato',
#              'riobamba',
#              'loja' )
# 
# for ( i in 1:length( ciudades ) ) {
#   
#   ciudad <- ciudades[ i ]
#   
#   message( '\tGenerando auto información agentes de tránsito de ', ciudad )
#   
#   expr <- expression( {
#     
#     REP$t_nombre_dem_1 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_1,
#                                   digits = 2, nsmall = 0, big.mark = '.',
#                                   decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_1_m <- format( fun_auto_dem( transito, {{ciudad}} )$dem_1_m,
#                                     digits = 2, nsmall = 0, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_1_f <- format( fun_auto_dem( transito, {{ciudad}} )$dem_1_f,
#                                     digits = 2, nsmall = 0, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_2_m <- format( fun_auto_dem( transito, {{ciudad}} )$dem_2_m,
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_2_f <- format( fun_auto_dem( transito, {{ciudad}} )$dem_2_f,
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_3 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_3,
#                                   digits = 2, nsmall = 2, big.mark = '.',
#                                   decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_4_1 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_4[ 1 ],
#                                     digits = 2, nsmall = 0, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_4_2 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_4[ 2 ],
#                                     digits = 2, nsmall = 0, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_4_3 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_4[ 3 ],
#                                     digits = 2, nsmall = 0, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_5_1 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_5[ 1 ],
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_5_2 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_5[ 2 ],
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#     
#     REP$t_nombre_dem_5_3 <- format( fun_auto_dem( transito, {{ciudad}} )$dem_5[ 3 ],
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#   })
#   
#   expr <- gsub( 'nombre', nombre[ i ], deparse( expr ) )
#   eval( eval( parse( text = expr ) ), envir = .GlobalEnv )
# }
# 
# # Escenario único-----------------------------------------------------------------------------------
# balance_anual <- as.data.table(balance_anual)
# 
# REP$bal_act_esc_1 <- format( balance_anual[  anio == parametros$anio_fin  ]$V,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_1 <- format( balance_anual[ anio == parametros$horizonte ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# 
# REP$aporte_coescop_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$aporte_coescop_vap,
#                               digits = 2, nsmall = 2, big.mark = '.', 
#                               decimal.mark = ',', format = 'f' )
#  
# REP$reserva_a_pagar_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$reserva_a_pagar_vap,
#                                    digits = 2, nsmall = 2, big.mark = '.', 
#                                    decimal.mark = ',', format = 'f' )
# 
# REP$reserva_matematica_vap  <- format( balance_anual[  anio == parametros$anio_fin  ]$reserva_matematica_vap ,
#                                    digits = 2, nsmall = 2, big.mark = '.', 
#                                    decimal.mark = ',', format = 'f' )
# 
# 
# REP$montepio_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$montepio_vap,
#                                    digits = 2, nsmall = 2, big.mark = '.', 
#                                    decimal.mark = ',', format = 'f' )
# 
# 
# REP$gastos_administrativos_vap <- format( balance_anual[  anio == parametros$anio_fin  ]$gastos_administrativos_vap,
#                                    digits = 2, nsmall = 2, big.mark = '.', 
#                                    decimal.mark = ',', format = 'f' )
# 
# 
# REP$egreso_total_vap  <- format( balance_anual[  anio == parametros$anio_fin  ]$egreso_total_vap,
#                                           digits = 2, nsmall = 2, big.mark = '.', 
#                                           decimal.mark = ',', format = 'f' )
# 
# #Balance corriente----------------------------------------------------------------------------------
# 
# #Impacto--------------------------------------------------------------------------------------------
# impacto <- as.data.table( impacto )
# 
# REP$impacto  <- format( impacto[  anio == parametros$anio_fin  ]$total_vap,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$aporte_ivm_vap <- format( impacto[  anio == parametros$anio_fin  ]$aporte_ivm_vap  ,
#                         digits = 2, nsmall = 2, big.mark = '.', 
#                         decimal.mark = ',', format = 'f' )
# 
# REP$aporte_ivm_vap_millones <- format( round( impacto[  anio == parametros$anio_fin  ]$aporte_ivm_vap / 1000000, 2 ),
#                               digits = 2, nsmall = 2, big.mark = '.', 
#                               decimal.mark = ',', format = 'f' )
# 
# REP$aporte_salud_vap <- format( impacto[  anio == parametros$anio_fin  ]$aporte_salud_vap  ,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
