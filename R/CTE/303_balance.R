message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_reserva_matematica.RData" ) ) 
load( paste0( parametros$RData, "IESS_proy_coescop.RData" ) )

message( '\tGenerando Balance corriente y actuarial' )

# 0. Parámetros-------------------------------------------------------------------------------------
i_a <- 0.0625

# 1. Aportes desde 2023 por el COESCOP--------------------------------------------------------------
aportes <- malla_ivm %>%
  filter( anio >= 2025 & anio <= 2049 ) %>%
  group_by( anio ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )

aportes_ins <- malla_ivm %>%
  filter( anio >= 2025 & anio <= 2049 ) %>%
  group_by( anio, tipo ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., tipo, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 tipo,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop )

jubilados_ins <- malla_coescop %>%
  filter( anio >= 2025 & anio <= 2049 ) %>%
  group_by( anio, tipo ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., tipo, anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, tipo,  n_old ) %>%
  arrange( tipo, anio )

jubilados <- malla_coescop %>%
  filter( anio >= 2025 & anio <= 2049 ) %>%
  group_by( anio ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, n_old ) %>%
  arrange( anio )

reserva_tipo_anio <- reserva_matematica %>% 
  group_by( anio, tipo ) %>% 
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos = sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>% 
  ungroup( ) %>% 
  distinct( anio, tipo, .keep_all = TRUE ) %>% 
  dplyr::select( anio, tipo, reserva_matematica, montepio, gastos_administrativos, egreso_total ) %>% 
  arrange( tipo, anio )


reserva_matematica_ins <- reserva_matematica %>%
  group_by( anio, tipo ) %>%
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos =  sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, tipo, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 tipo,
                 n_old,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total )

# 2. Impacto----------------------------------------------------------------------------------------
impacto <- aportes %>%
  dplyr::select( -aporte_coescop ) %>%
  mutate( total_cor = aporte_ivm + aporte_salud ) %>%
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2025',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_ivm_vap = cumsum( v * aporte_ivm ),
          aporte_salud_vap = cumsum( v * aporte_salud ),
          total_vap = cumsum( v * total_cor ) )

## 2.1. Impacto por entidad-------------------------------------------------------------------------
aux <- malla_ivm %>%
  filter( anio >= 2025 & anio <= 2049 ) %>%
  group_by( anio, tipo ) %>%
  mutate( aporte_ivm = sum( aporte_ivm, na.rm = TRUE ),
          aporte_salud = sum( aporte_salud, na.rm = TRUE ),
          aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., anio, tipo,  .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 tipo,
                 aporte_ivm,
                 aporte_salud,
                 aporte_coescop ) %>%
  dplyr::select( -aporte_coescop ) %>%
  mutate( total_cor = aporte_ivm + aporte_salud ) %>%
  group_by( tipo ) %>%
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2025',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_ivm_vap = cumsum( v * aporte_ivm ),
          aporte_salud_vap = cumsum( v * aporte_salud ),
          total_vap = cumsum( v * total_cor ) ) %>%
  ungroup( )

impacto_entidades <- aux %>%
  filter( anio == 2049)

sum(impacto_entidades$total_vap)

sum(impacto_entidades$aporte_ivm)

#1. Balance-----------------------------------------------------------------------------------------
reserva_mat_anual <- reserva_matematica %>%
  group_by( anio ) %>%
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos =  sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total )


balance_anual <- data.frame( anio = c( 2025:2049 ) ) %>%
  left_join( ., aportes, by = 'anio' ) %>%
  left_join( ., jubilados, by = 'anio' ) %>%
  left_join( .,reserva_mat_anual , by = 'anio' ) %>%
  mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
  mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
  mutate( V_cor = ingresos_total - egreso_total ) %>%
  dplyr::select( anio,
                 n_old,
                 aporte_coescop,
                 reserva_a_pagar,
                 ingresos_total,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total,
                 V_cor ) %>%
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2025',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
          reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
          ingresos_total_vap = cumsum( v * ingresos_total  ),
          reserva_matematica_vap = cumsum( v * reserva_matematica   ),
          montepio_vap = cumsum( v * montepio   ),
          gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
          egreso_total_vap = cumsum( v * egreso_total  ),
          V_cap = r * cumsum( v * V_cor ),
          V = v *  V_cap )

# 2. Aportes por instituciones----------------------------------------------------------------------

aportes_ins <- malla_ivm %>%
  filter( anio >= 2025 & anio <= 2049 ) %>%
  group_by( anio, tipo ) %>%
  mutate( aporte_coescop = sum(aporte_coescop, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct(., anio, tipo, .keep_all = TRUE ) %>%
  dplyr::select( anio, 
                 tipo,
                 aporte_coescop )


reserva_matematica_ins <- reserva_matematica %>%
  group_by( anio, tipo ) %>%
  mutate( reserva_matematica = sum( reserva_matematica, na.rm = TRUE ),
          montepio = sum( montepio, na.rm = TRUE ),
          gastos_administrativos =  sum( gastos_administrativos, na.rm = TRUE ),
          egreso_total = sum( reserva_total, na.rm = TRUE ) ) %>%
  mutate( n_old = sum( i_p_acu, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, tipo, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 tipo,
                 n_old,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total )


aux <- aportes_ins %>%
  left_join( ., reserva_matematica_ins, by = c('anio', 'tipo') )

# 3. Función Balance anual por institución----------------------------------------------------------

fun_balance_anual <- function( .data, sector ) {
  
  a <- data.frame( anio = c( 2025:2049 ) ) %>% 
    left_join( ., aux, by = 'anio' ) %>% 
    filter( tipo == {{sector}} ) %>% 
    #filter( tipo == 'aduanero' ) %>% 
    mutate_if( is.numeric , replace_na, replace = 0 ) %>%
    mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
    mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
    mutate( V_cor = ingresos_total - egreso_total ) %>%
    dplyr::select( anio,
                   n_old,
                   aporte_coescop,
                   reserva_a_pagar,
                   ingresos_total,
                   reserva_matematica,
                   montepio,
                   gastos_administrativos,
                   egreso_total,
                   V_cor ) %>%
    mutate( r = 1 + i_a ) %>%
    mutate( r = if_else( anio == '2025',
                         1, 
                         r ) ) %>%
    mutate( r = cumprod( r ) ) %>%
    mutate( v = 1 / r ) %>%
    mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
            reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
            ingresos_total_vap = cumsum( v * ingresos_total  ),
            reserva_matematica_vap = cumsum( v * reserva_matematica   ),
            montepio_vap = cumsum( v * montepio   ),
            gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
            egreso_total_vap = cumsum( v * egreso_total  ),
            V_cap = r * cumsum( v * V_cor ),
            V = v *  V_cap )

    return( a )
}

b_cte <- fun_balance_anual(aux, 'cte') %>% filter( anio == 2049 ) %>% dplyr::select( reserva_a_pagar_vap  )

valor_a_pagar_ins <- data.frame( tipo = c( 'Comisión de Tránsito del Ecuador' ),
                                 reserva_a_pagar_vap = rbind( b_cte ) ) %>%
  rbind( ., c( "Total", as.character(sum(.[,2],  na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2 ), as.numeric )

balance_cte <- fun_balance_anual(aux, 'cte') %>% filter( anio == 2049 ) %>% dplyr::select( reserva_a_pagar_vap  )


# Balance por institución---------------------------------------------------------------------------

balance_anual_ins <- expand.grid( anio = c( 2025:2049 ),
                                 tipo = c( 'cte' ) ) %>%
  left_join( ., aportes_ins, by = c( 'anio', 'tipo' ) ) %>%
  left_join( ., jubilados_ins, by = c( 'anio', 'tipo' ) ) %>%
  left_join( ., reserva_tipo_anio , by = c( 'anio', 'tipo' ) ) %>%
  group_by( anio, tipo ) %>%
  replace( is.na( . ), 0 ) %>% 
  mutate( reserva_a_pagar = egreso_total - aporte_coescop ) %>%
  mutate( ingresos_total =  aporte_coescop + reserva_a_pagar ) %>%
  mutate( V_cor = ingresos_total - egreso_total ) %>%
  ungroup( ) %>% 
  dplyr::select( anio,
                 tipo,
                 n_old,
                 aporte_coescop,
                 reserva_a_pagar,
                 ingresos_total,
                 reserva_matematica,
                 montepio,
                 gastos_administrativos,
                 egreso_total,
                 V_cor ) %>%
  arrange( tipo, anio ) %>% 
  group_by( tipo ) %>% 
  mutate( r = 1 + i_a ) %>%
  mutate( r = if_else( anio == '2025',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( aporte_coescop_vap = cumsum( v * aporte_coescop  ),
          reserva_a_pagar_vap = cumsum( v * reserva_a_pagar  ),
          ingresos_total_vap = cumsum( v * ingresos_total  ),
          reserva_matematica_vap = cumsum( v * reserva_matematica   ),
          montepio_vap = cumsum( v * montepio   ),
          gastos_administrativos_vap = cumsum( v * gastos_administrativos   ),
          egreso_total_vap = cumsum( v * egreso_total  ),
          V_cap = r * cumsum( v * V_cor ),
          V = v *  V_cap ) %>% 
  ungroup( )

#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando balance' )

save( impacto,
      balance_anual,
      valor_a_pagar_ins,
      balance_anual_ins,
      file = paste0( parametros$RData, 'IESS_balance.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()