message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los población COESCOP' )

#Cargando población de beneficiarios----------------------------------------------------------------
load( paste0( parametros$RData, 'CTE_servidores.RData' ) )
load( paste0( parametros$RData, 'IESS_avisos_cte.RData' ) )

#Tablas de contingencia por edad y sexo-------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tResumiendo información en tablas de contingencia' )

#CTE------------------------------------------------------------------------------------------------

#Tabla por edad y sexo------------------------------------------------------------------------------
tabla_cte_edad_sexo <- cte_servidores %>%
  filter( !is.na( fecha_nacimiento ) ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date("31/12/2024","%d/%m/%Y"),
                                  units = "years",
                                  precise = TRUE ) ) ) %>%
  group_by( edad, sexo ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select( sexo,
                 edad,
                 freq ) %>%
  arrange( sexo, edad ) %>%
  filter( edad > 17, edad < 75 ) %>% 
  mutate( fdp = freq / sum( freq ) )

#Tabla por rangos de edad --------------------------------------------------------------------------

cortes_edad <- c( 19, seq( 30, 70, 10 ) )

etiquetas_edad <- c( paste0( 
  "[ ",
  formatC( 
    c( seq( 20, 60, 10 ) ),
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  ),
  " - ",
  formatC( 
    c( seq( 30, 70, 10 ) ),
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  ),
  " ]"
) )

tabla_rangos_edad_cargos <- tabla_cte_edad_sexo %>% 
  mutate( rango_edad = cut( 
    edad,
    breaks = cortes_edad,
    labels = etiquetas_edad,
    #include.lowest = TRUE,
    right = TRUE
  ) ) %>%
  group_by( sexo, rango_edad ) %>%
  mutate( beneficiarios = sum( freq, na.rm = TRUE ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_edad, .keep_all = TRUE ) %>%
  mutate( N = sum( beneficiarios, na.rm = TRUE ) ) %>%
  mutate( dist = beneficiarios / N ) %>%
  dplyr::select( sexo, beneficiarios, rango_edad ) %>%
  arrange( rango_edad, sexo ) %>% 
  spread( ., sexo, value = c( beneficiarios ), sep = "_" ) %>%
  mutate_if( is.numeric , replace_na, replace = 0 ) %>%
  mutate( total = rowSums( .[2:ncol( . )] ) ) %>%
  mutate( rango_edad = as.character( rango_edad ) ) %>%
  rbind( ., c( "Total", as.character( colSums( .[,2:ncol( . )], na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2:ncol( . ) ), as.numeric ) %>%
  mutate( por_sexo_F = 100 * 2 * sexo_F / sum( total ),
          por_sexo_M = 100* 2 * sexo_M / sum( total ),
          por_total = 100 * 2 * total / sum( total ) ) %>%
  dplyr::select( rango_edad,
                 sexo_F,
                 por_sexo_F,
                 sexo_M,
                 por_sexo_M,
                 total,
                 por_total ) 

#Tabla cargos por sexo------------------------------------------------------------------------------
tabla_cte_cargo <- cte_servidores %>%
  filter( !is.na( fecha_nacimiento ) ) %>%
  filter( fecha_nacimiento < as.Date("31/12/2024","%d/%m/%Y") ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "31/12/2024", "%d/%m/%Y" ),
                                  units = "years",
                                  precise = TRUE ) ) ) %>%
  filter( edad > 17, edad < 75 )%>%
  group_by( sexo, cargo_coescop ) %>%
  mutate( freq = n( ) ) %>%
  ungroup( ) %>%
  distinct( sexo, cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select( cargo_coescop, 
                 sexo,
                 freq ) %>% 
  arrange( cargo_coescop ) %>% 
  spread( ., sexo, value = c( freq ), sep = "_" ) %>%
  mutate_if( is.numeric , replace_na, replace = 0 ) %>%
  mutate( total = rowSums( .[2:ncol( . )] ) ) %>%
  mutate( cargo_coescop = as.character( cargo_coescop ) ) %>%
  rbind( ., c( "Total", as.character( colSums( .[,2:ncol( . )], na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2:ncol( . ) ), as.numeric ) %>%
  mutate( por_sexo_F = 100 * 2 * sexo_F / sum( total ),
          por_sexo_M = 100* 2 * sexo_M / sum( total ),
          por_total = 100 * 2 * total / sum( total ) ) %>%
  dplyr::select( cargo_coescop,
                 sexo_F,
                 por_sexo_F,
                 sexo_M,
                 por_sexo_M,
                 total,
                 por_total ) 

#Salario promedio por cargo y sexo------------------------------------------------------------------

tabla_cte_salario <- cte_servidores %>%
  mutate( sueldo = as.numeric( sueldo ) ) %>%
  filter( !is.na( sueldo ), sueldo > 0 ) %>%
  group_by( cargo_coescop ) %>%
  mutate( media = mean( sueldo, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( cargo_coescop, .keep_all = TRUE ) %>%
  dplyr::select( cargo_coescop, media ) %>% 
  arrange( cargo_coescop )


#Funcionarios por imposiciones por sexo-------------------------------------------------------------

cortes_imp <- c( 1, 12, 24, 48, 72, 120, 180, 240, 300, 360, 530 )

etiquetas_imp <- c( paste0( 
  "( ",
  formatC( 
    c( 1, 12, 24, 48, 72, 120, 180, 240, 300, 360 ),
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  ),
  " - ",
  formatC( 
    c(  12, 24, 48, 72, 120, 180, 240, 300, 360, 530 ),
    digits = 0,
    format = 'f',
    big.mark = '.',
    decimal.mark = ','
  ),
  " ]"
) )

tabla_imp_sexo <- cte_servidores %>% 
  mutate( rango_imp = cut( 
    imp,
    breaks = cortes_imp,
    labels = etiquetas_imp,
    #include.lowest = TRUE,
    right = TRUE
  ) ) %>%
  group_by( sexo, rango_imp ) %>%
  mutate( n = n( ) ) %>%
  ungroup(  ) %>%
  distinct( sexo, rango_imp, .keep_all = TRUE ) %>%
  dplyr::select( sexo, n, rango_imp ) %>%
  arrange( rango_imp, sexo ) %>% 
  spread( ., sexo, value = c( n ), sep = "_" ) %>%
  mutate_if( is.numeric , replace_na, replace = 0 ) %>%
  mutate( total = rowSums( .[2:ncol( . )] ) ) %>%
  mutate( rango_imp = as.character( rango_imp ) ) %>%
  rbind( ., c( "Total", as.character( colSums( .[,2:ncol( . )], na.rm =TRUE ) ) ) ) %>%
  mutate_at( c( 2:ncol( . ) ), as.numeric ) %>%
  mutate( por_sexo_F = 100 * 2 * sexo_F / sum( total ),
          por_sexo_M = 100* 2 * sexo_M / sum( total ),
          por_total = 100 * 2 * total / sum( total ) ) %>%
  dplyr::select( rango_imp,
                 sexo_F,
                 por_sexo_F,
                 sexo_M,
                 por_sexo_M,
                 total,
                 por_total ) 

#Tabla de entradas y salidas------------------------------------------------------------------------

avisos_cte <- avisos_cte %>% 
  mutate( sexo = if_else( sexo_afiliado %in% c( 'Hombre' ),
                          'M',
                          'F' ) )

tabla_avisos_ent <- avisos_cte %>% 
  filter( codtipnovhislab == 'ENT' ) %>% 
  group_by( anio ) %>% 
  summarise( entradas = n( ) ) %>% 
  ungroup( ) %>% 
  mutate( entradas = if_else( anio == '2024',
                              round( 12 * entradas / 9, 0 ),
                              entradas ) )


tabla_avisos_sal <- avisos_cte %>% 
  filter( codtipnovhislab == 'SAL' ) %>% 
  group_by( anio ) %>% 
  summarise( salidas = n( ) ) %>% 
  ungroup( ) %>% 
  mutate( salidas = if_else( anio == '2024',
                              round( 12 * salidas / 9, 0 ),
                             salidas ) )

tabla_avisos <- tabla_avisos_ent %>% 
  left_join( ., tabla_avisos_sal, by = 'anio' )

#Guardando en un Rdata todas las tablas-------------------------------------------------------------
message( '\tGuardando en data.frame' )

tablas <- c('tabla_cte_cargo', 
            'tabla_cte_salario')
            

save(list = tablas , file = paste0( parametros$RData, 'IESS_tablas_contingencia.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

