message( paste( rep('-', 100 ), collapse = '' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )


#Cargando base del RC-------------------------------------------------------------------------------

load( paste0( parametros$RData, "IESS_Reg_Civil.RData" ) )
rc <- rc %>% 
  dplyr::select( cedula, nombre, sexo, fecha_nacimiento ) %>% 
  mutate( a_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 1],
          a_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 2],
          n_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 3] )

#Carga de avisos de salida y entrada----------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_avisos_cte.RData' ) )

avisos_cedulas <- avisos_cte %>% 
  dplyr::select( cedula ) %>% 
  distinct( cedula, .keep_all = TRUE )

avisos_sueldos <- avisos_cte %>% 
  filter( codtipnovhislab == 'ENT' ) %>% 
  group_by( cedula ) %>%
  filter( anio == max( anio, na.rm=TRUE) ) %>% 
  ungroup( ) %>% 
  group_by( cedula ) %>%
  filter( mes == max( mes, na.rm=TRUE) ) %>% 
  ungroup( ) %>% 
  dplyr::select( cedula, sueldo := valor_sueldo ) %>%  
  distinct( cedula, .keep_all = TRUE )

# Lectura de imposiciones a diciembre de 2023-------------------------------------------------------
message( '\tLectura de imposiciones a diciembre de 2023' )
file_2023 <- paste0( parametros$Data, 'CTE_31_12_2023.xlsx' )

cte_2023 <- read_excel( file_2023,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names( ) %>% 
  filter( tipo_planilla == 'A' ) %>% 
  dplyr::select( cedula,
                 sueldo_2023 := sueldo,
                 impo_2023 ) 

cte_nombres <- cte_2023 %>% 
  dplyr::select( cedula ) %>% 
  rbind( ., avisos_cedulas ) %>% 
  left_join( ., rc, by = c( 'cedula' ) ) %>% 
  distinct( cedula, .keep_all = TRUE )

#Lectura de transparencia a agosto de 2024----------------------------------------------------------
file_cargos <- paste0( parametros$Data, '2.1-2.2 Directorio-y-distributivo-personal-de-la-entidad TRANSPARENCIA ACTIVA AGOSTO CONJUNTO DE DATOS.csv' )
file_sueldos <- paste0( parametros$Data, '3 Remuneraciones-ingresos-adicionales TRANSPARENCIA ACTIVA AGOSTO CONJUNTO DE DATOS.csv' )

trans_cargos <- read.table( file_cargos,
                            dec = ",", 
                            header = TRUE, 
                            sep = ";", 
                            na.strings = "NA",
                            #nrows = 10000,
                            #colClasses = classes,
                            fill = TRUE,
                            stringsAsFactors = FALSE ) %>% 
  clean_names( ) %>% 
  rename( nombre = apellidos_y_nombres ) %>% 
  dplyr::select( -correo_electronico_institucional,
                 -telefono_institucional,
                 -extension_telefonica,
                 -direccion_institucional ) %>% 
  mutate( nombre = str_replace( nombre, "  ", " ") ) %>% 
  mutate( nombre = str_squish( nombre ) ) %>% 
  tildes_a_latex( . )
  

aux_1 <- trans_cargos %>%   
  mutate( a_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 1],
          a_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 2],
          n_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 3],
          n_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 4] ) %>% 
  left_join( ., cte_nombres, by = c( 'nombre', 'a_1', 'a_2', 'n_1' ) ) %>%  
  distinct( no, .keep_all = TRUE ) %>% 
  filter( !is.na( sexo ) )

aux_2 <- anti_join( trans_cargos, aux_1, by = c( 'nombre' = 'nombre' ) ) %>%
  mutate( a_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 1],
          a_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 2],
          n_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 3],
          n_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 4] ) %>% 
  left_join( ., cte_nombres, by = c( 'a_1', 'a_2', 'n_1' ) ) %>%  
  distinct( no, .keep_all = TRUE ) %>% 
  filter( !is.na( sexo ) ) %>% 
  mutate( nombre.x = nombre.y ) %>% 
  rename( nombre = nombre.x ) %>% 
  dplyr::select( -nombre.y ) 

aux_3 <- anti_join( trans_cargos, rbind( aux_1, aux_2 ), by = c( 'no' ) ) %>%
  mutate( a_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 1],
          a_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 2],
          n_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 3],
          n_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 4] ) %>% 
  left_join( ., cte_nombres, by = c( 'a_1', 'a_2' ) ) %>%  
  distinct( no, .keep_all = TRUE ) %>% 
  filter( !is.na( sexo ) ) %>% 
  rename( n_1 = n_1.x ) %>% 
  dplyr::select( -n_1.y ) %>% 
  mutate( nombre.x = nombre.y ) %>% 
  rename( nombre = nombre.x ) %>% 
  dplyr::select( -nombre.y ) 

aux_4 <- anti_join( trans_cargos, rbind( aux_1, aux_2, aux_3 ), by = c( 'no' ) ) %>%
  mutate( a_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 1],
          a_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 2],
          n_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 3],
          n_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 4] ) %>% 
  left_join( ., cte_nombres, by = c( 'a_1', 'n_1' ) ) %>%  
  distinct( no, .keep_all = TRUE ) %>% 
  filter( !is.na( sexo ) ) %>% 
  rename( a_2 = a_2.x ) %>% 
  dplyr::select( -a_2.y ) %>% 
  mutate( nombre.x = nombre.y ) %>% 
  rename( nombre = nombre.x ) %>% 
  dplyr::select( -nombre.y ) 

aux_5 <- anti_join( trans_cargos, rbind( aux_1, aux_2, aux_3, aux_4 ), by = c( 'no' ) ) %>%
  mutate( a_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 1],
          a_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 2],
          n_1 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 3],
          n_2 = str_split(string = nombre, 
                          pattern = "[[:space:]]", 
                          simplify = TRUE)[, 4] ) %>% 
  left_join( ., cte_nombres, by = c( 'a_2', 'n_1' ) ) %>%  
  distinct( no, .keep_all = TRUE ) %>% 
  filter( !is.na( sexo ) ) %>% 
  rename( a_1 = a_1.x ) %>% 
  dplyr::select( -a_1.y ) %>% 
  mutate( nombre.x = nombre.y ) %>% 
  rename( nombre = nombre.x ) %>% 
  dplyr::select( -nombre.y ) 

cte_servidores <- rbind( aux_1, aux_2, aux_3, aux_4, aux_5 ) %>% 
  distinct( cedula, .keep_all = TRUE ) %>% 
  left_join( ., cte_2023, by = 'cedula' ) %>% 
  mutate( cargo_coescop = if_else( unidad_a_la_que_pertenece == 'COMANDANCIA',
                                   puesto_institucional,
                                   'ADMINISTRATIVO') ) %>% 
  mutate( cargo_coescop = factor( cargo_coescop,
                                  levels = c( 'ADMINISTRATIVO',
                                              'AGENTE 4',
                                              'AGENTE 3',
                                              'AGENTE 2',
                                              'AGENTE 1',
                                              'SUB-INSPECTOR II',
                                              'SUB-INSPECTOR I',
                                              'INSPECTOR',
                                              'SUB-PREFECTO',
                                              'PREFECTO',
                                              'PREFECTO JEFE',
                                              'PREFECTO COMANDANTE' ) ) ) %>% 
  dplyr::select( -a_1,
                 -a_2,
                 -n_1,
                 -n_2,
                 -puesto_institucional )

#Sueldos según transparencia agosto de 2024---------------------------------------------------------
trans_sueldos_cargos <- read.table( file_sueldos,
                             dec = ",", 
                             header = TRUE, 
                             sep = ";", 
                             na.strings = "NA",
                             #nrows = 10000,
                             #colClasses = classes,
                             fill = TRUE,
                             stringsAsFactors = FALSE ) %>% 
  clean_names( ) %>% 
  dplyr::select( cargo := puesto_institucional,
                 regimen := regimen_laboral_al_que_pertenece,
                 grado := grado_jerarquico_o_escala_al_que_pertenece_el_puesto,
                 remuneracion := remuneracion_mensual_unificada ) %>% 
  slice( 1:nrow( cte_servidores ) ) %>% 
  mutate( cargo_coescop = if_else( regimen == '3.OTROS REGIMENES ESPECIALES',
                                   cargo,
                                   'ADMINISTRATIVO') ) %>% 
  mutate( cargo_coescop = factor( cargo_coescop,
                                  levels = c( 'ADMINISTRATIVO',
                                              'AGENTE 4',
                                              'AGENTE 3',
                                              'AGENTE 2',
                                              'AGENTE 1',
                                              'INSPECTOR',
                                              'SUB-INSPECTOR II',
                                              'SUB-INSPECTOR I',
                                              'SUB-PREFECTO',
                                              'PREFECTO',
                                              'PREFECTO JEFE',
                                              'PREFECTO COMANDANTE' ) ) ) %>% 
  dplyr::select( cargo_coescop,
                 grado,
                 remuneracion )

tab_sueldos_cargos <- trans_sueldos_cargos %>% 
  group_by( cargo_coescop ) %>%
  summarise(  sueldo = mean( remuneracion, na.rm = TRUE ) )

#Cruze con sueldos perdidos por cédula--------------------------------------------------------------

cte_servidores <- cte_servidores %>% 
  left_join( ., tab_sueldos_cargos, by = 'cargo_coescop' ) %>% 
  rename( ciudad = ciudad_en_la_que_labora ) %>% 
  mutate( imp = impo_2023 + 12 ) %>% 
  dplyr::select( -sueldo_2023,
                 -unidad_a_la_que_pertenece,
                 -impo_2023 )

# Guardar los data.frames en un Rdata---------------------------------------------------------------
message( '\tGuardando lista de servidores de la CTE' )

save( cte_servidores,
      tab_sueldos_cargos,
      file = paste0( parametros$RData, 'CTE_servidores.RData' ) )

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc( )
