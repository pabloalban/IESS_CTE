message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los avisos de salida' )

avisos_folder <- "W:/Data/DES/IESS-DNAC-2024-2073-M/"

# Lectura de avisos de salida y entrada-------------------------------------------------------------
file_1 <- paste0( avisos_folder, 'Base_1.dat' )
file_2 <- paste0( avisos_folder, 'Base_2.dat' )
file_3 <- paste0( avisos_folder, 'Base_3.dat' )

classes <- c( 'integer',
              'integer',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'numeric',
              'numeric',
              'character',
              'character',
              'character',
              'character' )

aux_1 <- read.table( file_1,
                     dec = ",", 
                     header = TRUE, 
                     sep = "\t", 
                     na.strings = "NA",
                     #nrows = 10000,
                     colClasses = classes,
                     stringsAsFactors = FALSE ) %>% 
  clean_names( )

aux_2 <- read.table( file_2,
                     dec = ",", 
                     header = TRUE, 
                     sep = "\t", 
                     na.strings = "NA",
                     #nrows = 10000,
                     colClasses = classes,
                     stringsAsFactors = FALSE ) %>% 
  clean_names( )

aux_3 <- read.table( file_3,
                     dec = ",", 
                     header = TRUE, 
                     sep = "\t", 
                     na.strings = "NA",
                     #nrows = 10000,
                     colClasses = classes,
                     stringsAsFactors = FALSE ) %>% 
  clean_names( )

avisos_cte <- rbind( aux_1,
                     aux_2,
                     aux_3 ) %>% 
  filter( desc_sector %in% c( 'PRIVADA', 'PUBLICA', 'VOLUNTA' ) ) %>% 
  mutate( fecnacper = as.Date( fecnacper, "%d/%m/%Y" ),
          fch_reg_novedad = as.Date( fch_reg_novedad, "%d/%m/%Y" ),
          fch_inicio_novedad = as.Date( fch_inicio_novedad, "%d/%m/%Y" ) ) %>%
  filter( ruc == '0968589570001' ) %>% 
  dplyr::select( anio,
                 mes,
                 cedula := cedula_afi,
                 codtipnovhislab,
                 fecnacper,
                 sexo_afiliado,
                 relacion_trabajo_nov,
                 poraponor,
                 porapoadi,
                 valor_sueldo,
                 val_apo,
                 fch_reg_novedad,
                 fch_inicio_novedad,
                 causal,
                 reltrasal
                 )


avisos_cte <- avisos_cte %>%
  arrange( cedula, fch_inicio_novedad ) %>% 
  lazy_dt( ) %>% 
  mutate( causal = if_else( nchar( causal ) < 2,
                            'No se especifica',
                            causal ) ) %>% 
  mutate( causal = if_else( causal == 'Desaparición del puesto dentro de la estructura de la empre',
                            'Desaparición del puesto',
                            causal ) ) %>% 
  mutate( causal = if_else( causal == 'Despido unilateral por parte del empleador',
                            'Despido unilateral',
                            causal ) ) %>% 
  mutate( causal = if_else( causal == 'Otras causas justificadas por empleador',
                            'Otras causas',
                            causal ) ) %>% 
  group_by( cedula ) %>% 
  mutate( dias_cesante = if_else( codtipnovhislab == 'SAL' & lead( codtipnovhislab )  == 'ENT',
                                  as.integer( lead( fch_inicio_novedad ) - fch_inicio_novedad ),
                                  NA ) ) %>% 
  ungroup( ) %>% 
  as_tibble( )

# Guardar los data.frames en un Rdata---------------------------------------------------------------
message( '\tGuardando avisos de salida y entrada' )

save( avisos_cte,
      file = paste0( parametros$RData, 'IESS_avisos_cte.RData' ) )

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc( )
