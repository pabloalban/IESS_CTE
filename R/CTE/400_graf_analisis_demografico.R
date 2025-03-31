
message(  paste(  rep( '-', 100  ), collapse = ''  )  )

message(  '\tGraficando demografía COESCOP'  )

# Plantilla gráfica --------------------------------------------------------------------------------
source(  'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE  )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_tablas_contingencia_cte.RData' ) )
load( paste0( parametros$RData, 'IESS_avisos_cte.RData' ) )

#Gráfico pirámide poblacional CTE------------------------------------------------------------------
message(  '\tGraficando pirámide poblacional CTE'  )

aux <- tabla_cte_edad_sexo %>% 
       dplyr::select(  sexo, edad, n:= fdp ) %>%
       mutate( n = ifelse(  sexo == 'M',
                            -n,
                            n ) )

max_edad <- 100
min_edad <- 15

salto_y <- 10
salto_x <- 0.02
brks_y <- seq( -0.1,0.1,salto_x )
lbls_y <- paste0( as.character( c( seq( 0.1, 0, -salto_x )*100, seq( salto_x, 0.1, salto_x ) * 100 ) ), "%" )
brks_x <- seq( 15,100,salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_cte <- ggplot( aux, aes( x = edad, y = n, fill = sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar(  data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1 ) +
  geom_bar(  data = aux %>% filter(  sexo == 'M'  ), stat = 'identity',colour="white", size=0.1 ) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip(  ) +
  theme_bw(  ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,label.position = "right",
                               label.hjust = 0, label.vjust = 0.5, reverse = TRUE ) ) +
  theme( legend.position="bottom" ) +
  scale_fill_manual( values = c( parametros$iess_blue,
                                 parametros$iess_green ),
                     labels = c( "Mujeres", 
                                 "Hombres" ) )

ggsave(  plot = iess_pir_cte, 
        filename = paste0(  parametros$resultado_graficos, 'iess_pir_cte', parametros$graf_ext  ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi  )

#Gráfico de barras cargo CTE------------------------------------------------------------------
message(  '\tGráfico de barras por cargo CTE'  )

aux <- tabla_cte_cargo

label <- format( aux$n, nsmall = 0, digits = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

iess_bar_cte <- ggplot( aux, aes( x = cargo_coescop, y = n ) ) + 
  geom_bar( stat = "identity", fill = parametros$iess_green ) +
  theme_bw( ) +
  plt_theme +
  geom_text( aes( label = label ), 
             vjust = -0.2, 
             color = "black", 
             hjust = 0.5,
             position = position_dodge( 0.9 ),
             angle = 0,
             family = 'Linux Libertine',
             size = 4 ) + 
  labs( x = "", y = "Número de Servidores" )+
  theme( axis.text.x = element_text( angle = 70, vjust = 1, hjust = 1 ) )

ggsave( plot = iess_bar_cte, 
        filename = paste0( parametros$resultado_graficos, 'iess_bar_cte', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Limpiar Memoria RAM--------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !(  ls( ) %in% c( 'parametros' ) ) ] )
gc( )