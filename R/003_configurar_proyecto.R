# Parámetros globales R ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tConfiguración global de R' )

options( scipen = 99 )
setNumericRounding( 2 )
options( stringsAsFactors = FALSE )

# Parámetros ---------------------------------------------------------------------------------------
message( '\tCreando entorno de parámetros' )

# Entorno con parámetros
parametros <- new.env()

# User name
parametros$user <- Sys.getenv( 'USER' )

parametros$fec_eje <- Sys.Date()

# Operating system name
parametros$opsys <- Sys.info()[[1]]

# Condicional para importar el tipo de letra Linux Libertine en diferentes sistemas operativos
if ( 'Linux Libertine' %notin% font_families() ) {
  if ( parametros$opsys == 'Linux' ) {
    file <- "/usr/share/fonts/opentype/linux-libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
  } else if ( parametros$opsys == 'Windows' ) {
    file <- "C:/Program Files/MiKTeX/fonts/opentype/public/libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
    file <- "C:/Program Files (x86)/MiKTeX/fonts/opentype/public/libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
    file <- "C:/MiKTeX/fonts/opentype/public/libertine/LinLibertine_DR.otf"
    if ( file.exists( file ) ) {
      font_add( "Linux Libertine", file )
    }
  }
}
showtext_auto()

# Hostname
parametros$hostname <- Sys.info()[[4]]

#Servidor de datos
parametros$data_server <- 'Y:/IESS_CTE/'
#parametros$data_server <- 'D:/OneDrive - Instituto Ecuatoriano de Seguridad Social/IESS_CTE/'

# local
#parametros$data_server <- paste0( getwd(), '/' )


# Directorio de trabajo
parametros$work_dir <- paste0( getwd(), '/' )

# Setting Time Zone
parametros$time_zone <- "America/Guayaquil"

# Colores IESS
parametros$iess_blue <- rgb( 0, 63, 138, maxColorValue = 255 )
parametros$iess_green <- rgb( 0, 116, 53, maxColorValue = 255 )
parametros$iess_total <- rgb( 138, 5, 81, maxColorValue = 255 )
parametros$female <- rgb( 255, 105, 180, maxColorValue = 255 )
parametros$male <- rgb( 51, 153, 255, maxColorValue = 255 )
# Calcular balance
# parametros$calcular_balance <- FALSE

parametros$mont_prop_afi <- 0.1275

# Direcciones globables  ---------------------------------------------------------------------------
message( '\tEstableciendo directorios globales' )
parametros$empresa <- 'IESS'

message( '\tConfiguración seguro' )
parametros$seguro <- c("CTE")
parametros$hacer_ana_dem <- FALSE
parametros$calcular_balance <- FALSE
parametros$fecha <- Sys.Date( )
parametros$fecha_anio <- year( Sys.Date( ) )
parametros$fecha_mes <- format( parametros$fecha, "%B" )
parametros$fecha_dia <- day( Sys.Date( ) )

# Configuraciones particulares por seguro ----------------------------------------------------------
parametros$fec_fin <- ymd( '2024-12-31' )
parametros$anio_ini <- 2025
parametros$anio <- 2025 # Año del estudio
parametros$edad_max <- 105

# Incluir casos según corresponda
parametros$horizonte <- 25 # en años
parametros$anio_fin <- 2049  # Año fin de la proyección
parametros$edad_max <- 105

# Variables automáticas ----------------------------------------------------------------------------
parametros$Data <- paste0( parametros$data_server, 'Data/' )
parametros$RData <- paste0( parametros$data_server, 'RData/' )
parametros$SQL <- paste0( parametros$data_server, 'SQL/' )
parametros$RSQL <- paste0( parametros$data_server, 'RSQL/' )
parametros$Data_seg <- paste0( parametros$Data, parametros$seguro, '/' )
parametros$RData_seg <- paste0( parametros$RData, parametros$seguro, '/' )
parametros$SQL_seg <- paste0( parametros$SQL, parametros$seguro, '/' )
parametros$RSQL_seg <- paste0( parametros$RSQL, parametros$seguro, '/' )
parametros$Driver <- paste0( parametros$data_server, 'Drivers/oracle', '/' )

parametros$reportes <- paste0( parametros$work_dir, 'Reportes/' )
parametros$resultados <- paste0( parametros$work_dir, 'Resultados/' )
parametros$reporte_seguro <- paste0( parametros$work_dir, 'Reportes/Reporte_', 
                                     parametros$seguro, '/' )
parametros$RData_macro <- paste0( parametros$RData, 'macro/' )
parametros$Data_macro <- paste0( parametros$Data, 'macro/' )

parametros$calculo_balance <- paste0( parametros$work_dir, 'R/CTE/305_calculo_escenarios_balance.R' )
parametros$reporte_genera <- paste0( parametros$work_dir, 'R/CTE/600_reporte_latex.R' )


parametros$reporte_script <- paste0( parametros$reporte_seguro, 'reporte.R' )
parametros$reporte_nombre <- paste0( parametros$empresa, '_', 
                                     parametros$seguro, '_estudio_actuarial' )
parametros$reporte_latex <- paste0( parametros$reporte_nombre, '.tex' )
parametros$resultado_seguro <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/' )
parametros$resultado_tablas <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/tablas/' )
parametros$resultado_graficos <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                         format( parametros$fec_eje, '%Y_%m_%d' ), '/graficos/' )

parametros$graf_modelo_1 <- 'R/401_graf_plantilla.R'
parametros$graf_ext <- '.pdf'

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

