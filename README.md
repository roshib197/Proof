# Quick financial analysis
# Función para calcular el ratio de liquidez
calcular_ratio_liquidez <- function(activos_corrientes, pasivos_corrientes) {
  ratio_liquidez <- activos_corrientes / pasivos_corrientes
  return(ratio_liquidez)
}

# Función para calcular el ratio de rentabilidad
calcular_ratio_rentabilidad <- function(utilidad_neta, ventas) {
  ratio_rentabilidad <- utilidad_neta / ventas
  return(ratio_rentabilidad)
}

# Función para generar un informe financiero
generar_informe_financiero <- function(datos_financieros) {
  # Calcular ratios financieros
  ratio_liquidez <- calcular_ratio_liquidez(datos_financieros$activos_corrientes, datos_financieros$pasivos_corrientes)
  ratio_rentabilidad <- calcular_ratio_rentabilidad(datos_financieros$utilidad_neta, datos_financieros$ventas)
  
  # Generar informe financiero
  informe_financiero <- data.frame(
    ratio_liquidez = ratio_liquidez,
    ratio_rentabilidad = ratio_rentabilidad
  )
  
  return(informe_financiero)
}

# Función para realizar proyecciones
realizar_proyecciones <- function(datos_financieros, tasa_crecimiento) {
  # Calcular proyecciones
  proyeccion_ventas <- datos_financieros$ventas * (1 + tasa_crecimiento)
  proyeccion_utilidad_neta <- datos_financieros$utilidad_neta * (1 + tasa_crecimiento)
  
  # Generar informe de proyecciones
  informe_proyecciones <- data.frame(
    proyeccion_ventas = proyeccion_ventas,
    proyeccion_utilidad_neta = proyeccion_utilidad_neta
  )
  
  return(informe_proyecciones)
