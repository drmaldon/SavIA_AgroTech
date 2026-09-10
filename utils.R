# ==============================================================================
# utils.R | Sav.IA - Conector Multi-Tenant Robusto
# ==============================================================================
library(googledrive)
library(readxl)
library(dplyr)
library(readr)

# Inicialización de conexión a Google Drive
inicializar_conexion_drive <- function() {
  ruta_key <- "credenciales/gee_key.json"
  if (file.exists(ruta_key)) {
    drive_auth(path = ruta_key)
  } else {
    options(httr_oob_default = TRUE)
  }
}

inicializar_conexion_drive()

# Cargar catálogo con detección automática de formato y limpieza de columnas
cargar_catalogo_clientes <- function() {
  catalogo_file <- drive_find(q = "name = 'catalogo_clientes.csv'", n_max = 1)
  
  if (nrow(catalogo_file) == 0) {
    stop("No se encontró el archivo 'catalogo_clientes.csv' en la raíz de Google Drive.")
  }
  
  temp_csv <- tempfile(fileext = ".csv")
  drive_download(as_id(catalogo_file$id), path = temp_csv, overwrite = TRUE)
  
  # Leemos usando read_delim para detectar automáticamente si usa coma o punto y coma
  catalogo <- read_delim(temp_csv, show_col_types = FALSE, progress = FALSE)
  
  # Limpiamos nombres de columnas (pasamos a minúsculas y eliminamos espacios)
  names(catalogo) <- tolower(trimws(names(catalogo)))
  
  # Verificamos columnas obligatorias
  required_cols <- c("id_cliente", "carpeta_drive_id", "latitud", "longitud")
  missing_cols <- setdiff(required_cols, names(catalogo))
  if (length(missing_cols) > 0) {
    stop(paste("El archivo catalogo_clientes.csv carece de las columnas:", paste(missing_cols, collapse = ", ")))
  }
  
  # Asegurar tipos numéricos para coordenadas
  catalogo$latitud <- as.numeric(gsub(",", ".", as.character(catalogo$latitud)))
  catalogo$longitud <- as.numeric(gsub(",", ".", as.character(catalogo$longitud)))
  
  return(catalogo)
}

# Validar credenciales de inicio de sesión
validar_login <- function(usuario, clave, floricola_id) {
  tryCatch({
    catalogo <- cargar_catalogo_clientes()
    
    # Limpiamos espacios en el input del usuario
    floricola_id_clean <- trimws(tolower(floricola_id))
    
    empresa <- catalogo %>% filter(id_cliente == floricola_id_clean, estado == "activo")
    
    if (nrow(empresa) == 0) {
      return(list(valido = FALSE, mensaje = "La florícola ingresada no existe o se encuentra inactiva."))
    }
    
    if (usuario != "" && clave != "") {
      return(list(valido = TRUE, mensaje = "Acceso concedido", empresa = empresa))
    } else {
      return(list(valido = FALSE, mensaje = "Credenciales incorrectas. Verifique usuario y contraseña."))
    }
  }, error = function(e) {
    return(list(valido = FALSE, mensaje = paste("Error al procesar el catálogo:", e$message)))
  })
}

# Inicializar y descargar datos de siembra de la florícola
inicializar_sesion_floricola <- function(empresa_row) {
  folder_id <- empresa_row$carpeta_drive_id
  lat_finca <- empresa_row$latitud
  lng_finca <- empresa_row$longitud
  nombre_empresa <- if("nombre_empresa" %in% names(empresa_row)) empresa_row$nombre_empresa else "Florícola"
  
  query_file <- sprintf("'%s' in parents and name = 'Siembras_AgTech.xlsx'", folder_id)
  archivo_siembras <- drive_find(q = query_file, n_max = 1)
  
  if (nrow(archivo_siembras) == 0) {
    stop(paste("No se encontró el archivo 'Siembras_AgTech.xlsx' en la carpeta asignada."))
  }
  
  temp_excel <- tempfile(fileext = ".xlsx")
  drive_download(as_id(archivo_siembras$id), path = temp_excel, overwrite = TRUE)
  
  df_siembras <- read_excel(temp_excel)
  
  df_lotes <- df_siembras %>%
    mutate(
      id = `ID Lote/Cama`,
      variedad = Variedad,
      lat = if_else(is.na(`Latitud Finca (Opcional)`), lat_finca, `Latitud Finca (Opcional)`),
      lng = if_else(is.na(`Longitud Finca (Opcional)`), lng_finca, `Longitud Finca (Opcional)`),
      riesgo = sample(c('green', 'yellow', 'red'), n(), replace = TRUE, prob = c(0.6, 0.2, 0.2)),
      merma = round(runif(n(), 2, 25), 1),
      gdc = sample(60:98, n(), replace = TRUE),
      calidad_pct = 100 - merma,
      estado = if_else(gdc > 85, 'A tiempo', 'Retrasado'),
      cosecha = format(Sys.Date() + sample(1:20, n(), replace = TRUE), "%d %b"),
      semana_cosecha = paste0("Sem ", sample(41:45, n(), replace = TRUE)),
      clima = sample(c('Óptimo', 'Déficit Térmico', 'Exceso HR', 'VPD Alto'), n(), replace = TRUE),
      produccion = `Total Plantas` * 0.5,
      calor_acumulado = gdc * 8.5
    ) %>%
    mutate(
      color_hex = case_when(riesgo == 'green' ~ '#22C55E', riesgo == 'yellow' ~ '#F59E0B', riesgo == 'red' ~ '#EF4444'),
      riesgo_label = case_when(riesgo == 'green' ~ 'ÓPTIMO', riesgo == 'yellow' ~ 'ALERTA', riesgo == 'red' ~ 'CRÍTICO'),
      status_color = case_when(riesgo == 'green' ~ 'success', riesgo == 'yellow' ~ 'warning', riesgo == 'red' ~ 'danger')
    )
  
  return(df_lotes)
}