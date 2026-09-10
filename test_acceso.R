library(googledrive)
source("Mentalytica_AgTech/utils.R")
catalogo <- cargar_catalogo_clientes()
print(catalogo)

# 1. Autenticarte en tu cuenta de Google (si estás en local, abrirá el navegador)
drive_auth()

# 2. Probar si lee el catálogo corporativo
cat("Buscando catálogo de clientes en Drive...\n")
catalogo <- cargar_catalogo_clientes()
print(catalogo)

# 3. Probar la simulación de inicio de sesión con 'bellaflor'
cat("\nProbando validación de credenciales para 'bellaflor'...\n")
resultado <- validar_login("admin", "12345", "bellaflor")
print(resultado)

if(resultado$valido) {
  cat("\n¡Acceso exitoso! Descargando 'Siembras_AgTech.xlsx' de la florícola...\n")
  lotes <- inicializar_sesion_floricola(resultado$empresa)
  print(head(lotes))
}