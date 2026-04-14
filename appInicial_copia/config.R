
# =========================
# 0) Configuración
# =========================
# Carga la API key desde variable de entorno.
# Crea un archivo .Renviron en tu directorio home con:
#   GOOGLE_API_KEY=tu_api_key_aqui
# O usa Sys.setenv(GOOGLE_API_KEY = "tu_key") localmente (nunca en el código).

GOOGLE_API_KEY <- Sys.getenv("GOOGLE_API_KEY")
