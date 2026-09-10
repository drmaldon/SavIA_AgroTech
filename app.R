# ==============================================================================
# app.R | Sav.IA - Predictive SaaS (Versión Final con Píldoras / Badges Minimalistas)
# ==============================================================================
library(shiny)
library(bs4Dash)
library(dplyr)
library(echarts4r)
library(leaflet)
library(purrr)
library(lubridate)
library(r2d3)

source("utils.R")

# Datos estáticos base para curvas evolutivas de calidad
dataEvolutivaCalidad <- data.frame(
  semana = c('Sem 41', 'Sem 42', 'Sem 43', 'Sem 44', 'Sem 45'),
  Calidad_Alta = c(85000, 98000, 70000, 110000, 120000),
  Calidad_Media = c(25000, 27000, 20000, 28000, 30000),
  Calidad_Baja = c(10000, 10000, 5000, 12000, 10000),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# INTERFAZ DE USUARIO (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&display=swap');
      body, .wrapper, .content-wrapper { font-family: 'Inter', sans-serif !important; background-color: #F4F6F9 !important; margin: 0; padding: 0; }
      
      .login-wrapper { display: flex; height: 100vh; width: 100vw; overflow: hidden; background-color: #FFFFFF; }
      .login-left-pane { 
        flex: 1.2; 
        background: linear-gradient(rgba(15, 23, 42, 0.4), rgba(15, 23, 42, 0.75)), url('https://images.unsplash.com/photo-1558788353-f76d92427f16?q=80&w=1200&auto=format&fit=crop') center/cover no-repeat;
        display: flex; flex-direction: column; justify-content: flex-end; padding: 70px; color: white;
      }
      .login-left-pane h1 { font-weight: 900; font-size: 3.1rem; margin-bottom: 16px; line-height: 1.15; letter-spacing: -0.5px; }
      .login-left-pane p { font-size: 1.3rem; color: #F1F5F9; font-weight: 500; max-width: 650px; line-height: 1.5; }
      
      .login-right-pane { flex: 1; display: flex; align-items: center; justify-content: center; padding: 40px; background-color: #FFFFFF; }
      .login-card-modern { width: 100%; max-width: 460px; padding: 25px; }
      .login-logo-badge { background-color: #1E6B34; color: white; width: 56px; height: 56px; border-radius: 14px; display: flex; align-items: center; justify-content: center; font-size: 1.6rem; margin-bottom: 22px; box-shadow: 0 4px 12px rgba(30, 107, 52, 0.3); }
      .login-title { font-size: 2.1rem; font-weight: 900; color: #0F172A; margin-bottom: 8px; }
      .login-subtitle { font-size: 1.05rem; color: #64748B; margin-bottom: 30px; font-weight: 500; }
      
      .form-control-modern { border-radius: 10px !important; border: 1px solid #CBD5E1 !important; padding: 14px 18px !important; height: auto !important; font-size: 1rem !important; color: #1E293B !important; background-color: #F8FAFC !important; }
      .form-control-modern:focus { border-color: #1E6B34 !important; background-color: #FFFFFF !important; box-shadow: 0 0 0 3px rgba(30, 107, 52, 0.1) !important; outline: none !important; }
      .login-label { font-size: 0.9rem; font-weight: 700; color: #334155; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px; display: block; }

      .btn-login-custom {
        background-color: #1E6B34 !important; color: #FFFFFF !important; font-weight: 700 !important;
        border-radius: 10px !important; padding: 14px !important; font-size: 1.05rem !important; border: none !important;
        width: 100% !important; box-shadow: 0 4px 12px rgba(30, 107, 52, 0.2); transition: all 0.2s;
      }
      .btn-login-custom:hover { background-color: #165228 !important; color: #FFFFFF !important; }

      /* Panel lateral izquierdo oscuro (#222D32 / #1E282C) */
      .main-sidebar { background-color: #222D32 !important; border-right: none !important; display: flex; flex-direction: column; justify-content: space-between; box-shadow: 2px 0 6px rgba(0,0,0,0.1); }
      .brand-link { background-color: #1A2226 !important; color: #FFFFFF !important; font-weight: 900 !important; font-size: 1.15rem !important; border-bottom: 1px solid #374850 !important; text-align: center; padding: 15px 10px !important;}
      .brand-link .fa-leaf { color: #FFFFFF !important; background-color: #2E7D32; padding: 6px; border-radius: 8px; margin-right: 8px;}

      /* Enlaces del menú lateral estilo AdminLTE oscuro */
      .nav-sidebar .nav-link { color: #B8C7CE !important; font-weight: 500; border-radius: 0 !important; margin: 0 !important; padding: 12px 18px !important; border-left: 3px solid transparent; transition: all 0.2s; }
      .nav-sidebar .nav-link i { margin-right: 10px; color: #8AA4AF; font-size: 1rem; }
      .nav-sidebar .nav-link:hover { background-color: #1E282C !important; color: #FFFFFF !important; }
      .nav-sidebar .nav-link.active { background-color: #1E282C !important; color: #FFFFFF !important; border-left: 3px solid #3C8DBC !important; font-weight: 600 !important; box-shadow: none !important;}
      .nav-sidebar .nav-link.active i { color: #FFFFFF !important; }

      /* Botón de Salir inferior en la sidebar */
      .sidebar-logout-container { padding: 15px 12px; margin-top: auto; border-top: 1px solid #374850; background-color: #1A2226; }
      .btn-logout-custom {
        background-color: #F4F4F4 !important; color: #444444 !important; font-weight: 700 !important; font-size: 0.85rem !important;
        border-radius: 4px !important; padding: 8px 14px !important; border: none !important; width: 100% !important;
        display: flex; align-items: center; justify-content: center; gap: 8px; transition: all 0.2s; text-transform: uppercase;
      }
      .btn-logout-custom i { color: #DD4B39; }
      .btn-logout-custom:hover { background-color: #DD4B39 !important; color: #FFFFFF !important; }
      .btn-logout-custom:hover i { color: #FFFFFF !important; }
      
      .page-header-custom { display: flex; justify-content: space-between; align-items: center; margin-bottom: 1.5rem; }
      .page-title-custom { font-size: 1.5rem; font-weight: 800; color: #1F2937; margin: 0; }
      
      .card { border-radius: 12px !important; border: 1px solid #E5E7EB !important; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05) !important; background-color: #FFFFFF !important; }
      .card-header { display: none !important; } 
      .card-body { padding: 1.5rem !important; }
      .box-title-custom { font-size: 1.1rem; font-weight: 800; color: #1F2937; margin-bottom: 1.5rem; }
      
      .kpi-card { padding: 1.2rem; border-radius: 12px; background: #FFF; box-shadow: 0 2px 4px rgba(0,0,0,0.02); border: 1px solid #E5E7EB; }
      .kpi-border-blue { border-left: 6px solid #3B82F6 !important; }
      .kpi-border-green { border-left: 6px solid #2E7D32 !important; }
      .kpi-border-yellow { border-left: 6px solid #F59E0B !important; }
      .kpi-border-red { border-left: 6px solid #EF4444 !important; }
      .kpi-title { font-size: 0.75rem; font-weight: 800; color: #9CA3AF; text-transform: uppercase; margin-bottom: 0.5rem; letter-spacing: 0.5px; }
      .kpi-val { font-size: 1.8rem; font-weight: 900; color: #111827; line-height: 1; }
      
      .leaflet-popup-content-wrapper { background-color: #0F172A !important; color: #F8FAFC !important; border-radius: 10px !important; padding: 5px !important; }
      .leaflet-popup-tip { background-color: #0F172A !important; }
      .leaflet-container a.leaflet-popup-close-button { color: #94A3B8 !important; }

      .filter-bar-card { background: #FFFFFF; border: 1px solid #E5E7EB; border-radius: 12px; padding: 12px 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.02); margin-bottom: 20px; }
      .campo-layout-wrapper { position: relative; padding-top: 10px; }

      .savia-card { border-radius: 14px !important; border: 1px solid #E5E7EB !important; background-color: #FFFFFF !important; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.03) !important; overflow: hidden; display: flex; flex-direction: column; height: 100%; }
      .savia-card-body { padding: 1.25rem !important; flex: 1; display: flex; flex-direction: column; }
      .savia-block { background-color: #F8FAFC; border: 1px solid #F1F5F9; border-radius: 10px; padding: 12px; margin-bottom: 12px; }
      .savia-block-title { font-size: 0.68rem; font-weight: 800; text-transform: uppercase; color: #64748B; margin-bottom: 4px; }
    "))
  ),
  
  uiOutput("contenedor_principal")
)

# ------------------------------------------------------------------------------
# LÓGICA DEL SERVIDOR (SERVER)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  sesion_iniciada <- reactiveVal(FALSE)
  datos_empresa <- reactiveVal(NULL)
  lotes_reactivos <- reactiveVal(NULL)
  
  celda_seleccionada <- reactiveVal(list(variedad = NULL, mes = NULL))
  
  observeEvent(input$celda_heatmap_clic, {
    celda_seleccionada(input$celda_heatmap_clic)
  })
  
  observeEvent(input$btn_cerrar_sesion, {
    sesion_iniciada(FALSE)
    lotes_reactivos(NULL)
    datos_empresa(NULL)
  })
  
  output$contenedor_principal <- renderUI({
    if (!sesion_iniciada()) {
      div(class = "login-wrapper",
          div(class = "login-left-pane",
              div(
                h1("Tecnología Satelital para la Floricultura de Precisión"),
                p("Optimiza los ciclos fenológicos, predice cosechas con precisión ANFIS y reduce drásticamente el desperdicio en tiempo real mediante gemelos digitales y control climático inteligente.")
              )
          ),
          div(class = "login-right-pane",
              div(class = "login-card-modern",
                  div(class = "login-logo-badge", icon("leaf")),
                  div(class = "login-title", "Iniciar Sesión"),
                  div(class = "login-subtitle", "Ingrese sus credenciales corporativas y tenant de florícola."),
                  
                  tags$label("Usuario", class = "login-label"),
                  textInput("txt_usuario", NULL, placeholder = "ej. admin", width = "100%"),
                  tags$style(" #txt_usuario { margin-bottom: 18px; } "),
                  
                  tags$label("Contraseña", class = "login-label"),
                  passwordInput("txt_clave", NULL, placeholder = "••••••••", width = "100%"),
                  tags$style(" #txt_clave { margin-bottom: 18px; } "),
                  
                  tags$label("ID Florícola (Tenant)", class = "login-label"),
                  textInput("txt_floricola", NULL, placeholder = "ej. bellaflor", width = "100%"),
                  tags$style(" #txt_floricola { margin-bottom: 25px; } "),
                  
                  actionButton("btn_login", "Acceder al Sistema", class = "btn-login-custom"),
                  
                  div(style = "text-align: right; margin-top: 18px;",
                      tags$a("¿Olvidaste tu clave?", href = "https://www.mentalytica.com/formulario-pymes/", target = "_blank", style = "font-size: 0.95rem; color: #1E6B34; font-weight: 700; text-decoration: none;")
                  ),
                  
                  uiOutput("mensaje_error_login")
              )
          )
      )
    } else {
      dashboardPage(
        title = "Sav.IA | Predictive SaaS",
        dark = NULL,
        header = dashboardHeader(title = dashboardBrand(title = tagList(icon("leaf"), " Sav.IA"), color = "white"), skin = "light", border = TRUE),
        sidebar = dashboardSidebar(
          skin = "dark", elevation = 0,
          sidebarMenu(
            id = "current_tab",
            menuItem("Presentación", tabName = "presentacion", icon = icon("home")),
            menuItem("Alta Gerencia", tabName = "gerencia", icon = icon("table-cells-large")),
            menuItem("Tarjetas de Alerta", tabName = "tarjetas", icon = icon("th-large")),
            menuItem("Gemelo Digital (Mapa)", tabName = "mapa", icon = icon("map"))
          ),
          div(class = "sidebar-logout-container",
              actionButton("btn_cerrar_sesion", tagList(icon("sign-out-alt"), "Cerrar Sesión"), class = "btn-logout-custom")
          )
        ),
        body = dashboardBody(
          tabItems(
            tabItem(tabName = "presentacion",
                    div(class = "card", style = "margin-top: 10px; border-radius: 12px; overflow: hidden;",
                        div(style = "background: linear-gradient(135deg, #1E6B34 0%, #0F172A 100%); padding: 30px; color: white;",
                            h2("Bienvenido a Sav.IA | Predictive SaaS", style = "font-weight: 900; margin-bottom: 8px;"),
                            p("Sistema integral de monitoreo, predicción fenológica y control satelital para floricultura de precisión.", style = "font-size: 1.1rem; margin: 0; opacity: 0.9;")
                        ),
                        div(style = "padding: 25px; color: #374151; font-size: 1rem; line-height: 1.6;",
                            p("Sav.IA permite determinar en tiempo real el comportamiento de los cultivos frente a parámetros climatológicos óptimos y predicciones ANFIS. Utilice el módulo 'Alta Gerencia' para visualizar indicadores macro, mapas de calor evolutivos y distribuciones polares, o diríjase a 'Tarjetas de Alerta' y 'Gemelo Digital' para la supervisión directa."),
                            tags$hr(style = "margin: 20px 0; border-color: #E5E7EB;"),
                            p("Soporte técnico y consultas corporativas: ", tags$a("soporte@savia.ia", href = "mailto:soporte@savia.ia", style = "color: #1E6B34; font-weight: 700; text-decoration: none;"))
                        )
                    )
            ),
            tabItem(tabName = "gerencia",
                    div(class = "page-header-custom", 
                        h2("Dashboard Estratégico (Vista Macro)", class = "page-title-custom")
                    ),
                    fluidRow(
                      column(width = 3, div(class = "kpi-card kpi-border-blue mb-4", p("NRO. LOTES", class = "kpi-title"), div(class = "kpi-val", textOutput("kpi_g_lotes", inline = TRUE)))),
                      column(width = 3, div(class = "kpi-card kpi-border-green mb-4", p("PLANTAS SEMBRADAS", class = "kpi-title"), div(class = "kpi-val", textOutput("kpi_g_plantas", inline = TRUE)))),
                      column(width = 3, div(class = "kpi-card kpi-border-yellow mb-4", p("PRODUCCIÓN (TALLOS)", class = "kpi-title"), div(class = "kpi-val", textOutput("kpi_g_vol", inline = TRUE)))),
                      column(width = 3, div(class = "kpi-card kpi-border-red mb-4", p("CALIDAD GLOBAL", class = "kpi-title"), uiOutput("kpi_g_calidad")))
                    ),
                    fluidRow(
                      column(width = 4, box(width = NULL, div(class="box-title-custom", "Distribución Global por Calidad"), echarts4rOutput("plot_ev_calidad", height = "340px"))),
                      column(width = 4, box(width = NULL, div(class="box-title-custom", uiOutput("titulo_desglose_calidad")), echarts4rOutput("plot_desglose_individual", height = "340px"))),
                      column(width = 4, box(width = NULL, div(class="box-title-custom", "Distribución Polar Apilada"), echarts4rOutput("plot_sunburst_calidad", height = "340px")))
                    ),
                    fluidRow(
                      column(width = 12, box(width = NULL, div(class="box-title-custom", "Heatmap Evolutivo: Producción por Variedad y Mes (D3.js)"), d3Output("plot_ev_variedad", height = "450px")))
                    )
            ),
            tabItem(tabName = "tarjetas",
                    div(class = "page-header-custom", 
                        h2("Tarjetas de Alerta", class = "page-title-custom")
                    ),
                    # 4 Indicadores superiores con lotes y producción debajo
                    fluidRow(
                      column(width = 3, div(class = "kpi-card kpi-border-blue mb-4", p("LOTES TOTALES", class = "kpi-title"), div(class = "kpi-val", textOutput("kpi_t_total", inline = TRUE)), div(style = "font-size: 0.75rem; font-weight: 600; color: #9CA3AF; margin-top: 4px;", textOutput("kpi_t_total_prod", inline = TRUE)))),
                      column(width = 3, div(class = "kpi-card kpi-border-green mb-4", p("LOTES ÓPTIMOS", class = "kpi-title"), div(class = "kpi-val text-success", textOutput("kpi_t_optimo", inline = TRUE)), div(style = "font-size: 0.75rem; font-weight: 600; color: #9CA3AF; margin-top: 4px;", textOutput("kpi_t_optimo_prod", inline = TRUE)))),
                      column(width = 3, div(class = "kpi-card kpi-border-yellow mb-4", p("LOTES EN ALERTA", class = "kpi-title"), div(class = "kpi-val text-warning", textOutput("kpi_t_alerta", inline = TRUE)), div(style = "font-size: 0.75rem; font-weight: 600; color: #9CA3AF; margin-top: 4px;", textOutput("kpi_t_alerta_prod", inline = TRUE)))),
                      column(width = 3, div(class = "kpi-card kpi-border-red mb-4", p("LOTES CRÍTICOS", class = "kpi-title"), div(class = "kpi-val text-danger", textOutput("kpi_t_critico", inline = TRUE)), div(style = "font-size: 0.75rem; font-weight: 600; color: #9CA3AF; margin-top: 4px;", textOutput("kpi_t_critico_prod", inline = TRUE))))
                    ),
                    div(class = "filter-bar-card",
                        fluidRow(
                          column(width = 4, uiOutput("filtro_var_ui")),
                          column(width = 4, selectInput("filtro_riesgo_campo", "Niveles", choices = c("Niveles: Todos", "CRÍTICO", "ALERTA", "ÓPTIMO"), width = "100%")),
                          column(width = 4, uiOutput("filtro_semana_ui"))
                        )
                    ),
                    div(style = "height: 520px; overflow-y: auto; padding-top: 10px;", uiOutput("grid_smart_cards"))
            ),
            tabItem(tabName = "mapa",
                    div(class = "campo-layout-wrapper",
                        h2("Gemelo Digital (Mapa)", class = "page-title-custom", style = "margin-bottom: 1rem;"),
                        div(style = "padding: 10px 0;",
                            div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
                                icon("map-marker-alt", style = "color: #166534; font-size: 1.2rem;"),
                                uiOutput("header_mapa_info")
                            ),
                            leafletOutput("mapa_gemelo", height = "580px")
                        )
                    )
            )
          ),
          absolutePanel(bottom = 20, right = 20, fixed = TRUE, draggable = FALSE, style = "z-index: 1050;", uiOutput("chat_panel"))
        )
      )
    }
  })
  
  observeEvent(input$btn_login, {
    req(input$txt_usuario, input$txt_clave, input$txt_floricola)
    
    res_login <- validar_login(input$txt_usuario, input$txt_clave, trimws(tolower(input$txt_floricola)))
    
    if (res_login$valido) {
      datos_empresa(res_login$empresa)
      
      withCallingHandlers({
        lotes_cargados <- inicializar_sesion_floricola(res_login$empresa)
        lotes_reactivos(lotes_cargados)
        sesion_iniciada(TRUE)
      }, error = function(e) {
        output$mensaje_error_login <- renderUI({
          p(paste("Error al cargar Drive:", e$message), class = "text-danger mt-3 text-center", style = "font-size: 0.9rem;")
        })
      })
      
    } else {
      output$mensaje_error_login <- renderUI({
        p(res_login$mensaje, class = "text-danger mt-3 text-center", style = "font-size: 0.9rem;")
      })
    }
  })
  
  output$filtro_var_ui <- renderUI({
    df <- lotes_reactivos()
    if (is.null(df) || !"variedad" %in% names(df)) {
      selectInput("filtro_var_campo", "Variedad", choices = c("Todas"), width = "100%")
    } else {
      selectInput("filtro_var_campo", "Variedad", choices = c("Todas", unique(df$variedad)), width = "100%")
    }
  })
  
  output$filtro_semana_ui <- renderUI({
    df <- lotes_reactivos()
    if (is.null(df) || !"semana_cosecha" %in% names(df)) {
      selectInput("filtro_semana_campo", "Semana", choices = c("Todas"), width = "100%")
    } else {
      selectInput("filtro_semana_campo", "Semana", choices = c("Todas", unique(df$semana_cosecha)), width = "100%")
    }
  })
  
  output$header_mapa_info <- renderUI({
    empresa <- datos_empresa(); req(empresa)
    div(
      h4(style = "margin: 0; font-weight: 800; font-size: 1.1rem; color: #1F2937;", paste("Vista Satelital -", empresa$nombre_empresa)),
      p(style = "margin: 0; font-size: 0.8rem; color: #6B7280;", sprintf("Centro: Lat %.5f, Lng %.5f", empresa$latitud, empresa$longitud))
    )
  })
  
  # KPIs Alta Gerencia
  output$kpi_g_lotes <- renderText({ df <- lotes_reactivos(); req(df); nrow(df) })
  output$kpi_g_plantas <- renderText({ 
    df <- lotes_reactivos(); req(df)
    val_plantas <- if("plantas" %in% names(df)) sum(df$plantas, na.rm = TRUE) else sum(df$produccion * 1.2, na.rm = TRUE)
    paste0(format(round(val_plantas/1000, 1), nsmall=1), "K")
  })
  output$kpi_g_vol <- renderText({ 
    df <- lotes_reactivos(); req(df) 
    paste0(format(round(sum(df$produccion)/1000, 1), nsmall=1), "K") 
  })
  output$kpi_g_calidad <- renderUI({
    df <- lotes_reactivos(); req(df)
    calidad_global <- round(mean(df$calidad_pct, na.rm = TRUE), 1)
    div(class = "kpi-val text-success", paste0(calidad_global, "%"))
  })
  
  # Formateador auxiliar para volumen de producción debajo en Tarjetas de Alerta
  fmt_prod_kpi <- function(x) { 
    if(length(x) == 0 || sum(x, na.rm = TRUE) == 0) return("0 tallos"); 
    paste0(format(round(sum(x, na.rm = TRUE)/1000, 1), big.mark = ".", decimal.mark = ","), "K tallos") 
  }
  
  # KPIs Tarjetas de Alerta (Lotes y Producción debajo)
  output$kpi_t_total <- renderText({ df <- lotes_reactivos(); req(df); nrow(df) })
  output$kpi_t_total_prod <- renderText({ df <- lotes_reactivos(); req(df); fmt_prod_kpi(df$produccion) })
  
  output$kpi_t_optimo <- renderText({ df <- lotes_reactivos(); req(df); sum(df$riesgo == 'green', na.rm = TRUE) })
  output$kpi_t_optimo_prod <- renderText({ df <- lotes_reactivos(); req(df); fmt_prod_kpi(df$produccion[df$riesgo == 'green']) })
  
  output$kpi_t_alerta <- renderText({ df <- lotes_reactivos(); req(df); sum(df$riesgo == 'yellow', na.rm = TRUE) })
  output$kpi_t_alerta_prod <- renderText({ df <- lotes_reactivos(); req(df); fmt_prod_kpi(df$produccion[df$riesgo == 'yellow']) })
  
  output$kpi_t_critico <- renderText({ df <- lotes_reactivos(); req(df); sum(df$riesgo == 'red', na.rm = TRUE) })
  output$kpi_t_critico_prod <- renderText({ df <- lotes_reactivos(); req(df); fmt_prod_kpi(df$produccion[df$riesgo == 'red']) })
  
  output$plot_ev_variedad <- renderD3({
    df <- lotes_reactivos()
    req(df)
    
    df_heat <- df %>%
      mutate(
        fecha_dt = as.Date(`Fecha Siembra (YYYY-MM-DD)`),
        mes_num = month(fecha_dt, label = TRUE, abbr = TRUE)
      ) %>%
      mutate(mes = if_else(is.na(mes_num), "Jan", as.character(mes_num)))
    
    df_mes_total <- df_heat %>%
      group_by(variedad, mes) %>%
      summarise(volumen_total = sum(produccion, na.rm = TRUE), .groups = "drop")
    
    lista_variedades <- unique(df_mes_total$variedad)
    lista_meses <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    matriz_js <- list()
    for(i in seq_len(nrow(df_mes_total))) {
      v_name <- df_mes_total$variedad[i]
      m_name <- df_mes_total$mes[i]
      v_idx <- which(lista_variedades == v_name) - 1
      m_idx <- which(lista_meses == m_name) - 1
      val_total <- df_mes_total$volumen_total[i]
      
      set.seed(v_idx * 100 + m_idx)
      pesos <- c(0.20, 0.32, 0.34, 0.14) + runif(4, -0.03, 0.03)
      pesos <- pmax(pesos, 0.05)
      pesos <- pesos / sum(pesos)
      
      sems_vals <- round(val_total * pesos)
      sems_vals[3] <- sems_vals[3] + (val_total - sum(sems_vals))
      
      if(length(v_idx) > 0 && length(m_idx) > 0 && m_idx >= 0) {
        matriz_js[[length(matriz_js) + 1]] <- list(
          v = v_idx, 
          m = m_idx, 
          total = val_total, 
          semanas = sems_vals
        )
      }
    }
    
    r2d3(
      data = list(
        variedades = lista_variedades,
        meses = lista_meses,
        matriz = matriz_js
      ),
      script = "heatmap_flores.js"
    )
  })
  
  output$plot_ev_calidad <- renderEcharts4r({
    df <- lotes_reactivos(); req(df)
    
    tot_alta <- sum(df$produccion * (df$calidad_pct / 100), na.rm = TRUE)
    tot_media <- sum(df$produccion * ((100 - df$calidad_pct) * 0.5 / 100), na.rm = TRUE)
    tot_baja <- sum(df$produccion * ((100 - df$calidad_pct) * 0.5 / 100), na.rm = TRUE)
    
    df_doughnut <- data.frame(
      calidad = c("Calidad Alta 🟢", "Calidad Media 🟡", "Calidad Baja 🔴"),
      valor = c(round(tot_alta), round(tot_media), round(tot_baja)),
      stringsAsFactors = FALSE
    )
    
    e_charts(df_doughnut, calidad) %>%
      e_pie(
        valor, 
        radius = c("50%", "85%"), 
        center = c("50%", "75%"),
        startAngle = 180,
        endAngle = 360,
        roseType = "radius"
      ) %>%
      e_color(c("#22C55E", "#F59E0B", "#EF4444")) %>%
      e_tooltip(trigger = "item") %>%
      e_legend(bottom = "10%", textStyle = list(fontSize = 10))
  })
  
  output$plot_sunburst_calidad <- renderEcharts4r({
    df <- lotes_reactivos(); req(df)
    
    df_hier <- df %>%
      mutate(
        Prod_Alta = produccion * (calidad_pct / 100),
        Prod_Media = produccion * ((100 - calidad_pct) * 0.5 / 100),
        Prod_Baja = produccion * ((100 - calidad_pct) * 0.5 / 100)
      ) %>%
      group_by(variedad) %>%
      summarise(
        Alta = round(sum(Prod_Alta, na.rm = TRUE)),
        Media = round(sum(Prod_Media, na.rm = TRUE)),
        Baja = round(sum(Prod_Baja, na.rm = TRUE)),
        .groups = "drop"
      )
    
    df_hier %>%
      e_charts(variedad) %>%
      e_polar() %>%
      e_angle_axis(variedad, category = TRUE, axisLabel = list(interval = 0, fontSize = 9)) %>%
      e_radius_axis() %>%
      e_bar(Alta, name = "Calidad Alta 🟢", stack = "calidad", coord_system = "polar", itemStyle = list(color = "#22C55E"), emphasis = list(focus = "series")) %>%
      e_bar(Media, name = "Calidad Media 🟡", stack = "calidad", coord_system = "polar", itemStyle = list(color = "#F59E0B"), emphasis = list(focus = "series")) %>%
      e_bar(Baja, name = "Calidad Baja 🔴", stack = "calidad", coord_system = "polar", itemStyle = list(color = "#EF4444"), emphasis = list(focus = "series")) %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(bottom = 0, textStyle = list(fontSize = 10))
  })
  
  output$titulo_desglose_calidad <- renderUI({
    sel <- celda_seleccionada()
    if (is.null(sel$variedad)) {
      "Desglose Individual (Seleccione celda en Heatmap)"
    } else {
      paste0("Desglose: ", sel$variedad, " (", sel$mes, ")")
    }
  })
  
  output$plot_desglose_individual <- renderEcharts4r({
    df <- lotes_reactivos(); req(df)
    sel <- celda_seleccionada()
    
    if (is.null(sel$variedad)) {
      df_filtrado <- df
    } else {
      df_filtrado <- df %>%
        mutate(
          fecha_dt = as.Date(`Fecha Siembra (YYYY-MM-DD)`),
          mes_num = month(fecha_dt, label = TRUE, abbr = TRUE)
        ) %>%
        mutate(mes = if_else(is.na(mes_num), "Jan", as.character(mes_num))) %>%
        filter(variedad == sel$variedad, mes == sel$mes)
    }
    
    if(nrow(df_filtrado) > 0) {
      tot_alta <- sum(df_filtrado$produccion * (df_filtrado$calidad_pct / 100), na.rm = TRUE)
      tot_media <- sum(df_filtrado$produccion * ((100 - df_filtrado$calidad_pct) * 0.5 / 100), na.rm = TRUE)
      tot_baja <- sum(df_filtrado$produccion * ((100 - df_filtrado$calidad_pct) * 0.5 / 100), na.rm = TRUE)
    } else {
      tot_alta <- 0; tot_media <- 0; tot_baja <- 0
    }
    
    df_doughnut_ind <- data.frame(
      calidad = c("Calidad Alta 🟢", "Calidad Media 🟡", "Calidad Baja 🔴"),
      valor = c(round(tot_alta), round(tot_media), round(tot_baja)),
      stringsAsFactors = FALSE
    )
    
    e_charts(df_doughnut_ind, calidad) %>%
      e_pie(
        valor, 
        radius = c("50%", "85%"), 
        center = c("50%", "75%"),
        startAngle = 180,
        endAngle = 360,
        roseType = "radius"
      ) %>%
      e_color(c("#22C55E", "#F59E0B", "#EF4444")) %>%
      e_tooltip(trigger = "item") %>%
      e_legend(bottom = "10%", textStyle = list(fontSize = 10))
  })
  
  data_campo <- reactive({
    df <- lotes_reactivos(); req(df)
    if (!is.null(input$filtro_var_campo) && input$filtro_var_campo != "Todas") df <- df %>% filter(variedad == input$filtro_var_campo)
    if (!is.null(input$filtro_riesgo_campo) && input$filtro_riesgo_campo != "Niveles: Todos") df <- df %>% filter(riesgo_label == input$filtro_riesgo_campo)
    if (!is.null(input$filtro_semana_campo) && input$filtro_semana_campo != "Todas") df <- df %>% filter(semana_cosecha == input$filtro_semana_campo)
    df
  })
  
  output$mapa_gemelo <- renderLeaflet({
    df <- data_campo(); empresa <- datos_empresa()
    
    leaflet(df) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = empresa$longitud, lat = empresa$latitud, zoom = 17) %>%
      # 1. Onda exterior difuminada (simula el radar de precisión)
      addCircleMarkers(
        lng = ~lng, lat = ~lat, 
        fillColor = ~color_hex, 
        fillOpacity = 0.25, 
        radius = 16, 
        stroke = TRUE, 
        weight = 1, 
        color = ~color_hex
      ) %>%
      # 2. Núcleo central sólido con el color de calidad exacto y borde blanco
      addCircleMarkers(
        lng = ~lng, lat = ~lat, 
        fillColor = ~color_hex, 
        fillOpacity = 0.95, 
        radius = 7, 
        stroke = TRUE, 
        weight = 2, 
        color = "#FFFFFF",
        popup = ~sprintf("<div style='font-family: Inter, sans-serif; padding: 6px;'><h4 style='margin: 0 0 8px 0; font-weight: 800; font-size: 1rem; color: #F8FAFC;'>%s - %s</h4><div style='font-size: 0.85rem; line-height: 1.5; color: #94A3B8;'><b>Estado:</b> <span style='color: %s; font-weight: 700;'>%s</span><br><b>Merma est.:</b> <span style='color: #F8FAFC; font-weight: 700;'>%s%%</span><br><i style='color: #38BDF8;'>Diagnóstico: %s</i></div></div>", id, variedad, color_hex, estado, merma, clima) %>% lapply(htmltools::HTML)
      )
  })
  
  output$grid_smart_cards <- renderUI({
    df <- data_campo()
    if(nrow(df) == 0) return(h5("No se encontraron resultados en esta selección.", class="text-muted p-4"))
    
    tarjetas <- purrr::map(1:nrow(df), function(i) {
      fila <- df[i, ]
      prod_fmt <- format(fila$produccion, big.mark = ".", decimal.mark = ",")
      
      div(class = "col-4 mb-4",
          div(class = "savia-card", style = paste0("border-top: 6px solid ", fila$color_hex, " !important;"),
              div(class = "savia-card-body",
                  div(class = "d-flex justify-content-between align-items-start mb-3",
                      div(h4(paste("Lote", fila$id), class="fw-bold mb-1", style="color:#1F2937; font-size: 1.25rem;"), span(fila$variedad, style="color:#6B7280; font-size:0.9rem;")),
                      span(class = paste0("badge bg-", fila$status_color), style="font-size:0.75rem; padding:6px 14px; border-radius:20px; font-weight: 700;", fila$riesgo_label)
                  ),
                  div(class = "savia-block",
                      fluidRow(
                        column(width = 8, p("1. CALIDAD EST. (ANFIS)", class = "savia-block-title"), div(style = "font-size: 1.15rem; font-weight: 800; color: #111827; margin-bottom: 2px;", paste0(fila$calidad_pct, "% de Calidad")), div(style = "font-size: 0.82rem; color: #334155;", HTML(paste0("Merma: <span style='color: ", fila$color_hex, "; font-weight: 700;'>", fila$merma, "%</span>")))),
                        column(width = 4, class = "d-flex align-items-center justify-content-center", div(style = paste0("width: 52px; height: 52px; border-radius: 50%; background: conic-gradient(", fila$color_hex, " ", fila$calidad_pct, "%, #E2E8F0 0%); display: flex; align-items: center; justify-content: center; position: relative;"), div(style = "width: 38px; height: 38px; border-radius: 50%; background: #FFFFFF; display: flex; flex-direction: column; align-items: center; justify-content: center;", span(style = "font-size: 0.68rem; font-weight: 800; color: #111827; line-height: 1;", paste0(fila$calidad_pct, "%")))))
                      )
                  ),
                  div(class = "savia-block", p("2. PRODUCCIÓN ESTIMADA", class = "savia-block-title"), div(style = "font-size: 1.15rem; font-weight: 800;", HTML(paste0("<span style='color: ", fila$color_hex, ";'>", prod_fmt, "</span> <span style='font-size: 0.85rem; font-weight: 600; color: #047857;'>tallos comerciales</span>")))),
                  div(class = "savia-block mb-3",
                      div(class = "d-flex justify-content-between align-items-center mb-1", p("3. RELOJ BIOLÓGICO & CORTE", class = "savia-block-title mb-0"), span(style = "font-size: 0.75rem; font-weight: 700; color: #111827;", HTML(paste0(fila$cosecha, " <span style='color: ", if(fila$status_color=="danger") "#EF4444" else "#22C55E", ";'>(", fila$estado, ")</span>")))),
                      div(style = "font-size: 0.8rem; color: #4B5563; margin-bottom: 6px;", paste0("Calor Acumulado: ", fila$calor_acumulado, " GDC")),
                      div(style = "width: 100%; background-color: #E2E8F0; border-radius: 9999px; height: 8px; margin-top: 4px; overflow: hidden;", div(style = paste0("width: ", fila$gdc, "%; background-color: ", fila$color_hex, "; height: 100%; border-radius: 9999px;")))
                  ),
                  div(class = "mt-auto", actionButton(paste0("btn_modal_", fila$id), "Ver Diagnóstico y Prescripción", class = "btn w-100", style = "background-color: #FFFFFF; border: 1px solid #D1D5DB; color: #1F2937; font-weight: 700; border-radius: 8px; padding: 10px; font-size: 0.85rem;"))
              )
          )
      )
    })
    
    tagList(lapply(seq(1, length(tarjetas), by = 3), function(i) { fluidRow(tarjetas[i:min(i+2, length(tarjetas))]) }))
  })
  
  observe({
    df <- lotes_reactivos()
    req(df)
    lapply(df$id, function(lote_id) {
      observeEvent(input[[paste0("btn_modal_", lote_id)]], {
        fila <- df %>% filter(id == lote_id)
        
        showModal(modalDialog(
          title = NULL, size = "l", easyClose = TRUE, footer = NULL,
          div(style = "margin: -15px; border-radius: 8px; overflow: hidden; font-family: 'Inter', sans-serif;",
              div(style = "background-color: #1E6B34; color: white; padding: 15px 20px; display: flex; justify-content: space-between; align-items: center;",
                  h4(style = "margin: 0; font-weight: 700; font-size: 1.1rem; display: flex; align-items: center; gap: 10px;", icon("cloud-rain"), " Asistente ANFIS (Satelital)"),
                  tags$button(type = "button", class = "close", `data-dismiss` = "modal", style = "color: white; opacity: 1; font-size: 1.5rem;", "×")
              ),
              div(style = "padding: 20px; background-color: #FFFFFF;",
                  div(style = "border-bottom: 1px solid #E5E7EB; padding-bottom: 15px; margin-bottom: 20px;",
                      h3(style = "font-weight: 800; color: #374151; font-size: 1.3rem; margin: 0 0 8px 0;", paste0("LOTE ", fila$id, " - ", toupper(fila$variedad))),
                      div(class = "d-flex align-items-center gap-2",
                          span(class = paste0("badge bg-", fila$status_color), style = "font-size: 0.8rem; padding: 5px 12px; font-weight: 700;", fila$riesgo_label),
                          span(style = "color: #6B7280; font-weight: 600; font-size: 0.9rem;", paste0("Cosecha estimada: ", fila$cosecha))
                      )
                  ),
                  
                  h4(style = "font-weight: 800; color: #1F2937; font-size: 1rem; margin-bottom: 15px;", "Condiciones vs Óptimo (4 Variables ANFIS)"),
                  
                  div(style = "margin-bottom: 12px;",
                      div(style = "display: flex; justify-content: space-between; font-size: 0.85rem; font-weight: 700; margin-bottom: 4px;",
                          span(style = "color: #4B5563;", HTML('<i class="fas fa-temperature-low text-danger"></i> Temperatura / Acumulación Térmica')),
                          span(style = "color: #DC2626;", "14°C (Óptimo: 18 - 22°C)")
                      ),
                      div(style = "width: 100%; background-color: #F3F4F6; height: 8px; border-radius: 4px; overflow: hidden;", div(style = "width: 45%; background-color: #EF4444; height: 100%; border-radius: 4px;"))
                  ),
                  
                  div(style = "margin-bottom: 12px;",
                      div(style = "display: flex; justify-content: space-between; font-size: 0.85rem; font-weight: 700; margin-bottom: 4px;",
                          span(style = "color: #4B5563;", HTML('<i class="fas fa-tint text-danger"></i> Humedad Relativa / Contenido Hídrico (NDWI)')),
                          span(style = "color: #DC2626;", "0.35 ndwi (Óptimo: 0.40 - 0.60 ndwi)")
                      ),
                      div(style = "width: 100%; background-color: #F3F4F6; height: 8px; border-radius: 4px; overflow: hidden;", div(style = "width: 55%; background-color: #EF4444; height: 100%; border-radius: 4px;"))
                  ),
                  
                  div(style = "margin-bottom: 12px;",
                      div(style = "display: flex; justify-content: space-between; font-size: 0.85rem; font-weight: 700; margin-bottom: 4px;",
                          span(style = "color: #4B5563;", HTML('<i class="fas fa-sun text-danger"></i> Intensidad de Luz')),
                          span(style = "color: #DC2626;", "1074.8 idx (Óptimo: 1200 - 1800 idx)")
                      ),
                      div(style = "width: 100%; background-color: #F3F4F6; height: 8px; border-radius: 4px; overflow: hidden;", div(style = "width: 60%; background-color: #EF4444; height: 100%; border-radius: 4px;"))
                  ),
                  
                  div(style = "margin-bottom: 18px;",
                      div(style = "display: flex; justify-content: space-between; font-size: 0.85rem; font-weight: 700; margin-bottom: 4px;",
                          span(style = "color: #4B5563;", HTML('<i class="fas fa-wind text-danger"></i> Déficit de Presión de Vapor (VPD)')),
                          span(style = "color: #DC2626;", "0.72 kPa (Óptimo: 0.40 - 0.60 kPa)")
                      ),
                      div(style = "width: 100%; background-color: #F3F4F6; height: 8px; border-radius: 4px; overflow: hidden;", div(style = "width: 70%; background-color: #EF4444; height: 100%; border-radius: 4px;"))
                  ),
                  
                  div(style = "background-color: #FEF2F2; border: 1px solid #FCA5A5; border-radius: 10px; padding: 15px; margin-bottom: 15px;",
                      div(style = "display: flex; align-items: center; gap: 8px; color: #991B1B; font-weight: 800; font-size: 0.95rem; margin-bottom: 6px;", icon("exclamation-triangle"), " Diagnóstico Fisiológico"),
                      p(style = "color: #7F1D1D; font-size: 0.88rem; margin: 0; line-height: 1.4;", paste0("Estrés detectado por satélite en el lote ", fila$id, ". El reloj biológico está en estado ", fila$estado, " con una merma proyectada del ", fila$merma, "%."))
                  ),
                  
                  div(style = "background-color: #F0FDF4; border: 1px solid #86EFAC; border-radius: 10px; padding: 15px; margin-bottom: 20px; display: flex; justify-content: space-between; align-items: center;",
                      div(
                        div(style = "display: flex; align-items: center; gap: 8px; color: #166534; font-weight: 800; font-size: 0.95rem; margin-bottom: 6px;", icon("leaf"), " Prescripción Directa (IA)"),
                        p(style = "color: #14532D; font-size: 0.88rem; margin: 0; line-height: 1.4; max-width: 480px;", "Ajustar temperatura cerrando cortinas laterales un 30% adicional. Reducir lámina de riego en un 15% por 3 días.")
                      ),
                      div(style = "background-color: #FEF2F2; color: #DC2626; border: 1px solid #FCA5A5; padding: 6px 12px; border-radius: 6px; font-size: 0.75rem; font-weight: 800; white-space: nowrap;", "ESFUERZO: ALTO")
                  ),
                  
                  actionButton(paste0("cerrar_modal_", lote_id), "Cerrar Ventana", class = "btn w-100", style = "background-color: #1E6B34; color: white; font-weight: 700; border-radius: 8px; padding: 10px;")
              )
          )
        ))
      })
      
      observeEvent(input[[paste0("cerrar_modal_", lote_id)]], {
        removeModal()
      })
    })
  })
  
  chat_open <- reactiveVal(FALSE)
  chat_history <- reactiveVal(data.frame(rol = "ia", text = "Hola, soy Sav.IA. ¿Qué deseas consultar?", stringsAsFactors = FALSE))
  observeEvent(input$btn_toggle_chat, { chat_open(!chat_open()) })
  
  observeEvent(input$btn_send_chat, {
    req(input$txt_chat)
    hist <- chat_history()
    hist <- rbind(hist, data.frame(rol = "user", text = input$txt_chat))
    msg <- tolower(input$txt_chat)
    resp <- if(grepl("calidad|merma", msg)) "Tenemos lotes en riesgo crítico (merma > 20%) debido a estrés hídrico." else if(grepl("produccion|tallos", msg)) "Proyección: 1.25M tallos." else "Intenta con 'calidad', 'producción' o 'fechas'."
    hist <- rbind(hist, data.frame(rol = "ia", text = resp))
    chat_history(hist)
    updateTextInput(session, "txt_chat", value = "")
  })
  
  output$chat_panel <- renderUI({
    if (!chat_open()) {
      actionButton("btn_toggle_chat", HTML('<i class="fas fa-leaf" style="margin-right: 6px;"></i> Consultar a Sav.IA'), class = "btn text-white elevation-3", style = "background-color: #1E6B34; border-radius: 30px; padding: 10px 20px; font-weight: 700; font-size: 0.9rem; border: none;")
    } else {
      div(class = "card", style="width: 320px; overflow: hidden; box-shadow: 0 10px 25px rgba(0,0,0,0.15);",
          div(class = "bg-success text-white p-2 d-flex justify-content-between align-items-center", tags$b("Sav.IA Copilot"), actionButton("btn_toggle_chat", "", icon = icon("times"), class="btn-sm btn-success border-0")),
          div(style = "height: 250px; overflow-y: auto; padding: 10px; background-color:#F4F7F6;",
              purrr::map(1:nrow(chat_history()), function(i) {
                row <- chat_history()[i,]
                align <- if(row$rol == "user") "text-right" else "text-left"
                bg <- if(row$rol == "user") "bg-success text-white" else "bg-white border text-dark"
                div(class = align, div(class = paste("d-inline-block p-2 mb-2 rounded", bg), style="max-width: 85%; font-size:13px; text-align:left;", row$text))
              })
          ),
          div(class = "p-2 bg-white d-flex gap-2", textInput("txt_chat", NULL, placeholder = "Pregunta...", width = "100%"), actionButton("btn_send_chat", "", icon = icon("paper-plane"), class = "btn-success"))
      )
    }
  })
}

shinyApp(ui, server)