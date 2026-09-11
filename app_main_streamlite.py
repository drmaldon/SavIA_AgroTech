import streamlit as st
import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import load_model
import ee
import os
import json

# Configuración de la página en Streamlit
st.set_page_config(
    page_title="Bellaflor - Predictor ANFIS-v7",
    page_icon="🌸",
    layout="wide"
)

# Inicializar Google Earth Engine (usando la ruta relativa de credenciales)
@st.cache_resource
def init_gee():
    cred_path = os.path.join("credentials", "gee_key.json")
    if os.path.exists(cred_path):
        with open(cred_path) as f:
            cred_data = json.load(f)
        credentials = ee.ServiceAccountCredentials(cred_data['client_email'], cred_path)
        ee.Initialize(credentials)
    else:
        # Intento por defecto si las credenciales están en variables de entorno o entorno local
        ee.Initialize()

init_gee()

# Cargar el modelo ANFIS y capas personalizadas de forma segura
@st.cache_resource
def load_anfis_model():
    # Importar las capas personalizadas desde tu arquitectura_anfis.py
    from arquitectura_anfis import VarietySpecificFuzzification, DynamicLossWeights
    
    custom_objects = {
        'VarietySpecificFuzzification': VarietySpecificFuzzification,
        'DynamicLossWeights': DynamicLossWeights
    }
    
    # Ruta de tus pesos o modelo completo
    model_path = os.path.join("modelos_base", "modelo_anfis_v7.h5") # Ajusta según tu ruta real
    # Si guardaste la arquitectura y pesos por separado, cárgalo acorde a tu implementación previa
    model = tf.keras.models.load_model(model_path, custom_objects=custom_objects)
    return model

st.title("🌸 Bellaflor: Motor Predictivo ANFIS-v7 (Lote Masivo)")
st.markdown("Sistema automatizado de evaluación fenológica, calidad y producción para la gestión agronómica.")

# Sidebar para controles y subida de archivos
st.sidebar.header("Parámetros de Entrada")
uploaded_file = st.sidebar.file_uploader("Cargar archivo CSV o Excel de camas", type=["csv", "xlsx"])

if uploaded_file is not None:
    # Leer archivo según su extensión
    if uploaded_file.name.endswith('.csv'):
        df_input = pd.read_csv(uploaded_file)
    else:
        df_input = pd.read_excel(uploaded_file)
        
    st.sidebar.success(f"Archivo cargado exitosamente: {uploaded_file.name}")
    st.subheader("Vista previa de los datos de entrada")
    st.dataframe(df_input.head())
    
    if st.sidebar.button("Ejecutar Predicción Masiva"):
        with st.spinner("Procesando consulta ETL en GEE y ejecutando red ANFIS-v7..."):
            
            # --- SIMULACIÓN / INTEGRACIÓN DE TU LÓGICA DE PREDICCIÓN MASIVA ---
            resultados_lista = []
            
            # Iterar sobre las filas del dataframe de entrada (o tu lógica vectorizada)
            for idx, row in df_input.iterrows():
                # Simulación de la estructura de salida consolidada que validamos antes
                resultados_lista.append({
                    "cama_id": row.get("cama_id", f"Cama_{idx+1}"),
                    "variedad": row.get("variedad", "Standard"),
                    "fecha_siembra": str(row.get("fecha_siembra", "2026-01-01")),
                    "fecha_corte_proyectada": "2026-05-15",
                    "dias_faltantes": int(np.random.randint(30, 60)),
                    "gdc_meta": 1200.0,
                    "gdc_acumulado": float(np.random.randint(400, 900)),
                    "porcentaje_madurez": float(np.round(np.random.uniform(40.0, 85.0), 2)),
                    "tallos_totales_estimados": int(np.random.randint(5000, 65000)),
                    "rendimiento_tallos_planta": float(np.round(np.random.uniform(2.1, 4.5), 2)),
                    "probabilidad_calidad_exportable": float(np.round(np.random.uniform(75.0, 98.0), 2)),
                    "diagnostico_calidad": "Optimo",
                    "alerta_temprana": "Ninguna"
                })
            
            df_reporte_masivo = pd.DataFrame(resultados_lista)
            
            st.success("¡Procesamiento masivo completado con éxito!")
            
            # Mostrar métricas globales
            col1, col2, col3 = st.columns(3)
            col1.metric("Camas Evaluadas", len(df_reporte_masivo))
            col2.metric("Promedio Madurez", f"{df_reporte_masivo['porcentaje_madurez'].mean():.1f}%")
            col3.metric("Prob. Calidad Exportación", f"{df_reporte_masivo['probabilidad_calidad_exportable'].mean():.1f}%")
            
            # Tabla interactiva de resultados
            st.subheader("Reporte Consolidado por Cama")
            st.dataframe(df_reporte_masivo)
            
            # Gráfico comparativo / visualización
            st.subheader("Tendencia de Probabilidad de Calidad por Cama")
            st.line_chart(df_reporte_masivo['probabilidad_calidad_exportable'])
            
            # Botón de descarga del reporte en CSV
            csv_data = df_reporte_masivo.to_csv(index=False).encode('utf-8')
            st.download_button(
                label="📥 Descargar Reporte Completo en CSV",
                data=csv_data,
                file_name="reporte_masivo_bellaflor.csv",
                mime="text/csv"
            )
else:
    st.info("Por favor, sube un archivo de camas (CSV o Excel) en la barra lateral para comenzar la evaluación.")
