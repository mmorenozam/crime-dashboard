## mod_about.R --------------------------------------------------------------
## Static "About" page. Kept as its own module for consistency, even though
## it has no server-side reactive logic, so all page content lives under R/.

mod_about_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    width = 1,
    card(
      card_header(h3("Acerca de este Dashboard")),
      markdown(
      "
### Propósito

**Homicidios y Detenciones en Quito** reúne registros de homicidios y detenciones del Distrito Metropolitano de Quito en una sola herramienta exploratoria. Está diseñada para ayudar a analistas, investigadores y tomadores de decisiones a entender *dónde*, *cuándo* y *bajo qué circunstancias* ocurren estos eventos, así como a comparar el número de casos registrados con las tasas de incidencia ajustadas por población entre parroquias y años.

### Fuentes de datos

* **Límites administrativos**: contorno de Quito y polígonos de las parroquias, utilizados para la agregación espacial y la elaboración de mapas coropléticos consultados en Julio de 2026 del <a href='https://geoportal.quito.gob.ec/' target='_blank' rel='noopener noreferrer'>Geoportal de la ciudad de Quito</a>.

* **Homicidios**: registros individuales de homicidios, incluyendo el tipo de muerte, presunta motivación, arma, tipo de lugar, características demográficas y fecha/hora del evento consultados en Julio de 2026 de la página de abiertos del Gobierno del Ecuador, <a href='https://www.datosabiertos.gob.ec/dataset/?organization=ministerio-del-interior' target='_blank' rel='noopener noreferrer'>sección del Ministerio del Interior</a>.

* **Detenciones**: registros individuales de detenciones y aprehensiones, incluyendo la clasificación de delitos ICCS, tipo de evento, características demográficas y fecha/hora del evento consultados en Julio de 2026 de la página de abiertos del Gobierno del Ecuador, <a href='https://www.datosabiertos.gob.ec/dataset/?organization=ministerio-del-interior' target='_blank' rel='noopener noreferrer'>sección del Ministerio del Interior</a>.

* **Población por parroquia**: estimaciones anuales de población por parroquia (2001-2035), utilizadas exclusivamente para calcular las tasas de incidencia con la misma metodología para las proyecciones poblacionales de Quito 2022 - 2035, <a href='https://github.com/gobierno-abierto2026/Poblacion-Quito-2022-2035' target='_blank' rel='noopener noreferrer'>usadas por el Municipio de Quito</a>.

### Principales funcionalidades

* Páginas independientes de **Homicidios** y **Detenciones**, cada una con filtros adaptados a las variables propias de cada conjunto de datos.
* Cuatro **modos de mapa** interactivos: ubicación de casos individuales, mapa de calor de densidad, agrupación de marcadores y mapa coroplético de incidencia normalizada por población.
* Una sección de **Tendencias** con evolución temporal, distribución por sexo, distribución por grupos de edad y un mapa de calor por hora y día de la semana. Todas las visualizaciones responden a los filtros activos.
* **Tablas resumen**: frecuencia de uso de armas para homicidios y frecuencia de clasificación ICCS para detenciones.
* Una tabla de **datos en bruto** que permite buscar, ordenar y descargar los registros filtrados.

### Guía rápida de uso

1. Selecciona una página (**Homicidios** o **Detenciones**) desde la barra de navegación superior.
2. Utiliza el panel lateral izquierdo para filtrar los datos: tipo de mapa, rango de años y cualquier combinación de variables categóricas.
3. Explora la pestaña **Mapa** para identificar patrones espaciales, o utiliza las subpestañas para revisar las **Tendencias**, **Tablas resumen** o los **Datos en bruto**.
4. Al filtrar por una o más parroquias, o por zona Urbana/Rural, los gráficos de evolución temporal y distribución por sexo se desagregarán automáticamente según esa dimensión para facilitar la comparación.
5. Para quitar una selección en los filtros, haz click sobre ella y presiona la tecla de 'Backspace' (delete).

### Interpretación de incidencia vs. conteos

Los **conteos** representan el número absoluto de eventos registrados y son útiles para preguntas operativas y de asignación de recursos, pero están fuertemente influenciados por el número de personas que viven en cada parroquia. Las tasas de **incidencia** (eventos por cada 100.000 habitantes, utilizando las estimaciones anuales de población por parroquia) ajustan por el tamaño de la población y son una métrica más apropiada para *comparar* parroquias o evaluar si un cambio real en el riesgo ha ocurrido a lo largo del tiempo.

Las parroquias pequeñas, con poblaciones reducidas, pueden mostrar grandes variaciones en la incidencia a partir de tan solo unos pocos eventos. Por ello, siempre es recomendable revisar el número de casos subyacente junto con la tasa de incidencia antes de sacar conclusiones fuertes.

### Nota metodológica

Los registros de detenciones y aprehensiones originalmente no cuentan en su totalidad con códigos ICCS, para la realización de este dashboard, imputé los datos usando lookup tables, Random Forest y curación manual como se describe <a href='https://mauricio-moreno.net/projects/iccs-imputation/' target='_blank' rel='noopener noreferrer'>aquí</a>.

### Créditos

Esta aplicación fue diseñada y desarrollada por **Mauricio Moreno, PhD**, con ideas de implementación y sugerencias de código aportadas por **ChatGPT** y **Claude**, asistentes de inteligencia artificial utilizados durante el desarrollo.

### Contacto

Para cualquier duda o comentario, puedes contactarme a mi correo electrónico: mmorenozambrano@gmail.com, o a través de mi <a href='https://www.linkedin.com/in/mmorenozam/' target='_blank' rel='noopener noreferrer'>LinkedIn</a>.
        "
      )
    )
  )
}

mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic required for a static informational page.
  })
}
