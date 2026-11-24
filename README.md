# Calculadora estadística en R

Paquete de R con una aplicación Shiny que calcula medidas descriptivas de una variable y medidas de relación entre variables, construye tablas de contingencia y aplica técnicas de muestreo, con gráficas interactivas. Es la traducción a R de la versión en Python del proyecto de la Maestría en Sistemas Computacionales (TecNM, campus Apizaco).

## Qué hace

- Recibe datos capturados a mano o desde un archivo CSV (detecta el separador y la codificación).
- Descarta los valores no numéricos y avisa cuántos descartó.
- Permite elegir entre cálculo **muestral** o **poblacional**.
- Si una medida no aplica a los datos (por ejemplo, la media armónica con un 0), muestra "No aplica" con el motivo, sin afectar a las demás.
- Tema claro y oscuro, con paletas distinguibles con daltonismo (Okabe-Ito).

| Pestaña | Contenido |
| --- | --- |
| Una variable | Media, mediana, moda, medias armónica y geométrica; rango, varianza, desviación estándar y rango intercuartílico; asimetría y curtosis con su interpretación; histograma con curva de densidad |
| Relación entre variables | Matriz de covarianza; correlación de Pearson, Spearman, Kendall (tau-a, b y c) y gamma con mapa de calor; dispersión con recta de regresión y color por grupo |
| Tablas de contingencia | Frecuencias y porcentajes por fila, por columna o del total |
| Muestreo | Aleatorio simple, sistemático, estratificado (proporcional, igual y Neyman), por conglomerados y por cuotas; calculadora de tamaño de muestra; comparación de la muestra con la población |

## Requisitos

- R 4.3 o superior (desarrollado con R 4.6.2)
- Las dependencias se instalan con renv a partir de `renv.lock`

## Instalación

```bash
git clone https://github.com/ManuelLagunas/estadistica_app_r.git
cd estadistica_app_r
```

En la consola de R, dentro de la carpeta del proyecto:

```r
renv::restore()   # instala las versiones exactas de renv.lock
```

## Uso

```r
shiny::runApp()
```

La aplicación se abre en el navegador o en el visor de Positron. Los archivos de `datos_prueba/` (`iris.csv`, `mtcars.csv` y `ejemplo.csv`) sirven para probar todas las pestañas.

> Usa punto para los decimales (`7.5`). En la captura manual, la coma separa valores: `7,5` se lee como dos datos.

Las funciones también se pueden usar sin la interfaz:

```r
pkgload::load_all()
datos <- limpiar_serie(desde_texto("2, 4, 4, 4, 5, 5, 7, 9"))$datos
media(datos)
correlacion_kendall(c(1, 2, 2, 3), c(1, 2, 3, 3), variante = "c")
```

## Estructura

```
estadistica_app_r/
├── app.R                          # Punto de arranque: shiny::runApp()
├── DESCRIPTION                    # Nombre, autor y dependencias del paquete
├── renv.lock                      # Versiones exactas de los paquetes
├── R/
│   ├── entradas_manual.R          # desde_texto
│   ├── entradas_csv.R             # leer_csv: separador y codificación
│   ├── errores.R                  # Clase de error error_estadistica
│   ├── validacion.R               # limpiar_serie, limpiar_tabla
│   ├── calculos_centralidad.R     # media, mediana, moda, armónica, geométrica
│   ├── calculos_variabilidad.R    # rango, varianza, desviación, IQR
│   ├── calculos_forma.R           # asimetría, curtosis e interpretación
│   ├── calculos_relacion.R        # covarianza, correlaciones, regresión, contingencia
│   ├── muestreo.R                 # técnicas de muestreo y tamaño de muestra
│   ├── interfaz_app.R             # ejecutar(): pestañas y tema
│   ├── interfaz_una_variable.R    # un módulo de Shiny por pestaña
│   ├── interfaz_relacion.R
│   ├── interfaz_contingencia.R
│   ├── interfaz_muestreo.R
│   ├── interfaz_componentes.R     # tarjetas, secciones y avisos compartidos
│   ├── interfaz_graficas.R        # gráficas con plotly
│   ├── interfaz_columnas.R        # columnas de grupo y numéricas
│   └── interfaz_formato.R         # números a 2 decimales
├── tests/testthat/                # pruebas automatizadas
├── datos_prueba/                  # iris.csv, mtcars.csv, ejemplo.csv
└── docs/                          # reporte técnico en Quarto
```

El flujo de datos es siempre el mismo:

```
entrada → texto o data.frame crudo → validación → cálculos → interfaz
```

## Convenciones

- **Varianza y desviación:** el parámetro `muestral` decide si se divide entre n − 1 o entre n. `stats::var()` no se usa porque siempre divide entre n − 1.
- **Cuartiles:** interpolación lineal (`type = 7`), igual que pandas y `CUARTIL.INC` de Excel.
- **Asimetría y curtosis:** con la corrección muestral de pandas y Excel; la curtosis es en exceso (normal = 0).
- **Errores:** cuando los datos no permiten calcular una medida, la función lanza un error de clase `error_estadistica` con un mensaje claro.
- **Semilla:** las técnicas de muestreo aceptan `semilla` y no cambian el generador aleatorio global. Con la misma semilla, R y Python eligen muestras distintas, porque sus generadores son distintos.

## Desarrollo

```r
pkgload::load_all()          # carga el paquete sin instalarlo
roxygen2::roxygenise()       # genera la documentación de man/ y el NAMESPACE
testthat::test_local()       # ejecuta las pruebas
```

## Reporte técnico

El reporte en `docs/reporte.qmd` ejecuta el código del paquete y se genera en HTML, PDF y Word. Requiere Quarto, TinyTeX y Google Chrome (para convertir las gráficas en imágenes con webshot2):

```bash
cd docs
quarto render reporte.qmd
```

## Estado

Implementado: captura manual y por CSV, validación, todas las medidas de la versión en Python, las cuatro pestañas, las gráficas y las pruebas automatizadas.

Pendiente:

- [ ] Entrada por URL
- [ ] Entrada por cámara con reconocimiento de texto (OCR)
- [ ] Coeficiente de variación
- [ ] Prueba chi-cuadrada en las tablas de contingencia

## Autor

Manuel Alejandro Lagunas Aragón
