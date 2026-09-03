# IASI Quarto User Guide


`iasi.quarto` ayuda a organizar, construir y publicar proyectos documentales basados en Quarto.

IASI Quarto nació de una necesidad concreta: gestionar distintos tipos de publicaciones dentro de una misma estructura documental, compartiendo reglas de construcción, validación, formatos, publicación y despliegue sin obligar a que todas las publicaciones tengan que organizarse de la misma manera.

IASI es su primer caso de uso y el entorno en el que el framework se desarrolla y se valida. Por eso su implementación actual responde a problemas reales del propio proyecto. Sin embargo, el modelo no está ligado a IASI ni pretende ser una solución exclusiva para su estructura documental.

La idea es que otras organizaciones puedan partir de una infraestructura de publicación ya resuelta y realizar únicamente el *tailoring* necesario para adaptarla a su contexto: tipos de publicación, perfiles, formatos, estilos, convenciones y reglas propias. El pipeline, los contratos y los mecanismos comunes no deberían tener que reinventarse para cada organización.

En otras palabras: gran parte del problema ya está resuelto; cada organización adapta la parte que le pertenece.

Esta guía explica **cómo utilizarlo**. No describe su arquitectura interna ni el funcionamiento de sus componentes privados.

El recorrido recomendado es sencillo:

1.  conocer los términos que utiliza IASI Quarto;
2.  instalar la herramienta;
3.  elegir el tipo de publicación y su estrategia;
4.  configurar los formatos de salida;
5.  construir y publicar;
6.  utilizar un multiproyecto cuando varias publicaciones comparten un mismo espacio de trabajo.

Si ya conoce IASI Quarto, puede ir directamente al capítulo que corresponda a la tarea que quiere realizar.
