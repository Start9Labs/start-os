import type { i18n } from '../i18n.providers'

export default {
  ui: {
    back: 'Atrás',
    change: 'Cambiar',
    update: 'Actualizar',
    reset: 'Reiniciar',
  },
  system: {
    outlet: {
      system: 'Sistema',
      general: 'General',
      email: 'Correo Electrónico',
      backup: 'Crear Copia de Seguridad',
      restore: 'Restaurar Copia de Seguridad',
      interfaces: 'Direcciones de Interfaz de Usuario',
      acme: 'ACME',
      wifi: 'WiFi',
      sessions: 'Sesiones Activas',
      ssh: 'SSH',
      password: 'Cambiar Contraseña',
    },
    general: {
      title: 'Configuración General',
      subtitle: 'Gestiona tu configuración general y preferencias',
      tab: 'Título de la Pestaña del Navegador',
      language: 'Idioma',
      repair: {
        title: 'Reparación de Disco',
        subtitle: 'Intentar reparación automática',
        button: 'Reparar',
      },
      ca: {
        title: 'Autoridad de Certificación Raíz',
        subtitle: 'Descarga la autoridad certificadora raíz de tu servidor',
        button: 'Descarga',
      },
      tor: {
        title: 'Reiniciar Tor',
        subtitle: 'Reiniciar el daemon de Tor en tu servidor',
      },
      update: {
        title: 'Actualización de Software',
        button: {
          restart: 'Reiniciar para aplicar',
          check: 'Buscar actualizaciones',
        },
      },
      sync: {
        title: 'Fallo en la sincronización del reloj',
        subtitle:
          'Esto causará problemas de conectividad. Para resolverlo, consulta la',
      },
    },
  },
} satisfies i18n
