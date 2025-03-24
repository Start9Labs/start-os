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
      update: 'Actualización de Software',
      restart: 'Reiniciar para aplicar',
      check: 'Buscar actualizaciones',
      tab: 'Título de la Pestaña del Navegador',
      language: 'Idioma',
      tor: 'Reiniciar Tor',
      daemon: 'Reiniciar el daemon de Tor en tu servidor',
      disk: 'Reparación de Disco',
      attempt: 'Intentar reparación automática',
      repair: 'Reparar',
      sync: {
        title: 'Fallo en la sincronización del reloj',
        subtitle:
          'Esto causará problemas de conectividad. Para resolverlo, consulta la',
      },
    },
  },
} satisfies i18n
