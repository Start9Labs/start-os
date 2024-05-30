import { inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { BadgeService } from 'src/app/services/badge.service'

export const SYSTEM_UTILITIES: Record<string, { icon: string; title: string }> =
  {
    '/portal/system/notifications': {
      icon: 'tuiIconBell',
      title: 'Notifications',
    },
    '/portal/system/marketplace': {
      icon: 'tuiIconShoppingCart',
      title: 'Marketplace',
    },
    '/portal/system/updates': {
      icon: 'tuiIconGlobe',
      title: 'Updates',
    },
    '/portal/system/sideload': {
      icon: 'tuiIconUpload',
      title: 'Sideload',
    },
    '/portal/system/logs': {
      icon: 'tuiIconFileText',
      title: 'Logs',
    },
    '/portal/system/metrics': {
      icon: 'tuiIconActivity',
      title: 'Metrics',
    },
    '/portal/system/backups': {
      icon: 'tuiIconSave',
      title: 'Backups',
    },
    '/portal/system/settings': {
      icon: 'tuiIconTool',
      title: 'Settings',
    },
  }

export function getMenu() {
  const badge = inject(BadgeService)

  return Object.keys(SYSTEM_UTILITIES).map(key => ({
    name: SYSTEM_UTILITIES[key].title,
    icon: SYSTEM_UTILITIES[key].icon,
    routerLink: key,
    badge: toSignal(badge.getCount(key), { initialValue: 0 }),
  }))
}
