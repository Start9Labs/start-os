import { inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { BadgeService } from 'src/app/services/badge.service'

export const SYSTEM_UTILITIES: Record<string, { icon: string; title: string }> =
  {
    '/portal/services': {
      icon: '@tui.layout-grid',
      title: 'Services',
    },
    '/portal/marketplace': {
      icon: '@tui.shopping-cart',
      title: 'Marketplace',
    },
    '/portal/sideload': {
      icon: '@tui.upload',
      title: 'Sideload',
    },
    // '/portal/updates': {
    //   icon: '@tui.globe',
    //   title: 'Updates',
    // },
    '/portal/backups': {
      icon: '@tui.save',
      title: 'Backups',
    },
    '/portal/metrics': {
      icon: '@tui.activity',
      title: 'Metrics',
    },
    '/portal/logs': {
      icon: '@tui.file-text',
      title: 'Logs',
    },
    '/portal/settings': {
      icon: '@tui.wrench',
      title: 'Settings',
    },
    '/portal/notifications': {
      icon: '@tui.bell',
      title: 'Notifications',
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
