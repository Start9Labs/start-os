import { inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { BadgeService } from 'src/app/services/badge.service'

export const SYSTEM_UTILITIES: Record<string, { icon: string; title: string }> =
  {
    '/portal/system/notifications': {
      icon: '@tui.bell',
      title: 'Notifications',
    },
    '/portal/system/marketplace': {
      icon: '@tui.shopping-cart',
      title: 'Marketplace',
    },
    '/portal/system/updates': {
      icon: '@tui.globe',
      title: 'Updates',
    },
    '/portal/system/sideload': {
      icon: '@tui.upload',
      title: 'Sideload',
    },
    '/portal/system/logs': {
      icon: '@tui.file-text',
      title: 'Logs',
    },
    '/portal/system/metrics': {
      icon: '@tui.activity',
      title: 'Metrics',
    },
    '/portal/system/backups': {
      icon: '@tui.save',
      title: 'Backups',
    },
    '/portal/system/settings': {
      icon: '@tui.wrench',
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