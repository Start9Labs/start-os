import { Pipe, PipeTransform } from '@angular/core'
import { SessionWithId } from './table.component'

@Pipe({
  name: 'platformInfo',
})
export class PlatformInfoPipe implements PipeTransform {
  transform({ kind, userAgent }: SessionWithId): {
    name: string
    icon: string
  } {
    if (kind === 'token') {
      return {
        name: 'API',
        icon: '@tui.code',
      }
    }

    if (!userAgent) {
      return {
        name: 'API',
        icon: '@tui.code',
      }
    }

    if (/Android/i.test(userAgent)) {
      return {
        name: 'Android',
        icon: '@tui.smartphone',
      }
    }

    if (/iPhone/i.test(userAgent)) {
      return {
        name: 'iPhone',
        icon: '@tui.smartphone',
      }
    }

    if (/iPad/i.test(userAgent)) {
      return {
        name: 'iPad',
        icon: '@tui.smartphone',
      }
    }

    return {
      name: 'Desktop/Laptop',
      icon: '@tui.monitor',
    }
  }
}
