import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'platformInfo',
  standalone: true,
})
export class PlatformInfoPipe implements PipeTransform {
  transform(userAgent: string | null): {
    name: string
    icon: string
  } {
    if (!userAgent) {
      return {
        name: 'CLI',
        icon: '@tui.terminal',
      }
    }

    if (/Android/i.test(userAgent)) {
      return {
        name: 'Android Device',
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
