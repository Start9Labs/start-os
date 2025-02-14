import { Pipe, PipeTransform } from '@angular/core'
import { PlatformType } from 'src/app/services/api/api.types'

@Pipe({
  name: 'platformInfo',
  standalone: true,
})
export class PlatformInfoPipe implements PipeTransform {
  transform(platforms: readonly PlatformType[]): {
    name: string
    icon: string
  } {
    const info = {
      name: '',
      icon: '@tui.smartphone',
    }

    if (platforms.includes('cli')) {
      info.name = 'CLI'
      info.icon = '@tui.terminal'
    } else if (platforms.includes('desktop')) {
      info.name = 'Desktop/Laptop'
      info.icon = '@tui.monitor'
    } else if (platforms.includes('android')) {
      info.name = 'Android Device'
    } else if (platforms.includes('iphone')) {
      info.name = 'iPhone'
    } else if (platforms.includes('ipad')) {
      info.name = 'iPad'
    } else if (platforms.includes('ios')) {
      info.name = 'iOS Device'
    } else {
      info.name = 'Unknown Device'
    }

    return info
  }
}
