import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'platformInfo',
  standalone: true,
})
export class PlatformInfoPipe implements PipeTransform {
  transform(userAgent: string): {
    name: string
    icon: string
  } {
    const info = {
      name: 'CLI',
      icon: '@tui.terminal',
    }

    // @TODO Alex we need to parse the userAgent to determine the platform

    // if (platforms.includes('desktop')) {
    //   info.name = 'Desktop/Laptop'
    //   info.icon = '@tui.monitor'
    // } else if (platforms.includes('android')) {
    //   info.name = 'Android Device'
    // } else if (platforms.includes('iphone')) {
    //   info.name = 'iPhone'
    // } else if (platforms.includes('ipad')) {
    //   info.name = 'iPad'
    // } else if (platforms.includes('ios')) {
    //   info.name = 'iOS Device'
    // } else {
    //   info.name = 'Unknown Device'
    // }

    return info
  }
}
