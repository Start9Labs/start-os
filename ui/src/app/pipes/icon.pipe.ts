import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'iconParse',
})
export class IconPipe implements PipeTransform {
  transform (iconUrl: string): string {
    iconUrl = iconUrl || ''
    if (iconUrl.startsWith('/')) return '/api' + iconUrl
    return iconUrl
  }
}
