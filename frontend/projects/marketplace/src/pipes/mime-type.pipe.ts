import { NgModule, Pipe, PipeTransform } from '@angular/core'
import { MarketplacePkg } from '../types'

@Pipe({
  name: 'mimeType',
})
export class MimeTypePipe implements PipeTransform {
  transform(pkg: MarketplacePkg): string {
    switch (pkg.manifest.assets.icon.split('.')[1]) {
      case 'png':
        return `data:image/png;base64,${pkg.icon}`
      case 'jpeg':
      case 'jpg':
        return `data:image/jpeg;base64,${pkg.icon}`
        break
      case 'gif':
        return `data:image/gif;base64,${pkg.icon}`
        break
      case 'svg':
        return `data:image/svg+xml;base64,${pkg.icon}`
      default:
        return `data:image/png;base64,${pkg.icon}`
    }
  }
}

@NgModule({
  declarations: [MimeTypePipe],
  exports: [MimeTypePipe],
})
export class MimeTypePipeModule {}
