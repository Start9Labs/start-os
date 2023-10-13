import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { NavigationItem } from '../types/navigation-item'
import { toDesktopItem } from '../utils/to-desktop-item'

@Pipe({
  name: 'toDesktopItem',
  standalone: true,
})
export class ToDesktopItemPipe implements PipeTransform {
  transform(
    packages: Record<string, PackageDataEntry>,
    id: string,
  ): NavigationItem | null {
    return id ? toDesktopItem(id, packages) : null
  }
}
