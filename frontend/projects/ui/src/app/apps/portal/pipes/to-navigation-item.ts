import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { NavigationItem } from '../types/navigation-item'
import { toNavigationItem } from '../utils/to-navigation-item'

@Pipe({
  name: 'toNavigationItem',
  standalone: true,
})
export class ToNavigationItemPipe implements PipeTransform {
  transform(
    packages: Record<string, PackageDataEntry>,
    id: string,
  ): NavigationItem | null {
    return id ? toNavigationItem(id, packages) : null
  }
}
