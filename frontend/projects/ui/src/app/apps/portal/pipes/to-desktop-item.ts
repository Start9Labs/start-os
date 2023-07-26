import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { SYSTEM_UTILITIES } from '../components/drawer/drawer.const'
import { NavigationItem } from '../components/navigation/navigation.service'

@Pipe({
  name: 'toDesktopItem',
  standalone: true,
})
export class ToDesktopItemPipe implements PipeTransform {
  private readonly system = SYSTEM_UTILITIES

  transform(
    packages: Record<string, PackageDataEntry>,
    id: string,
  ): NavigationItem {
    const item = SYSTEM_UTILITIES[id]

    if (SYSTEM_UTILITIES[id]) {
      return {
        icon: item.icon,
        title: item.title,
        routerLink: id,
      }
    }

    return {
      icon: packages[id].icon,
      title: packages[id].manifest.title,
      routerLink: `/portal/services/${id}`,
    }
  }
}
