import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { SYSTEM_UTILITIES } from '../components/drawer/drawer.const'
import { NavigationItem } from '../components/navigation/navigation.service'
import { toRouterLink } from '../utils/to-router-link'

@Pipe({
  name: 'toDesktopItem',
  standalone: true,
})
export class ToDesktopItemPipe implements PipeTransform {
  transform(
    packages: Record<string, PackageDataEntry>,
    id: string,
  ): NavigationItem | null {
    if (!id) return null

    const item = SYSTEM_UTILITIES[id]
    const routerLink = toRouterLink(id)

    if (SYSTEM_UTILITIES[id]) {
      return {
        icon: item.icon,
        title: item.title,
        routerLink,
      }
    }

    return {
      icon: packages[id]?.icon,
      title: packages[id]?.manifest.title,
      routerLink,
    }
  }
}
