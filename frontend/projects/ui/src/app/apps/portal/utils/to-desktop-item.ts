import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { SYSTEM_UTILITIES } from '../constants/system-utilities'
import { NavigationItem } from '../types/navigation-item'
import { toRouterLink } from './to-router-link'

export function toDesktopItem(
  id: string,
  packages: Record<string, PackageDataEntry> = {},
): NavigationItem {
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
