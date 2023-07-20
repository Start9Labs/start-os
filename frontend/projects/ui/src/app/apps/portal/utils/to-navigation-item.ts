import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { NavigationItem } from '../components/navigation/navigation.service'

export function toNavigationItem({
  manifest,
  icon,
}: PackageDataEntry): NavigationItem {
  return {
    title: manifest.title,
    routerLink: `/portal/services/${manifest.id}`,
    icon,
  }
}
