import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { NavigationItem } from '../components/navigation/navigation.service'
import { toNavigationItem } from '../utils/to-navigation-item'

@Pipe({
  name: 'toNavigationItem',
  standalone: true,
})
export class ToNavigationItemPipe implements PipeTransform {
  transform(service: PackageDataEntry): NavigationItem {
    return toNavigationItem(service)
  }
}
