import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { NavigationService } from '../../components/navigation/navigation.service'
import { ServicesService } from './services.service'

@Component({
  templateUrl: 'services.component.html',
  styleUrls: ['services.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ServicesComponent {
  private readonly navigation = inject(NavigationService)

  readonly services$ = inject(ServicesService)

  onClick({ manifest, icon }: PackageDataEntry) {
    this.navigation.addTab({
      title: manifest.title,
      routerLink: ['services', manifest.id].join('/'),
      icon,
    })
  }
}
