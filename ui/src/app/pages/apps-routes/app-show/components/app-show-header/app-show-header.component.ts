import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-header',
  templateUrl: './app-show-header.component.html',
  styleUrls: ['./app-show-header.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowHeaderComponent {
  @Input()
  pkg: PackageDataEntry
}
