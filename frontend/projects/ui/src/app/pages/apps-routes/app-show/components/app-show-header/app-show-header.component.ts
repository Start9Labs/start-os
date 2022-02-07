import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageDataEntry } from '@start9labs/shared'

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
