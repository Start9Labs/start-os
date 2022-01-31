import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PkgInfo } from 'src/app/util/get-package-info'

@Component({
  selector: 'app-list-icon',
  templateUrl: 'app-list-icon.component.html',
  styleUrls: ['app-list-icon.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListIconComponent {
  @Input()
  pkg: PkgInfo

  @Input()
  connectionFailure = false

  get color (): string {
    return this.connectionFailure
      ? 'var(--ion-color-dark)'
      : 'var(--ion-color-' + this.pkg.primaryRendering.color + ')'
  }
}
