import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import {
  InstallProgress,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ProgressData } from 'src/app/util/package-loading-progress'

@Component({
  selector: 'app-show-progress',
  templateUrl: './app-show-progress.component.html',
  styleUrls: ['./app-show-progress.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowProgressComponent {
  @Input()
  pkg: PackageDataEntry

  @Input()
  installProgress: ProgressData

  get unpackingBuffer(): number {
    return this.installProgress.validateProgress === 100 &&
      !this.installProgress.unpackProgress
      ? 0
      : 1
  }

  get validationBuffer(): number {
    return this.installProgress.downloadProgress === 100 &&
      !this.installProgress.validateProgress
      ? 0
      : 1
  }

  getColor(action: keyof InstallProgress): string {
    return this.pkg['install-progress'][action] ? 'success' : 'secondary'
  }
}
