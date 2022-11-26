import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { NavController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { tap } from 'rxjs/operators'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { DOCUMENT } from '@angular/common'
import { ConfigService } from 'src/app/services/config.service'

const STATES = [
  PackageState.Installing,
  PackageState.Updating,
  PackageState.Restoring,
]

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowPage {
  readonly secure = this.config.isSecure()

  private readonly pkgId = getPkgId(this.route)

  readonly pkg$ = this.patch.watch$('package-data', this.pkgId).pipe(
    tap(pkg => {
      // if package disappears, navigate to list page
      if (!pkg) this.navCtrl.navigateRoot('/services')
    }),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  isInstalled({ state }: PackageDataEntry): boolean {
    return state === PackageState.Installed
  }

  isRunning({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.Running
  }

  isBackingUp({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.BackingUp
  }

  showProgress({ state }: PackageDataEntry): boolean {
    return STATES.includes(state)
  }

  launchHttps() {
    window.open(this.document.location.href.replace('http', 'https'))
  }
}
