import { ChangeDetectionStrategy, Component } from '@angular/core'
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
}
