import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import { Router, RouterLink } from '@angular/router'
import {
  AboutModule,
  AdditionalModule,
  MarketplacePkg,
  PackageModule,
} from '@start9labs/marketplace'
import {
  Emver,
  ErrorService,
  LoadingService,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiAlertService } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'

import { NavigationService } from '../../../services/navigation.service'
import { SideloadDependenciesComponent } from './dependencies.component'

@Component({
  selector: 'sideload-package',
  template: `
    <ng-content></ng-content>
    <marketplace-package *tuiLet="button$ | async as button" [pkg]="package">
      <a
        *ngIf="button !== null && button !== 'Install'"
        tuiButton
        appearance="secondary"
        [routerLink]="'/portal/service/' + package.manifest.id"
      >
        View installed
      </a>
      <button *ngIf="button" tuiButton (click)="upload()">
        {{ button }}
      </button>
    </marketplace-package>
    <marketplace-about [pkg]="package"></marketplace-about>
    <sideload-dependencies
      *ngIf="!(package.manifest.dependencies | empty)"
      [package]="package"
    ></sideload-dependencies>
    <marketplace-additional [pkg]="package"></marketplace-additional>
  `,
  standalone: true,
  imports: [
    CommonModule,
    RouterLink,
    SharedPipesModule,
    AboutModule,
    AdditionalModule,
    PackageModule,
    TuiButtonModule,
    TuiLetModule,
    SideloadDependenciesComponent,
  ],
})
export class SideloadPackageComponent {
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)
  private readonly alerts = inject(TuiAlertService)
  private readonly emver = inject(Emver)

  readonly button$ = combineLatest([
    inject(ClientStorageService).showDevTools$,
    inject(PatchDB<DataModel>)
      .watch$('package-data')
      .pipe(
        map(local =>
          local[this.package.manifest.id]
            ? this.emver.compare(
                local[this.package.manifest.id].manifest.version,
                this.package.manifest.version,
              )
            : null,
        ),
      ),
  ]).pipe(
    map(([devtools, version]) => {
      switch (version) {
        case null:
          return 'Install'
        case 1:
          return 'Update'
        case -1:
          return devtools ? 'Downgrade' : ''
        default:
          return ''
      }
    }),
  )

  @Input({ required: true })
  package!: MarketplacePkg

  @Input({ required: true })
  file!: File

  async upload() {
    const loader = this.loader.open('Uploading package').subscribe()
    const { manifest, icon } = this.package
    const { size } = this.file

    try {
      const pkg = await this.api.sideloadPackage({ manifest, icon, size })

      await this.api.uploadPackage(pkg, this.file)
      await this.router.navigate(['/portal/service', manifest.id])

      this.navigation.removeTab('/portal/system/sideload')
      this.alerts
        .open('Package uploaded successfully', { status: 'success' })
        .subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
