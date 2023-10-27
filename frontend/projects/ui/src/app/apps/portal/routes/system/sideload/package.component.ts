import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import { Router } from '@angular/router'
import {
  AboutModule,
  AdditionalModule,
  DependenciesModule,
  MarketplacePkg,
  PackageModule,
} from '@start9labs/marketplace'
import {
  ErrorService,
  LoadingService,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiAlertService } from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'

import { toDesktopItem } from '../../../utils/to-desktop-item'
import { NavigationService } from '../../../services/navigation.service'

@Component({
  selector: 'sideload-package',
  template: `
    <ng-content></ng-content>
    <marketplace-package [pkg]="package">
      <button tuiButton (click)="upload()">Install</button>
    </marketplace-package>
    <marketplace-about [pkg]="package"></marketplace-about>
    <marketplace-dependencies
      *ngIf="!(package.manifest.dependencies | empty)"
      [pkg]="package"
    ></marketplace-dependencies>
    <marketplace-additional [pkg]="package"></marketplace-additional>
  `,
  standalone: true,
  imports: [
    CommonModule,
    SharedPipesModule,
    AboutModule,
    AdditionalModule,
    DependenciesModule,
    PackageModule,
    TuiButtonModule,
  ],
})
export class SideloadPackageComponent {
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)
  private readonly alerts = inject(TuiAlertService)

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
      await this.router.navigate(['/portal/desktop'])

      this.navigation.removeTab(toDesktopItem('/portal/system/sideload'))
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
