import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  TuiAlertService,
  TuiLinkModule,
  TuiWrapperModule,
} from '@taiga-ui/core'
import { TuiAvatarModule, TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiInputFilesModule,
  tuiInputFilesOptionsProvider,
} from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'

import { NavigationService } from '../../../services/navigation.service'
import { toDesktopItem } from '../../../utils/to-desktop-item'
import { parseS9pk, validateS9pk } from './sideload.utils'

@Component({
  template: `
    <tui-input-files
      [style.height.%]="100"
      [ngModel]="null"
      (ngModelChange)="upload($event)"
      (pointerdown)="invalid = false"
    >
      <input tuiInputFiles accept=".s9pk" />
      <ng-template>
        <div *ngIf="invalid; else valid">
          <tui-avatar
            tuiWrapper
            appearance="secondary"
            src="tuiIconXCircleLarge"
          ></tui-avatar>
          <p [style.color]="'var(--tui-negative)'">Invalid package file</p>
          <button tuiButton>Try again</button>
        </div>
        <ng-template #valid>
          <div>
            <tui-avatar
              tuiWrapper
              appearance="secondary"
              src="tuiIconUploadCloudLarge"
            ></tui-avatar>
            <p>Upload .s9pk package file</p>
            <p *ngIf="isTor" [style.color]="'var(--tui-positive)'">
              Tip: switch to LAN for faster uploads
            </p>
            <button tuiButton>Upload</button>
          </div>
        </ng-template>
      </ng-template>
    </tui-input-files>
  `,
  host: { class: 'g-page', '[style.padding-top.rem]': '2' },
  providers: [tuiInputFilesOptionsProvider({ maxFileSize: Infinity })],
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiInputFilesModule,
    TuiLinkModule,
    TuiAvatarModule,
    TuiWrapperModule,
    TuiButtonModule,
  ],
})
export class SideloadComponent {
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)
  private readonly alerts = inject(TuiAlertService)

  readonly isTor = inject(ConfigService).isTor()

  invalid = false

  async upload(file: File | null) {
    if (!file || !(await validateS9pk(file))) {
      this.invalid = true

      return
    }

    const loader = this.loader.open('Uploading package').subscribe()
    const { manifest, icon } = await parseS9pk(file)
    const { size } = file

    try {
      const pkg = await this.api.sideloadPackage({ manifest, icon, size })

      await this.api.uploadPackage(pkg, file)
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
