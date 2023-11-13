import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { MarketplacePkg } from '@start9labs/marketplace'
import { TuiLinkModule, TuiWrapperModule } from '@taiga-ui/core'
import { TuiAvatarModule, TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiInputFilesModule,
  tuiInputFilesOptionsProvider,
} from '@taiga-ui/kit'
import { Subject } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'

import { parseS9pk, validateS9pk } from './sideload.utils'
import { SideloadPackageComponent } from './package.component'

@Component({
  template: `
    <ng-container *ngIf="refresh$ | async"></ng-container>
    <sideload-package
      *ngIf="package && file; else upload"
      [package]="package"
      [file]="file"
    >
      <button
        tuiIconButton
        appearance="secondary"
        iconLeft="tuiIconXLarge"
        [style.border-radius.%]="100"
        [style.float]="'right'"
        (click)="clear()"
      >
        Close
      </button>
    </sideload-package>
    <ng-template #upload>
      <tui-input-files
        [ngModel]="null"
        (ngModelChange)="onFile($event)"
        (click)="clear()"
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
    </ng-template>
  `,
  host: { class: 'g-page', '[style.padding-top.rem]': '2' },
  styles: [
    `
      tui-input-files {
        height: 100%;
        max-width: 40rem;
        margin: 0 auto;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
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
    SideloadPackageComponent,
  ],
})
export class SideloadComponent {
  readonly refresh$ = new Subject<void>()
  readonly isTor = inject(ConfigService).isTor()

  invalid = false
  file: File | null = null
  package: MarketplacePkg | null = null

  clear() {
    this.invalid = false
    this.file = null
    this.package = null
  }

  async onFile(file: File | null) {
    if (!file || !(await validateS9pk(file))) {
      this.invalid = true
    } else {
      this.package = await parseS9pk(file)
      this.file = file
    }

    this.refresh$.next()
  }
}
