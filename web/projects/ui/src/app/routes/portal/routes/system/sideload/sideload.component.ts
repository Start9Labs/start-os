import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { MarketplacePkg } from '@start9labs/marketplace'
import { TuiButton, TuiLink } from '@taiga-ui/core'
import {
  TuiAvatar,
  TuiFiles,
  tuiInputFilesOptionsProvider,
} from '@taiga-ui/kit'
import { Subject } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
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
        iconStart="@tui.x"
        [style.border-radius.%]="100"
        [style.float]="'right'"
        (click)="clear()"
        class="justify-self-end"
      >
        Close
      </button>
    </sideload-package>
    <ng-template #upload>
      <label tuiInputFiles (click)="clear()">
        <input
          tuiInputFiles
          accept=".s9pk"
          [ngModel]="null"
          (ngModelChange)="onFile($event)"
        />
        <ng-template>
          <div *ngIf="invalid; else valid">
            <tui-avatar appearance="secondary" src="@tui.circle-x" />
            <p [style.color]="'var(--tui-text-negative)'">
              Invalid package file
            </p>
            <button tuiButton>Try again</button>
          </div>
          <ng-template #valid>
            <div>
              <tui-avatar appearance="secondary" src="@tui.cloud-upload" />
              <p>Upload .s9pk package file</p>
              <p *ngIf="isTor" [style.color]="'var(--tui-text-positive)'">
                Tip: switch to LAN for faster uploads
              </p>
              <button tuiButton>Upload</button>
            </div>
          </ng-template>
        </ng-template>
      </label>
    </ng-template>
  `,
  host: { class: 'g-page', '[style.padding-top.rem]': '2' },
  styles: [
    `
      label {
        height: 100%;
        max-width: 40rem;
        margin: 0 auto;
      }

      button {
        margin-bottom: 2rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiInputFilesOptionsProvider({ maxFileSize: Infinity })],
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiFiles,
    TuiLink,
    TuiAvatar,
    TuiButton,
    SideloadPackageComponent,
  ],
})
export default class SideloadComponent {
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

  // @TODO Alex refactor sideload
  async onFile(file: File | null) {
    // if (!file || !(await validateS9pk(file))) {
    //   this.invalid = true
    // } else {
    //   this.package = await parseS9pk(file)
    //   this.file = file
    // }

    this.refresh$.next()
  }
}
