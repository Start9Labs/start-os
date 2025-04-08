import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { tuiIsString } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import {
  TuiAvatar,
  TuiFiles,
  tuiInputFilesOptionsProvider,
} from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'
import { TitleDirective } from 'src/app/services/title.service'
import { SideloadPackageComponent } from './package.component'
import { validateS9pk } from './sideload.utils'
import { MarketplacePkgBase } from '@start9labs/marketplace'

@Component({
  template: `
    <ng-container *title>Sideload</ng-container>
    @if (file && package(); as pkg) {
      <sideload-package [pkg]="pkg" [file]="file!">
        <button
          tuiIconButton
          appearance="neutral"
          iconStart="@tui.x"
          [style.border-radius.%]="100"
          [style.justify-self]="'end'"
          (click)="clear()"
        >
          Close
        </button>
      </sideload-package>
    } @else {
      <label tuiInputFiles (click)="clear()">
        <input
          tuiInputFiles
          accept=".s9pk"
          [ngModel]="null"
          (ngModelChange)="onFile($event)"
        />
        <ng-template>
          @if (error()) {
            <div>
              <tui-avatar appearance="secondary" src="@tui.circle-x" />
              <p class="g-negative">{{ error() }}</p>
              <button tuiButton>Try again</button>
            </div>
          } @else {
            <div>
              <tui-avatar appearance="secondary" src="@tui.cloud-upload" />
              <p>Upload .s9pk package file</p>
              @if (isTor) {
                <p class="g-warning">
                  Warning: package upload will be slow over Tor. Switch to local
                  for a better experience.
                </p>
              }
              <button tuiButton>Upload</button>
            </div>
          }
        </ng-template>
      </label>
    }
  `,
  host: { class: 'g-page', '[style.padding-top.rem]': '2' },
  styles: [
    `
      label {
        height: 100%;
        max-width: 42rem;
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
    FormsModule,
    TuiFiles,
    TuiAvatar,
    TuiButton,
    SideloadPackageComponent,
    TitleDirective,
  ],
})
export default class SideloadComponent {
  private readonly cdr = inject(ChangeDetectorRef)
  readonly isTor = inject(ConfigService).isTor()

  file: File | null = null
  readonly package = signal<MarketplacePkgBase | null>(null)
  readonly error = signal('')

  clear() {
    this.file = null
    this.package.set(null)
    this.error.set('')
  }

  async onFile(file: File | null) {
    const parsed = file ? await validateS9pk(file) : ''
    this.file = file
    this.package.set(tuiIsString(parsed) ? null : parsed)
    this.error.set(tuiIsString(parsed) ? parsed : '')
    // @TODO Alex figure out why it is needed even though we use signals
    this.cdr.markForCheck()
  }
}
