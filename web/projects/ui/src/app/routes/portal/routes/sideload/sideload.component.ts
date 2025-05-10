import {
  ChangeDetectionStrategy,
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
import { MarketplacePkgSideload, validateS9pk } from './sideload.utils'
import { i18nKey, i18nPipe } from '@start9labs/shared'

@Component({
  template: `
    <ng-container *title>{{ 'Sideload' | i18n }}</ng-container>
    @if (file && package(); as pkg) {
      <sideload-package [pkg]="pkg" [file]="file!">
        <button
          tuiIconButton
          appearance="neutral"
          iconStart="@tui.x"
          size="s"
          [style.border-radius.%]="100"
          [style.justify-self]="'end'"
          (click)="clear()"
        >
          {{ 'Close' | i18n }}
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
          @if (error(); as err) {
            <div>
              <tui-avatar appearance="secondary" src="@tui.circle-x" />
              <p class="g-negative">{{ err | i18n }}</p>
              <button tuiButton>{{ 'Try again' | i18n }}</button>
            </div>
          } @else {
            <div>
              <tui-avatar appearance="secondary" src="@tui.cloud-upload" />
              <p>{{ 'Upload .s9pk package file' | i18n }}</p>
              @if (isTor) {
                <p class="g-warning">
                  {{
                    'Warning: package upload will be slow over Tor. Switch to local for a better experience.'
                      | i18n
                  }}
                </p>
              }
              <button tuiButton>{{ 'Upload' | i18n }}</button>
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
    i18nPipe,
  ],
})
export default class SideloadComponent {
  readonly isTor = inject(ConfigService).isTor()

  file: File | null = null
  readonly package = signal<MarketplacePkgSideload | null>(null)
  readonly error = signal<i18nKey | null>(null)

  clear() {
    this.file = null
    this.package.set(null)
    this.error.set(null)
  }

  async onFile(file: File | null) {
    this.file = file

    const parsed = file ? await validateS9pk(file) : ''

    this.package.set(tuiIsString(parsed) ? null : parsed)
    this.error.set(tuiIsString(parsed) ? (parsed as i18nKey) : null)
  }
}
