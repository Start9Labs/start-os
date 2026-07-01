import { KeyValuePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { ErrorService, getErrorMessage, i18nPipe } from '@start9labs/shared'
import { ISB, T } from '@start9labs/start-core'
import {
  TuiButton,
  TuiCell,
  TuiIcon,
  TuiLoader,
  TuiNotificationService,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiButtonLoading,
  TuiNotificationMiddleService,
  TuiTooltip,
} from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

type ServerValue = {
  upsName: string
  driver: string
  port: string
  monitorUsername: string
  monitorPassword: string | null
  listenAll: boolean
  remoteUsername: string | null
  remotePassword: string | null
  shutdownDelay: number
}

type ClientValue = {
  upsName: string
  host: string
  port: number
  monitorUsername: string
  monitorPassword: string | null
  shutdownDelay: number
}

type NutForm = {
  enabled: boolean
  settings:
    | { selection: 'server'; value: ServerValue }
    | { selection: 'client'; value: ClientValue }
}

const DEFAULT_SERVER: ServerValue = {
  upsName: 'ups',
  driver: 'usbhid-ups',
  port: 'auto',
  monitorUsername: 'monuser',
  monitorPassword: null,
  listenAll: false,
  remoteUsername: null,
  remotePassword: null,
  shutdownDelay: 5,
}

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'Network UPS Tools' | i18n }}
    </ng-container>

    @if (data()) {
      <section class="g-card">
        <header>
          {{ 'Network UPS Tools' | i18n }}
          <tui-icon
            [tuiTooltip]="
              'NUT shuts down StartOS when the UPS battery is low' | i18n
            "
          />
          <button
            tuiIconButton
            size="xs"
            type="button"
            iconStart="@tui.settings"
            [style.margin-inline-start]="'auto'"
            (click)="configure()"
          >
            {{ 'Configure' | i18n }}
          </button>
          <button
            tuiIconButton
            size="xs"
            type="button"
            iconStart="@tui.refresh-cw"
            [loading]="refreshing()"
            (click)="refreshStatus()"
          >
            {{ 'Refresh' | i18n }}
          </button>
        </header>

        @if (status()?.target || target(); as url) {
          <div tuiCell>
            <div tuiTitle>
              <div tuiSubtitle>
                <code>Target</code>
              </div>
              <b>{{ url }}</b>
            </div>
          </div>
        }
        @if (status(); as current) {
          @for (row of current.variables | keyvalue; track $index) {
            <div tuiCell>
              <div tuiTitle>
                <div tuiSubtitle>
                  <code>{{ row.key }}</code>
                </div>
                <b>{{ row.value }}</b>
              </div>
            </div>
          }
        } @else if (refreshing()) {
          <tui-loader [style.height.rem]="5" />
        } @else {
          <app-placeholder icon="@tui.zap">
            {{ 'No UPS data available' | i18n }}
          </app-placeholder>
        }
      </section>
    }
  `,
  styles: `
    :host {
      max-width: 36rem;
    }

    form {
      padding: 0.75rem 0;
    }

    footer {
      display: flex;
      gap: 1rem;
      justify-content: flex-end;
      margin-block-start: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    RouterLink,
    TitleDirective,
    i18nPipe,
    TuiButton,
    TuiButtonLoading,
    TuiLoader,
    TuiTitle,
    PlaceholderComponent,
    KeyValuePipe,
    TuiCell,
    TuiIcon,
    TuiTooltip,
  ],
})
export default class SystemNutComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  readonly status = signal<T.NutStatus | null>(null)
  readonly refreshing = signal(false)

  readonly data = toSignal(
    this.patch
      .watch$('serverInfo', 'nut')
      .pipe(map(c => c ?? ({ enabled: false, settings: null } as T.NutConfig))),
  )

  constructor() {
    this.patch
      .watch$('serverInfo', 'nut')
      .pipe(takeUntilDestroyed())
      .subscribe(config => {
        this.status.set(null)

        if (config?.enabled && config.settings) {
          void this.refreshStatus(config)
        }
      })
  }

  target(config = this.data()): string {
    if (!config?.enabled || !config?.settings) {
      return ''
    }

    switch (config.settings.mode) {
      case 'server':
        return `${config.settings.upsName}@localhost:3493`
      case 'client':
        return `${config.settings.upsName}@${config.settings.host}:${config.settings.port}`
      default:
        return ''
    }
  }

  async refreshStatus(config = this.data()) {
    if (!config?.enabled || !config?.settings) {
      this.status.set(null)
      return
    }

    this.refreshing.set(true)

    try {
      const status = await this.api.getNutStatus({})
      this.status.set(status)
    } catch (e: any) {
      this.status.set(null)
      this.showStatusAlert(getErrorMessage(e))
    } finally {
      this.refreshing.set(false)
    }
  }

  async save(value: NutForm): Promise<boolean> {
    const config = this.toNutConfig(value)
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.setNut({ config })

      if (!config.enabled) {
        this.status.set(null)
      } else {
        await this.refreshStatus(config)
      }

      return true
    } catch (e: any) {
      this.errorService.handleError(e)

      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async configure() {
    this.formDialog.open(FormComponent, {
      label: this.i18n.transform('Network UPS Tools'),
      data: {
        spec: await configBuilderToSpec(this.nutSpec()),
        value: this.toNutForm(
          this.data() ?? { enabled: false, settings: null },
        ),
        buttons: [
          {
            text: this.i18n.transform('Save'),
            handler: async (value: NutForm) => this.save(value),
          },
        ],
      },
    })
  }

  private showStatusAlert(error: string) {
    const message =
      this.i18n.transform(
        'StartOS could not read UPS status with the current NUT configuration.',
      ) ??
      'StartOS could not read UPS status with the current NUT configuration.'
    const hint =
      this.i18n.transform(
        'The UPS did not return status data. Check the UPS name, host, port, credentials, and driver.',
      ) ??
      'The UPS did not return status data. Check the UPS name, host, port, credentials, and driver.'

    this.alerts
      .open(`${message}\n\n${hint}\n\n${error}`, {
        label: this.i18n.transform('UPS status unavailable'),
        appearance: 'negative',
      })
      .subscribe()
  }

  private nutSpec() {
    return ISB.InputSpec.of({
      enabled: ISB.Value.toggle({
        name: this.i18n.transform('Enabled'),
        default: false,
      }),
      settings: ISB.Value.union({
        name: this.i18n.transform('Mode'),
        default: 'server',
        variants: ISB.Variants.of({
          server: {
            name: this.i18n.transform('Direct UPS'),
            spec: ISB.InputSpec.of({
              upsName: ISB.Value.text({
                name: this.i18n.transform('UPS Name'),
                required: true,
                default: 'ups',
                placeholder: 'ups',
              }),
              driver: ISB.Value.text({
                name: this.i18n.transform('Driver'),
                required: true,
                default: 'usbhid-ups',
                placeholder: 'usbhid-ups',
              }),
              port: ISB.Value.text({
                name: this.i18n.transform('Device or address'),
                required: true,
                default: 'auto',
                placeholder: 'auto',
              }),
              monitorUsername: ISB.Value.text({
                name: this.i18n.transform('Monitor username'),
                required: true,
                default: 'monuser',
                placeholder: 'monuser',
              }),
              monitorPassword: ISB.Value.text({
                name: this.i18n.transform('Monitor password'),
                required: true,
                default: null,
                masked: true,
              }),
              listenAll: ISB.Value.toggle({
                name: this.i18n.transform('Allow network clients'),
                default: false,
              }),
              remoteUsername: ISB.Value.text({
                name: this.i18n.transform('Network client username'),
                required: false,
                default: null,
                placeholder: 'upsclient',
              }),
              remotePassword: ISB.Value.text({
                name: this.i18n.transform('Network client password'),
                required: false,
                default: null,
                masked: true,
              }),
              shutdownDelay: ISB.Value.number({
                name: this.i18n.transform('Shutdown delay'),
                required: true,
                default: 5,
                integer: true,
                min: 0,
                max: 300,
                units: this.i18n.transform('Seconds'),
              }),
            }),
          },
          client: {
            name: this.i18n.transform('Network UPS client'),
            spec: ISB.InputSpec.of({
              upsName: ISB.Value.text({
                name: this.i18n.transform('UPS Name'),
                required: true,
                default: 'ups',
                placeholder: 'ups',
              }),
              host: ISB.Value.text({
                name: this.i18n.transform('NUT server host'),
                required: true,
                default: null,
                placeholder: '192.168.1.10',
              }),
              port: ISB.Value.number({
                name: this.i18n.transform('NUT server port'),
                required: true,
                default: 3493,
                integer: true,
                min: 1,
                max: 65535,
              }),
              monitorUsername: ISB.Value.text({
                name: this.i18n.transform('Monitor username'),
                required: true,
                default: 'monuser',
                placeholder: 'monuser',
              }),
              monitorPassword: ISB.Value.text({
                name: this.i18n.transform('Monitor password'),
                required: true,
                default: null,
                masked: true,
              }),
              shutdownDelay: ISB.Value.number({
                name: this.i18n.transform('Shutdown delay'),
                required: true,
                default: 5,
                integer: true,
                min: 0,
                max: 300,
                units: this.i18n.transform('Seconds'),
              }),
            }),
          },
        }),
      }),
    })
  }

  private toNutForm(config: T.NutConfig): NutForm {
    return {
      enabled: config.enabled,
      settings: this.toSettingsForm(config.settings),
    }
  }

  private toSettingsForm(settings: T.NutSettings | null): NutForm['settings'] {
    switch (settings?.mode) {
      case 'client':
        return { selection: 'client', value: settings }
      case 'server':
        return { selection: 'server', value: settings }
      default:
        return { selection: 'server', value: DEFAULT_SERVER }
    }
  }

  private toNutConfig(value: NutForm): T.NutConfig {
    return {
      enabled: value.enabled,
      settings: this.toNutSettings(value.settings),
    }
  }

  private toNutSettings(settings: NutForm['settings']): T.NutSettings {
    switch (settings.selection) {
      case 'client':
        return {
          mode: 'client',
          ...settings.value,
          monitorPassword: settings.value.monitorPassword || '',
        }
      case 'server':
        return {
          mode: 'server',
          ...settings.value,
          monitorPassword: settings.value.monitorPassword || '',
          remoteUsername: settings.value.remoteUsername || null,
          remotePassword: settings.value.remotePassword || null,
        }
    }
  }
}
