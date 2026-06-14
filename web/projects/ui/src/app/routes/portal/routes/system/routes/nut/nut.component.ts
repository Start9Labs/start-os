import {
  ChangeDetectionStrategy,
  Component,
  DestroyRef,
  computed,
  inject,
  signal,
} from '@angular/core'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  ErrorService,
  getErrorMessage,
  i18nKey,
  i18nPipe,
} from '@start9labs/shared'
import { ISB, T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiLoader,
  TuiNotificationService,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiButtonLoading, TuiNotificationMiddleService } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { switchMap } from 'rxjs'
import { FormGroupComponent } from 'src/app/routes/portal/components/form/containers/group.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormService } from 'src/app/services/form.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

type NutForm = {
  config:
    | { selection: 'disabled'; value: Record<string, never> }
    | {
        selection: 'server'
        value: {
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
      }
    | {
        selection: 'client'
        value: {
          upsName: string
          host: string
          port: number
          monitorUsername: string
          monitorPassword: string | null
          shutdownDelay: number
        }
      }
}

@Component({
  template: `
    <ng-container *title>
      <div>
        <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
          {{ 'Back' | i18n }}
        </a>
        {{ 'Network UPS Tools' | i18n }}
      </div>
    </ng-container>

    @if (data(); as data) {
      <form [formGroup]="data.form">
        <header tuiHeader="body-l">
          <h3 tuiTitle>
            <b>{{ 'Network UPS Tools' | i18n }}</b>
          </h3>
        </header>

        <form-group [spec]="data.spec" />

        <footer>
          @if (!data.form.pristine) {
            <button
              tuiButton
              type="button"
              size="l"
              appearance="secondary"
              (click)="cancel(data)"
            >
              {{ 'Cancel' | i18n }}
            </button>
          }
          <button
            tuiButton
            type="button"
            size="l"
            [disabled]="data.form.invalid || data.form.pristine"
            (click)="save(data.form.value)"
          >
            {{ 'Save' | i18n }}
          </button>
        </footer>
      </form>

      @if (data.config.mode !== 'disabled') {
        <section class="status-section">
          <header tuiHeader="body-l">
            <h3 tuiTitle>
              <b>{{ 'UPS Status' | i18n }}</b>
              <span tuiSubtitle>
                {{ status()?.target || target(data.config) }}
              </span>
            </h3>
            <button
              tuiButton
              type="button"
              iconStart="@tui.refresh-cw"
              [loading]="refreshing()"
              (click)="refreshStatus(true, data.config)"
            >
              {{ 'Refresh' | i18n }}
            </button>
          </header>

          @if (status(); as current) {
            <div class="status-table" role="table">
              <div class="status-row status-heading" role="row">
                <span role="columnheader">{{ 'Name' | i18n }}</span>
                <span role="columnheader">{{ 'Value' | i18n }}</span>
              </div>
              @for (row of statusRows(); track row.name) {
                <div class="status-row" role="row">
                  <code role="cell">{{ row.name }}</code>
                  <span role="cell">{{ row.value }}</span>
                </div>
              }
            </div>
          } @else if (refreshing()) {
            <tui-loader [style.height.rem]="5" />
          } @else {
            <app-placeholder icon="@tui.zap">
              {{ 'No UPS data available' | i18n }}
            </app-placeholder>
          }

          @if (statusError(); as error) {
            <p class="status-error">{{ error }}</p>
          }
        </section>
      }
    }
  `,
  styles: `
    :host {
      max-width: 36rem;
    }

    :host-context(tui-root._mobile) form:first-of-type > [tuiHeader] {
      display: none;
    }

    form header,
    form footer,
    .status-section header {
      margin: 1rem 0;
      display: flex;
      gap: 1rem;
    }

    .status-section header {
      align-items: flex-start;
      justify-content: space-between;
    }

    footer {
      justify-content: flex-end;
    }

    .status-section {
      margin-block-start: 1rem;
    }

    .status-table {
      border: 1px solid var(--tui-border-normal);
      border-radius: 0.5rem;
      overflow: hidden;
    }

    .status-row {
      display: grid;
      grid-template-columns: minmax(10rem, 1fr) minmax(0, 2fr);
      gap: 1rem;
      padding: 0.75rem 1rem;
      border-block-start: 1px solid var(--tui-border-normal);
      align-items: start;
    }

    .status-row:first-child {
      border-block-start: none;
    }

    .status-heading {
      background: var(--tui-background-neutral-1);
      color: var(--tui-text-secondary);
      font: var(--tui-typography-body-s);
      font-weight: bold;
    }

    code,
    .status-row span {
      overflow-wrap: anywhere;
    }

    .status-error {
      color: var(--tui-status-negative);
      margin: 1rem 0 0;
    }

    :host-context(tui-root._mobile) {
      .status-section header {
        align-items: stretch;
        flex-direction: column;
      }

      .status-row {
        grid-template-columns: 1fr;
        gap: 0.25rem;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    FormGroupComponent,
    RouterLink,
    TitleDirective,
    i18nPipe,
    TuiButton,
    TuiButtonLoading,
    TuiHeader,
    TuiLoader,
    TuiTitle,
    PlaceholderComponent,
  ],
})
export default class SystemNutComponent {
  private readonly destroyRef = inject(DestroyRef)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly formService = inject(FormService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

  readonly status = signal<T.NutStatus | null>(null)
  readonly statusError = signal<string | null>(null)
  readonly refreshing = signal(false)
  readonly statusRows = computed(() =>
    Object.entries(this.status()?.variables ?? {}).map(([name, value]) => ({
      name,
      value,
    })),
  )

  readonly data = toSignal(
    this.patch.watch$('serverInfo', 'nut').pipe(
      switchMap(async config => {
        const savedConfig = config ?? ({ mode: 'disabled' } as T.NutConfig)
        const spec = await configBuilderToSpec(this.nutSpec())
        const formData = this.toNutForm(savedConfig)
        const form = this.formService.createForm(spec, formData)

        return { config: savedConfig, form, formData, spec }
      }),
    ),
  )

  constructor() {
    this.patch
      .watch$('serverInfo', 'nut')
      .pipe(takeUntilDestroyed(this.destroyRef))
      .subscribe(config => {
        this.status.set(null)
        this.statusError.set(null)

        if (config?.mode !== 'disabled') {
          void this.refreshStatus(false, config)
        }
      })
  }

  target(config: T.NutConfig): string {
    switch (config.mode) {
      case 'server':
        return `${config.upsName}@localhost:3493`
      case 'client':
        return `${config.upsName}@${config.host}:${config.port}`
      default:
        return ''
    }
  }

  async refreshStatus(notifyOnError: boolean, config = this.data()?.config) {
    if (!config || config.mode === 'disabled') {
      this.status.set(null)
      this.statusError.set(null)
      return
    }

    this.refreshing.set(true)

    try {
      const status = await this.api.getNutStatus({})
      this.status.set(status)
      this.statusError.set(null)
    } catch (e: any) {
      const error = getErrorMessage(e)
      this.status.set(null)
      this.statusError.set(error)

      if (notifyOnError) {
        this.showStatusAlert(error)
      }
    } finally {
      this.refreshing.set(false)
    }
  }

  cancel(data: {
    form: ReturnType<FormService['createForm']>
    formData: NutForm
  }) {
    data.form.reset(data.formData)
  }

  async save(value: NutForm): Promise<void> {
    const config = this.toNutConfig(value)
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.setNut({ config })

      if (config.mode === 'disabled') {
        this.status.set(null)
        this.statusError.set(null)
      } else {
        await this.refreshStatus(true, config)
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
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
      config: ISB.Value.union({
        name: this.i18n.transform('Mode'),
        default: 'disabled',
        description: this.i18n.transform(
          'NUT shuts down StartOS when the UPS battery is low.',
        ),
        variants: ISB.Variants.of({
          disabled: {
            name: this.i18n.transform('Disabled'),
            spec: ISB.InputSpec.of({}),
          },
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
    switch (config.mode) {
      case 'server':
        return {
          config: {
            selection: 'server',
            value: config,
          },
        }
      case 'client':
        return {
          config: {
            selection: 'client',
            value: config,
          },
        }
      default:
        return {
          config: {
            selection: 'disabled',
            value: {},
          },
        }
    }
  }

  private toNutConfig(value: NutForm): T.NutConfig {
    switch (value.config.selection) {
      case 'server':
        return {
          mode: 'server',
          ...value.config.value,
          monitorPassword: value.config.value.monitorPassword || '',
          remoteUsername: value.config.value.remoteUsername || null,
          remotePassword: value.config.value.remotePassword || null,
        }
      case 'client':
        return {
          mode: 'client',
          ...value.config.value,
          monitorPassword: value.config.value.monitorPassword || '',
        }
      default:
        return { mode: 'disabled' }
    }
  }
}
