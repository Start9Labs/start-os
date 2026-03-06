import { KeyValuePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  ErrorService,
  getPkgId,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB, T } from '@start9labs/start-sdk'
import { TuiCell } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom, map } from 'rxjs'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StandardActionsService } from 'src/app/services/standard-actions.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceActionComponent } from '../components/action.component'
import {
  ALLOWED_STATUSES,
  BaseStatus,
  getInstalledBaseStatus,
  INACTIVE_STATUSES,
} from 'src/app/services/pkg-status-rendering.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

@Component({
  template: `
    @if (package(); as pkg) {
      @let inactive = isInactive();

      @for (group of pkg.actions | keyvalue; track $index) {
        <section class="g-card">
          <header>{{ group.key }}</header>
          @for (a of group.value; track $index) {
            <button
              tuiCell
              [action]="a"
              [inactive]="inactive"
              (click)="handle(pkg.status, pkg.icon, pkg.manifest, a)"
            ></button>
          }
        </section>
      }

      <section class="g-card">
        <header>StartOS</header>
        <button
          tuiCell
          [action]="outboundGatewayAction()"
          [inactive]="inactive"
          (click)="openOutboundGatewayModal()"
        ></button>
        <button
          tuiCell
          [action]="rebuild"
          [inactive]="inactive"
          (click)="service.rebuild(pkg.manifest.id)"
        ></button>
        <button
          tuiCell
          [action]="uninstall"
          [inactive]="inactive"
          (click)="service.uninstall(pkg.manifest)"
        ></button>
      </section>
    }
  `,
  styles: `
    section {
      max-width: 42rem;
      display: flex;
      flex-direction: column;
      margin-bottom: 2rem;
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ServiceActionComponent, TuiCell, KeyValuePipe],
})
export default class ServiceActionsRoute {
  private readonly actions = inject(ActionService)
  private readonly i18n = inject(i18nPipe)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly formDialog = inject(FormDialogService)
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)

  ungrouped: 'General' | 'Other' = 'General'

  readonly service = inject(StandardActionsService)
  private readonly gateways = toSignal(
    this.patch.watch$('serverInfo', 'network', 'gateways'),
  )

  readonly package = toSignal(
    this.patch.watch$('packageData', getPkgId()).pipe(
      map(pkg => {
        const specialGroup = Object.values(pkg.actions).some(a => !!a.group)
          ? 'Other'
          : 'General'
        const status = getInstalledBaseStatus(pkg.statusInfo)
        return {
          status,
          icon: pkg.icon,
          manifest: getManifest(pkg),
          outboundGateway: pkg.outboundGateway,
          actions: Object.entries(pkg.actions)
            .filter(([_, action]) => action.visibility !== 'hidden')
            .map(([id, action]) => ({
              ...action,
              id,
              group: action.group || specialGroup,
              visibility: ALLOWED_STATUSES[action.allowedStatuses].has(status)
                ? action.visibility
                : ({
                    disabled: `${this.i18n.transform('Action can only be executed when service is')} ${this.i18n.transform(action.allowedStatuses === 'only-running' ? 'Running' : 'Stopped')?.toLowerCase()}`,
                  } as T.ActionVisibility),
            }))
            .sort((a, b) => {
              if (a.group === specialGroup && b.group !== specialGroup) return 1
              if (b.group === specialGroup && a.group !== specialGroup)
                return -1

              const groupCompare = a.group.localeCompare(b.group) // sort groups lexicographically
              if (groupCompare !== 0) return groupCompare

              return a.id.localeCompare(b.id) // sort actions within groups lexicographically
            })
            .reduce<
              Record<
                string,
                Array<T.ActionMetadata & { id: string; group: string }>
              >
            >((acc, action) => {
              const key = action.group
              if (!acc[key]) {
                acc[key] = []
              }
              acc[key].push(action)
              return acc
            }, {}),
        }
      }),
    ),
  )

  readonly outboundGatewayAction = computed(() => {
    const pkg = this.package()
    const gatewayId = pkg?.outboundGateway
    const gateways = this.gateways()
    const gatewayName =
      gatewayId && gateways?.[gatewayId]
        ? (gateways[gatewayId].name ??
          gateways[gatewayId].ipInfo?.name ??
          gatewayId)
        : null
    return {
      name: this.i18n.transform('Set Outbound Gateway')!,
      description: gatewayName
        ? `${this.i18n.transform('Current')}: ${gatewayName}`
        : `${this.i18n.transform('Current')}: ${this.i18n.transform('System')}`,
    }
  })

  readonly rebuild = {
    name: this.i18n.transform('Rebuild Service')!,
    description: this.i18n.transform(
      'Rebuilds the service container. Only necessary if there is a bug in StartOS',
    )!,
  }

  readonly uninstall = {
    name: this.i18n.transform('Uninstall')!,
    description: this.i18n.transform(
      'Uninstalls this service from StartOS and deletes all data permanently.',
    )!,
  }

  handle(
    status: BaseStatus,
    icon: string,
    { id, title }: T.Manifest,
    action: T.ActionMetadata & { id: string },
  ) {
    this.actions.present({
      pkgInfo: { id, title, icon, status },
      actionInfo: { id: action.id, metadata: action },
    })
  }

  async openOutboundGatewayModal() {
    const pkg = this.package()
    if (!pkg) return

    const gateways = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network', 'gateways'),
    )

    const SYSTEM_KEY = 'system'

    const options: Record<string, string> = {
      [SYSTEM_KEY]: this.i18n.transform('System default')!,
    }

    Object.entries(gateways)
      .filter(
        ([_, g]) =>
          !!g.ipInfo &&
          g.ipInfo.deviceType !== 'bridge' &&
          g.ipInfo.deviceType !== 'loopback',
      )
      .forEach(([id, g]) => {
        options[id] = g.name ?? g.ipInfo?.name ?? id
      })

    const spec = ISB.InputSpec.of({
      gateway: ISB.Value.select({
        name: this.i18n.transform('Outbound Gateway'),
        description: this.i18n.transform(
          'Select the gateway for outbound traffic',
        ),
        default: pkg.outboundGateway ?? SYSTEM_KEY,
        values: options,
      }),
    })

    this.formDialog.open(FormComponent, {
      label: 'Set Outbound Gateway',
      data: {
        spec: await configBuilderToSpec(spec),
        buttons: [
          {
            text: this.i18n.transform('Save'),
            handler: async (input: typeof spec._TYPE) => {
              const loader = this.loader.open('Saving').subscribe()

              try {
                await this.api.setServiceOutbound({
                  package: pkg.manifest.id,
                  gateway: input.gateway === SYSTEM_KEY ? null : input.gateway,
                })
                return true
              } catch (e: any) {
                this.errorService.handleError(e)
                return false
              } finally {
                loader.unsubscribe()
              }
            },
          },
        ],
      },
    })
  }

  protected readonly isInactive = computed(
    (pkg = this.package()) => !pkg || INACTIVE_STATUSES.includes(pkg.status),
  )
}
