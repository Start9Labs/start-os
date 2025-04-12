import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute } from '@angular/router'
import { isEmptyObject } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { map, of } from 'rxjs'
import { UptimeComponent } from 'src/app/routes/portal/components/uptime.component'
import { ConnectionService } from 'src/app/services/connection.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getInstalledPrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { ServiceActionRequestsComponent } from '../components/action-requests.component'
import { ServiceControlsComponent } from '../components/controls.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceErrorComponent } from '../components/error.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceInterfacesComponent } from '../components/interfaces.component'
import { ServiceProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'

@Component({
  template: `
    <service-status
      [connected]="!!connected()"
      [installingInfo]="pkg()?.stateInfo?.installingInfo"
      [status]="status()"
    >
      @if ($any(pkg()?.status)?.started; as started) {
        <p class="g-secondary" [appUptime]="started"></p>
      }
      @if (installed() && connected() && pkg(); as pkg) {
        <service-controls [pkg]="pkg" [status]="status()" />
      }
    </service-status>

    @if (installed() && pkg(); as pkg) {
      @if (pkg.status.main === 'error') {
        <service-error [pkg]="pkg" />
      }

      <service-interfaces [pkg]="pkg" [disabled]="status() !== 'running'" />
      @if (errors() | async; as errors) {
        <service-dependencies
          [pkg]="pkg"
          [services]="services() || {}"
          [errors]="errors"
        />
      }
      <service-health-checks [checks]="health()" />
      <service-action-requests [pkg]="pkg" [services]="services() || {}" />
    }

    @if (installing() && pkg(); as pkg) {
      @for (
        item of pkg.stateInfo.installingInfo?.progress?.phases;
        track $index
      ) {
        <p [progress]="item.progress">{{ item.name }}</p>
      }
    }
  `,
  styles: `
    :host {
      display: grid;
      grid-template-columns: repeat(6, 1fr);
      grid-auto-rows: max-content;
      gap: 1rem;
    }

    small {
      font-weight: normal;
      text-transform: uppercase;
    }

    :host-context(tui-root._mobile) {
      grid-template-columns: 1fr;

      > * {
        grid-column: span 1;
      }
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    ServiceProgressComponent,
    ServiceStatusComponent,
    ServiceControlsComponent,
    ServiceInterfacesComponent,
    ServiceHealthChecksComponent,
    ServiceDependenciesComponent,
    ServiceErrorComponent,
    ServiceActionRequestsComponent,
    UptimeComponent,
  ],
})
export class ServiceRoute {
  private readonly errorService = inject(DepErrorService)
  protected readonly connected = toSignal(inject(ConnectionService))

  protected readonly id = toSignal(
    inject(ActivatedRoute).paramMap.pipe(map(params => params.get('pkgId'))),
  )

  protected readonly services = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('packageData'),
    { initialValue: {} as Record<string, PackageDataEntry> },
  )

  protected readonly errors = computed((id = this.id()) =>
    id ? this.errorService.getPkgDepErrors$(id) : of({}),
  )

  protected readonly pkg = computed(() => this.services()[this.id() || ''])

  protected readonly health = computed((pkg = this.pkg()) =>
    pkg ? toHealthCheck(pkg.status) : [],
  )

  protected readonly status = computed((pkg = this.pkg()) =>
    pkg?.stateInfo.state === 'installed'
      ? getInstalledPrimaryStatus(pkg)
      : pkg?.stateInfo.state,
  )

  protected readonly installed = computed(
    () =>
      this.pkg()?.stateInfo.state === 'installed' &&
      this.status() !== 'backingUp',
  )

  protected readonly installing = computed(
    (state = this.status()) =>
      state === 'installing' || state === 'updating' || state === 'restoring',
  )
}

function toHealthCheck(status: T.MainStatus): T.NamedHealthCheckResult[] {
  return status.main !== 'running' || isEmptyObject(status.health)
    ? []
    : Object.values(status.health)
}
