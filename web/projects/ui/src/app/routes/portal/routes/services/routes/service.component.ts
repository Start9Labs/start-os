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
import { map } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getInstalledPrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { ServiceActionRequestsComponent } from '../components/action-requests.component'
import { ServiceActionsComponent } from '../components/actions.component'
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
      [installingInfo]="pkg().stateInfo.installingInfo"
      [status]="status()"
    >
      @if (installed() && connected()) {
        <service-actions [pkg]="pkg()" [status]="status()" />
      }
    </service-status>

    @if (installed()) {
      @if (pkg().status.main === 'error') {
        <service-error [pkg]="pkg()" />
      }

      <service-interfaces [pkg]="pkg()" [disabled]="status() !== 'running'" />
      <service-dependencies [pkg]="pkg()" [services]="services()" />
      <service-health-checks [checks]="health()" />

      <section class="g-card">
        <header>Tasks</header>
        <service-action-requests [pkg]="pkg()" />
      </section>
    }

    @if (installing()) {
      @for (
        item of pkg().stateInfo.installingInfo?.progress?.phases;
        track $index
      ) {
        <p [progress]="item.progress">{{ item.name }}</p>
      }
    }
  `,
  styles: `
    :host {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
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
        grid-column: span 1 !important;
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
    ServiceActionsComponent,
    ServiceInterfacesComponent,
    ServiceHealthChecksComponent,
    ServiceDependenciesComponent,
    ServiceErrorComponent,
    ServiceActionRequestsComponent,
  ],
})
export class ServiceRoute {
  protected readonly connected = toSignal(inject(ConnectionService))

  protected readonly id = toSignal(
    inject(ActivatedRoute).paramMap.pipe(map(params => params.get('pkgId'))),
  )

  protected readonly services = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('packageData'),
    { initialValue: {} as Record<string, PackageDataEntry> },
  )

  protected readonly pkg = computed(() => this.services()[this.id() || ''])

  protected readonly health = computed(() =>
    this.pkg() ? toHealthCheck(this.pkg().status) : [],
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
