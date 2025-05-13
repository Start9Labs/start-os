import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute } from '@angular/router'
import { WaIntersectionObserver } from '@ng-web-apis/intersection-observer'
import { i18nPipe, isEmptyObject } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiElement } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { map, of } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getInstalledPrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { ServiceTasksComponent } from '../components/tasks.component'
import { ServiceControlsComponent } from '../components/controls.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceErrorComponent } from '../components/error.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceInterfacesComponent } from '../components/interfaces.component'
import { ServiceInstallProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'
import { ServiceUptimeComponent } from '../components/uptime.component'

@Component({
  template: `
    @if (pkg(); as pkg) {
      @if (pkg.status.main === 'error') {
        <service-error [pkg]="pkg" />
      } @else if (installing()) {
        <service-install-progress [pkg]="pkg" />
      } @else if (installed()) {
        <service-status
          [connected]="!!connected()"
          [installingInfo]="pkg.stateInfo.installingInfo"
          [status]="status()"
        >
          @if (connected()) {
            <service-controls [pkg]="pkg" [status]="status()" />
          }
        </service-status>

        @if (status() !== 'backingUp') {
          <service-uptime [started]="$any(pkg.status)?.started" />
          <service-interfaces [pkg]="pkg" [disabled]="status() !== 'running'" />

          @if (errors() | async; as errors) {
            <service-dependencies
              [pkg]="pkg"
              [services]="services()"
              [errors]="errors"
            />
          }

          <service-health-checks [checks]="health()" />
          <service-tasks
            #tasks="elementRef"
            tuiElement
            waIntersectionObserver
            waIntersectionThreshold="0.5"
            (waIntersectionObservee)="scrolled = $event.at(-1)?.isIntersecting"
            [pkg]="pkg"
            [services]="services() || {}"
          />
          <button
            tuiIconButton
            iconStart="@tui.arrow-down"
            tabindex="-1"
            class="arrow"
            [class.arrow_hidden]="scrolled"
            (click)="
              tasks.nativeElement.scrollIntoView({
                block: 'end',
                behavior: 'smooth',
              })
            "
          >
            {{ 'Tasks' | i18n }}
          </button>
        }
      } @else if (removing()) {
        <service-status
          [connected]="!!connected()"
          [status]="status()"
        ></service-status>
      }
    }
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    @keyframes bounce {
      to {
        transform: translateY(-1rem);
      }
    }

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

    .arrow {
      @include taiga.transition(opacity);
      position: sticky;
      bottom: 1rem;
      border-radius: 100%;
      place-self: center;
      grid-area: auto / span 6;
      box-shadow: inset 0 0 0 2rem var(--tui-status-warning);
      animation: bounce 1s infinite alternate;

      &_hidden,
      :host:has(::ng-deep service-tasks app-placeholder) & {
        opacity: 0;
        pointer-events: none;
      }
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
  imports: [
    CommonModule,
    TuiElement,
    TuiButton,
    WaIntersectionObserver,
    i18nPipe,
    ServiceInstallProgressComponent,
    ServiceStatusComponent,
    ServiceControlsComponent,
    ServiceInterfacesComponent,
    ServiceHealthChecksComponent,
    ServiceDependenciesComponent,
    ServiceErrorComponent,
    ServiceTasksComponent,
    ServiceUptimeComponent,
  ],
})
export class ServiceRoute {
  private readonly errorService = inject(DepErrorService)
  protected readonly connected = toSignal(inject(ConnectionService))

  protected scrolled?: boolean

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
    () => this.pkg()?.stateInfo.state === 'installed',
  )

  protected readonly installing = computed(
    (state = this.status()) =>
      state === 'installing' || state === 'updating' || state === 'restoring',
  )

  protected readonly removing = computed(
    () => this.pkg()?.stateInfo.state === 'removing',
  )
}

function toHealthCheck(status: T.MainStatus): T.NamedHealthCheckResult[] {
  return status.main !== 'running' || isEmptyObject(status.health)
    ? []
    : Object.values(status.health)
}
