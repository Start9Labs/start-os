import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiLoader } from '@taiga-ui/core'
import { ServiceUptimeComponent } from 'src/app/routes/portal/routes/services/components/uptime.component'
import { getProgressText } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import {
  getInstalledPrimaryStatus,
  PrimaryRendering,
} from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'service-status',
  template: `
    <header>{{ 'Status' | i18n }}</header>
    <div>
      @if (info()) {
        <h3>
          <tui-loader size="s" [inheritColor]="true" />
          {{ 'Installing' | i18n }}
          <span class="loading-dots"></span>
          {{ info() | i18n }}
        </h3>
      } @else {
        <h3 [class]="class()">
          {{ text() || 'Unknown' | i18n }}
          @if (text() === 'Task Required') {
            <small>{{ 'See below' | i18n }}</small>
          }

          @if (rendering().showDots) {
            <span class="loading-dots"></span>
          }

          @if (pkg().statusInfo.started; as started) {
            <service-uptime [started]="started" />
          }
        </h3>
      }
      <ng-content />
    </div>
  `,
  styles: `
    :host {
      grid-column: span 3;
      min-height: 12rem;
    }

    h3 {
      font: var(--tui-font-heading-4);
      font-weight: normal;
      margin: 0;
      text-align: center;
    }

    div {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      flex: 1;
      padding: 1rem 0;
    }

    small {
      display: block;
      font: var(--tui-font-text-l);
      color: var(--tui-text-secondary);
      text-align: center;
    }

    tui-loader {
      display: inline-flex;
      vertical-align: bottom;
      margin: 0 0.25rem -0.125rem 0;
    }

    service-uptime {
      display: none;
      width: fit-content;
      margin: 0.5rem 0.125rem;
    }

    :host-context(tui-root._mobile) {
      :host {
        min-height: 0;
      }

      div {
        display: grid;
        grid-template-columns: 1fr max-content;
        padding: 0.5rem 0;
      }

      h3 {
        text-align: left;
      }

      small {
        text-align: left;
      }

      service-uptime {
        display: flex;
      }
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader, i18nPipe, ServiceUptimeComponent],
})
export class ServiceStatusComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly connected = input(false)

  protected readonly status = computed((pkg = this.pkg()) =>
    pkg?.stateInfo.state === 'installed'
      ? getInstalledPrimaryStatus(pkg)
      : pkg?.stateInfo.state,
  )

  protected readonly rendering = computed(() => PrimaryRendering[this.status()])
  protected readonly text = computed(
    () => this.connected() && this.rendering().display,
  )

  protected readonly class = computed(() => {
    switch (this.connected() && this.rendering().color) {
      case 'danger':
        return 'g-negative'
      case 'warning':
        return 'g-warning'
      case 'success':
        return 'g-positive'
      case 'primary':
        return 'g-info'
      default:
        return null
    }
  })

  protected readonly info = computed(
    (progress = this.pkg().stateInfo.installingInfo?.progress.overall) =>
      progress ? getProgressText(progress) : '',
  )
}
