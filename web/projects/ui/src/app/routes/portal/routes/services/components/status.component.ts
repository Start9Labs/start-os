import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiLoader } from '@taiga-ui/core'
import { getProgressText } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { InstallingInfo } from 'src/app/services/patch-db/data-model'
import {
  PrimaryRendering,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'service-status',
  template: `
    <header>Status</header>
    <div>
      @if (installingInfo) {
        <h3>
          <tui-loader size="s" [inheritColor]="true" />
          Installing
          <span class="loading-dots"></span>
          {{ getText(installingInfo.progress.overall) }}
        </h3>
      } @else {
        <h3 [class]="class">
          {{ text }}
          @if (text === 'Action Required') {
            <small>See below</small>
          }

          @if (rendering?.showDots) {
            <span class="loading-dots"></span>
          }
        </h3>
      }
      <ng-content />
    </div>
  `,
  styles: [
    `
      :host {
        grid-column: span 2;
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

      :host-context(tui-root._mobile) {
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
      }
    `,
  ],
  host: { class: 'g-card' },
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class ServiceStatusComponent {
  @Input({ required: true })
  status?: PrimaryStatus

  @Input()
  installingInfo?: InstallingInfo

  @Input()
  connected = false

  get text() {
    return this.connected ? this.rendering?.display : 'Unknown'
  }

  get class(): string | null {
    if (!this.connected) return null

    switch (this.rendering?.color) {
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
  }

  get rendering() {
    return this.status && PrimaryRendering[this.status]
  }

  getText(progress: T.Progress): string {
    return getProgressText(progress)
  }
}
