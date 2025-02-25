import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  Input,
} from '@angular/core'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'
import { InstallingInfo } from 'src/app/services/patch-db/data-model'
import {
  PrimaryRendering,
  PrimaryStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { InstallingProgressDisplayPipe } from '../pipes/install-progress.pipe'

@Component({
  selector: 'service-status',
  template: `
    <header>Status</header>
    <div [class]="class">
      @if (installingInfo) {
        <strong>
          <tui-loader size="s" [inheritColor]="true" />
          Installing
          <span class="loading-dots"></span>
          {{ installingInfo.progress.overall | installingProgressString }}
        </strong>
      } @else {
        <tui-icon [icon]="icon" [style.margin-bottom.rem]="0.25" />
        {{ connected ? rendering.display : 'Unknown' }}
        @if (rendering.showDots) {
          <span class="loading-dots"></span>
        }
      }
      <ng-content />
    </div>
  `,
  styles: [
    `
      :host {
        display: grid;
        grid-template-rows: min-content 1fr;
        align-items: center;
        font: var(--tui-font-heading-6);
        text-align: center;
      }

      status {
        display: grid;
        grid-template-rows: min-content 1fr 1fr;
        align-items: center;
      }

      tui-loader {
        display: inline-flex;
        vertical-align: bottom;
        margin: 0 0.25rem -0.125rem 0;
      }
    `,
  ],
  host: { class: 'g-card' },
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [InstallingProgressDisplayPipe, TuiIcon, TuiLoader],
})
export class ServiceStatusComponent {
  @Input({ required: true })
  status!: PrimaryStatus

  @Input()
  installingInfo?: InstallingInfo

  @Input()
  connected = false

  get class(): string | null {
    if (!this.connected) return null

    switch (this.rendering.color) {
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
    return PrimaryRendering[this.status]
  }

  get icon(): string {
    if (!this.connected) return '@tui.circle'

    switch (this.rendering.color) {
      case 'danger':
        return '@tui.circle-x'
      case 'warning':
        return '@tui.circle-alert'
      case 'success':
        return '@tui.circle-check'
      case 'primary':
        return '@tui.circle-minus'
      default:
        return '@tui.circle'
    }
  }
}
