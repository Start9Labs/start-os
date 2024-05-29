import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  Input,
} from '@angular/core'
import { TuiLoaderModule } from '@taiga-ui/core'
import { TuiIconModule } from '@taiga-ui/experimental'
import { StatusRendering } from 'src/app/services/pkg-status-rendering.service'
import { InstallingProgressDisplayPipe } from '../pipes/install-progress.pipe'
import { InstallingInfo } from 'src/app/services/patch-db/data-model'
import { UnitConversionPipesModule } from '@start9labs/shared'

@Component({
  selector: 'service-status',
  template: `
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
      @if (sigtermTimeout && (sigtermTimeout | durationToSeconds) > 30) {
        <div>This may take a while</div>
      }
    }
  `,
  styles: [
    `
      :host {
        display: block;
        font-size: x-large;
        white-space: nowrap;
        margin: auto 0;
        min-height: 2.75rem;
        color: var(--tui-text-02);
      }

      tui-loader {
        display: inline-flex;
        vertical-align: bottom;
        margin: 0 0.25rem -0.125rem 0;
      }

      div {
        font-size: 1rem;
        color: var(--tui-text-02);
        margin: 1rem 0;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    InstallingProgressDisplayPipe,
    UnitConversionPipesModule,
    TuiIconModule,
    TuiLoaderModule,
  ],
})
export class ServiceStatusComponent {
  @Input({ required: true })
  rendering!: StatusRendering

  @Input()
  installingInfo?: InstallingInfo

  @Input()
  connected = false

  @Input() sigtermTimeout?: string | null = null

  @HostBinding('class')
  get class(): string | null {
    if (!this.connected) return null

    switch (this.rendering.color) {
      case 'danger':
        return 'g-error'
      case 'warning':
        return 'g-warning'
      case 'success':
        return 'g-success'
      case 'primary':
        return 'g-info'
      default:
        return null
    }
  }

  get icon(): string {
    if (!this.connected) return 'tuiIconCircle'

    switch (this.rendering.color) {
      case 'danger':
        return 'tuiIconXCircle'
      case 'warning':
        return 'tuiIconAlertCircle'
      case 'success':
        return 'tuiIconCheckCircle'
      case 'primary':
        return 'tuiIconMinusCircle'
      default:
        return 'tuiIconCircle'
    }
  }
}
