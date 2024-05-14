import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  Input,
} from '@angular/core'
import { StatusRendering } from 'src/app/services/pkg-status-rendering.service'
import { InstallingProgressDisplayPipe } from '../pipes/install-progress.pipe'
import { InstallingInfo } from 'src/app/services/patch-db/data-model'
import { UnitConversionPipesModule } from '@start9labs/shared'

@Component({
  selector: 'service-status',
  template: `
    @if (installingInfo) {
      <strong>
        Installing
        <span class="loading-dots"></span>
        {{ installingInfo.progress.overall | installingProgressString }}
      </strong>
    } @else {
      {{ connected ? rendering.display : 'Unknown' }}

      <span *ngIf="sigtermTimeout && (sigtermTimeout | durationToSeconds) > 30">
        . This may take a while
      </span>

      <span *ngIf="rendering.showDots" class="loading-dots"></span>
    }
  `,
  styles: [
    `
      :host {
        display: block;
        font-size: x-large;
        white-space: nowrap;
        margin: auto 0;
        height: 2.75rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    InstallingProgressDisplayPipe,
    UnitConversionPipesModule,
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

  @HostBinding('style.color')
  get color(): string {
    if (!this.connected) return 'var(--tui-text-02)'

    switch (this.rendering.color) {
      case 'danger':
        return 'var(--tui-error-fill)'
      case 'warning':
        return 'var(--tui-warning-fill)'
      case 'success':
        return 'var(--tui-success-fill)'
      case 'primary':
        return 'var(--tui-info-fill)'
      default:
        return 'var(--tui-text-02)'
    }
  }
}
