import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  Input,
} from '@angular/core'
import { InstallProgress } from 'src/app/services/patch-db/data-model'
import { StatusRendering } from 'src/app/services/pkg-status-rendering.service'
import { InstallProgressPipe } from '../pipes/install-progress.pipe'

@Component({
  selector: 'service-status',
  template: `
    <strong *ngIf="!installProgress; else installing">
      {{ connected ? rendering.display : 'Unknown' }}
      <!-- @TODO should show 'this may take a while' if sigtermTimeout is > 30s -->
      <span *ngIf="rendering.showDots" class="loading-dots"></span>
    </strong>
    <ng-template #installing>
      <strong *ngIf="installProgress | installProgress as progress">
        Installing
        <span class="loading-dots"></span>
        {{ progress }}
      </strong>
    </ng-template>
  `,
  styles: [
    `
      :host {
        font-size: x-large;
        margin: 1em 0;
        display: block;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InstallProgressPipe],
})
export class ServiceStatusComponent {
  @Input({ required: true })
  rendering!: StatusRendering

  @Input()
  installProgress?: InstallProgress

  @Input()
  connected = false

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
