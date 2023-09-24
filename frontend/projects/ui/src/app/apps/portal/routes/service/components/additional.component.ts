import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { AdditionalItem, FALLBACK_URL } from '../pipes/to-additional.pipe'

@Component({
  selector: '[additional]',
  template: `
    <div [style.flex]="1">
      <strong>{{ additional.name }}</strong>
      <div>{{ additional.description }}</div>
    </div>
    <tui-svg *ngIf="icon" [src]="icon"></tui-svg>
  `,
  styles: [
    `
      :host._disabled {
        pointer-events: none;
        opacity: var(--tui-disabled-opacity);
      }
    `,
  ],
  host: {
    '[attr.href]': 'additional.description',
    '[class._disabled]': 'disabled',
    target: '_blank',
    rel: 'noreferrer',
  },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiSvgModule],
})
export class ServiceAdditionalComponent {
  @Input({ required: true })
  additional!: AdditionalItem

  get disabled(): boolean {
    return this.additional.description === FALLBACK_URL
  }

  get icon(): string | undefined {
    return this.additional.description.startsWith('http')
      ? 'tuiIconExternalLinkLarge'
      : this.additional.icon
  }
}
