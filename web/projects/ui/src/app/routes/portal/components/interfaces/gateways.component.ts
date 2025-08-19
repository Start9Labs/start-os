import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton, TuiSwitch } from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
import { i18nPipe } from '@start9labs/shared'
import { TuiCell } from '@taiga-ui/layout'
import { InterfaceGateway } from './interface.service'

@Component({
  selector: 'section[gateways]',
  template: `
    <header>{{ 'Gateways' | i18n }}</header>
    @for (gateway of gateways(); track $index) {
      <label tuiCell="s">
        <span tuiTitle>{{ gateway.ipInfo.name }}</span>
        <input
          type="checkbox"
          tuiSwitch
          size="s"
          [showIcons]="false"
          [ngModel]="gateway.enabled"
          (ngModelChange)="onToggle(gateway)"
          [disabled]="isOs() && !gateway.public"
        />
      </label>
    } @empty {
      @for (_ of [0, 1]; track $index) {
        <label tuiCell="s">
          <span tuiTitle [tuiSkeleton]="true">{{ 'Loading' | i18n }}</span>
        </label>
      }
    }
  `,
  styles: `
    :host {
      grid-column: span 2;

      &:has(+ section table) header {
        background: transparent;
      }
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    TuiSwitch,
    i18nPipe,
    TuiCell,
    TuiTitle,
    TuiSkeleton,
  ],
})
export class InterfaceGatewaysComponent {
  readonly gateways = input.required<InterfaceGateway[] | undefined>()
  readonly isOs = input.required<boolean>()

  async onToggle(gateway: InterfaceGateway) {}
}
