import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiCell } from '@taiga-ui/layout'
import { SettingBtn } from '../system.types'

@Component({
  selector: 'system-button',
  template: `
    @if (button.action) {
      <button tuiCell (click)="button.action()">
        <ng-container *ngTemplateOutlet="template" />
      </button>
    }

    @if (button.routerLink) {
      <a tuiCell [routerLink]="button.routerLink">
        <ng-container *ngTemplateOutlet="template" />
      </a>
    }

    <ng-template #template>
      <tui-icon [icon]="button.icon" />
      <div tuiTitle>
        <strong>{{ button.title }}</strong>
        <div tuiSubtitle>{{ button.description }}</div>
        <ng-content />
      </div>
      @if (button.routerLink) {
        <tui-icon icon="@tui.chevron-right" />
      }
    </ng-template>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;

      &:not(:last-child) {
        box-shadow: 0 1px var(--tui-background-neutral-1);
      }
    }

    button {
      cursor: pointer;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiIcon, TuiTitle, RouterLink, TuiCell],
})
export class SystemButtonComponent {
  @Input({ required: true })
  button!: SettingBtn
}
