import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiIconModule, TuiTitleModule } from '@taiga-ui/experimental'
import { SettingBtn } from '../settings.types'

@Component({
  selector: 'settings-button',
  template: `
    <button *ngIf="button.action" class="g-action" (click)="button.action()">
      <ng-container *ngTemplateOutlet="template" />
    </button>
    <a
      *ngIf="button.routerLink"
      class="g-action"
      [routerLink]="button.routerLink"
    >
      <ng-container *ngTemplateOutlet="template" />
    </a>
    <ng-template #template>
      <tui-icon [icon]="button.icon" />
      <div tuiTitle [style.flex]="1">
        <strong>{{ button.title }}</strong>
        <div tuiSubtitle>{{ button.description }}</div>
        <ng-content />
      </div>
      <tui-icon *ngIf="button.routerLink" icon="tuiIconChevronRight" />
    </ng-template>
  `,
  styles: [
    ':host:not(:last-child) { display: block; box-shadow: 0 1px var(--tui-clear); }',
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiIconModule, TuiTitleModule, RouterLink],
})
export class SettingsButtonComponent {
  @Input({ required: true })
  button!: SettingBtn
}
