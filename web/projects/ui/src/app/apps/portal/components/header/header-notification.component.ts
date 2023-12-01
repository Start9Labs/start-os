import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { TuiButtonModule, TuiTitleModule } from '@taiga-ui/experimental'
import { TuiLineClampModule } from '@taiga-ui/kit'
import { ServerNotification } from 'src/app/services/api/api.types'
import { NotificationService } from '../../services/notification.service'

@Component({
  selector: 'header-notification',
  template: `
    <tui-svg
      style="align-self: flex-start; margin: 0.2rem 0;"
      [style.color]="color"
      [src]="icon"
    ></tui-svg>
    <div tuiTitle>
      <div tuiSubtitle><ng-content></ng-content></div>
      <div [style.color]="color">
        {{ notification.title }}
      </div>
      <tui-line-clamp
        tuiSubtitle
        style="pointer-events: none"
        [linesLimit]="4"
        [lineHeight]="16"
        [content]="notification.message"
        (overflownChange)="overflow = $event"
      />
      <div style="display: flex; gap: 0.5rem; padding-top: 0.5rem;">
        <button
          *ngIf="notification.code === 1"
          tuiButton
          appearance="secondary"
          size="xs"
          (click)="service.viewReport(notification)"
        >
          View Report
        </button>

        <button
          *ngIf="overflow"
          tuiButton
          appearance="secondary"
          size="xs"
          (click)="service.viewFull(notification)"
        >
          View full
        </button>
        <ng-content select="a"></ng-content>
      </div>
    </div>
    <ng-content select="button"></ng-content>
  `,
  styles: [':host { box-shadow: 0 1px var(--tui-clear); }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiSvgModule,
    TuiTitleModule,
    TuiButtonModule,
    TuiLineClampModule,
  ],
})
export class HeaderNotificationComponent<T extends number> {
  readonly service = inject(NotificationService)

  @Input({ required: true }) notification!: ServerNotification<T>

  overflow = false

  get color(): string {
    return this.service.getColor(this.notification)
  }

  get icon(): string {
    return this.service.getIcon(this.notification)
  }
}
