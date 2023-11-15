import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiDialogService, TuiSvgModule } from '@taiga-ui/core'
import { TuiButtonModule, TuiTitleModule } from '@taiga-ui/experimental'
import { TuiLineClampModule } from '@taiga-ui/kit'
import {
  NotificationLevel,
  ServerNotification,
} from 'src/app/services/api/api.types'
import { REPORT } from 'src/app/apps/portal/modals/report.component'

@Component({
  selector: 'header-notification',
  template: `
    <tui-svg
      style="align-self: flex-start; margin: 0.75rem 0;"
      [style.color]="color"
      [src]="icon"
    ></tui-svg>
    <div tuiTitle>
      <div tuiSubtitle><ng-content></ng-content></div>
      <div [style.color]="color">{{ notification.title }}</div>
      <tui-line-clamp
        tuiSubtitle
        style="pointer-events: none"
        [linesLimit]="8"
        [lineHeight]="16"
        [content]="notification.message"
        (overflownChange)="overflow = $event"
      />
      <div style="display: flex; gap: 0.5rem">
        <button
          *ngIf="notification.code === 1"
          tuiButton
          appearance="secondary"
          size="xs"
          (click)="viewReport()"
        >
          View Report
        </button>

        <button
          *ngIf="overflow"
          tuiButton
          appearance="secondary"
          size="xs"
          (click)="viewFull()"
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
  private readonly dialogs = inject(TuiDialogService)

  @Input({ required: true }) notification!: ServerNotification<T>

  overflow = false

  get color(): string {
    switch (this.notification.level) {
      case NotificationLevel.Info:
        return 'var(--tui-info-fill)'
      case NotificationLevel.Success:
        return 'var(--tui-success-fill)'
      case NotificationLevel.Warning:
        return 'var(--tui-warning-fill)'
      case NotificationLevel.Error:
        return 'var(--tui-error-fill)'
    }
  }

  get icon(): string {
    switch (this.notification.level) {
      case NotificationLevel.Info:
        return 'tuiIconInfoLarge'
      case NotificationLevel.Success:
        return 'tuiIconCheckCircleLarge'
      case NotificationLevel.Warning:
        return 'tuiIconAlertCircleLarge'
      case NotificationLevel.Error:
        return 'tuiIconXCircleLarge'
    }
  }

  viewFull() {
    this.dialogs
      .open(this.notification.message, { label: this.notification.title })
      .subscribe()
  }

  viewReport() {
    this.dialogs
      .open(REPORT, {
        label: 'Backup Report',
        data: {
          report: this.notification.data,
          timestamp: this.notification['created-at'],
        },
      })
      .subscribe()
  }
}
