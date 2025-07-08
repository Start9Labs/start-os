import { TuiCheckbox, TuiSkeleton } from '@taiga-ui/kit'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import {
  ServerNotification,
  ServerNotifications,
} from 'src/app/services/api/api.types'
import { NotificationItemComponent } from './item.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  selector: 'table[notifications]',
  template: `
    <thead>
      <tr>
        <th [style.width.rem]="1.5">
          <input
            tuiCheckbox
            size="s"
            type="checkbox"
            [style.display]="'block'"
            [disabled]="!notifications?.length"
            [ngModel]="all"
            (ngModelChange)="onAll($event)"
          />
        </th>
        <th [style.min-width.rem]="12">{{ 'Date' | i18n }}</th>
        <th [style.min-width.rem]="14">{{ 'Title' | i18n }}</th>
        <th [style.min-width.rem]="8">{{ 'Service' | i18n }}</th>
        <th>{{ 'Message' | i18n }}</th>
      </tr>
    </thead>
    <tbody>
      @if (notifications) {
        @for (notification of notifications; track $index) {
          <tr
            [notificationItem]="notification"
            [style.font-weight]="notification.seen ? 'normal' : 'bold'"
          >
            <input
              tuiCheckbox
              size="s"
              type="checkbox"
              [style.display]="'block'"
              [ngModel]="selected().includes(notification)"
              (ngModelChange)="handleToggle(notification)"
            />
          </tr>
        } @empty {
          <tr>
            <td colspan="5">{{ 'No notifications' | i18n }}</td>
          </tr>
        }
      } @else {
        @for (row of ['', '']; track $index) {
          <tr>
            <td colspan="5">
              <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
            </td>
          </tr>
        }
      }
    </tbody>
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host-context(tui-root._mobile) {
      margin: 0 -1rem;

      input {
        @include taiga.fullsize();
        opacity: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    FormsModule,
    TuiCheckbox,
    NotificationItemComponent,
    TuiSkeleton,
    i18nPipe,
  ],
})
export class NotificationsTableComponent implements OnChanges {
  @Input() notifications?: ServerNotifications

  get all(): boolean | null {
    if (!this.notifications?.length || !this.selected().length) {
      return false
    }

    if (this.notifications?.length === this.selected().length) {
      return true
    }

    return null
  }

  readonly selected = signal<ServerNotifications>([])

  ngOnChanges() {
    this.selected.set([])
  }

  onAll(selected: boolean) {
    this.selected.set((selected && this.notifications) || [])
  }

  handleToggle(notification: ServerNotification<number>) {
    const selected = this.selected()

    if (selected.some(s => s.id === notification.id)) {
      this.selected.set(selected.filter(s => s.id !== notification.id))
    } else {
      this.selected.set([...selected, notification])
    }
  }
}
