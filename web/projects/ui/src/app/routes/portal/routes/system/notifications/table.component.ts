import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiCheckboxModule } from '@taiga-ui/experimental'
import { TuiLineClampModule } from '@taiga-ui/kit'
import { BehaviorSubject } from 'rxjs'
import {
  ServerNotification,
  ServerNotifications,
} from 'src/app/services/api/api.types'
import { NotificationItemComponent } from './item.component'

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
        <th [style.min-width.rem]="12">Date</th>
        <th [style.min-width.rem]="12">Title</th>
        <th [style.min-width.rem]="8">Service</th>
        <th>Message</th>
      </tr>
    </thead>
    <tbody>
      @if (notifications) {
        @for (notification of notifications; track $index) {
          <tr
            [notificationItem]="notification"
            [style.font-weight]="notification.read ? 'normal' : 'bold'"
          >
            <input
              tuiCheckbox
              size="s"
              type="checkbox"
              [style.display]="'block'"
              [ngModel]="selected$.value.includes(notification)"
              (ngModelChange)="handleToggle(notification)"
            />
          </tr>
        } @empty {
          <tr>
            <td colspan="5">You have no notifications</td>
          </tr>
        }
      } @else {
        @for (row of ['', '']; track $index) {
          <tr>
            <td colspan="5"><div class="tui-skeleton">Loading</div></td>
          </tr>
        }
      }
    </tbody>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    FormsModule,
    TuiCheckboxModule,
    TuiLineClampModule,
    NotificationItemComponent,
  ],
})
export class NotificationsTableComponent implements OnChanges {
  @Input() notifications: ServerNotifications | null = null

  get all(): boolean | null {
    if (!this.notifications?.length || !this.selected$.value.length) {
      return false
    }

    if (this.notifications?.length === this.selected$.value.length) {
      return true
    }

    return null
  }

  readonly selected$ = new BehaviorSubject<ServerNotifications>([])

  ngOnChanges() {
    this.selected$.next([])
  }

  onAll(selected: boolean) {
    this.selected$.next((selected && this.notifications) || [])
  }

  handleToggle(notification: ServerNotification<number>) {
    const selected = this.selected$.value

    if (selected.some(s => s.id === notification.id)) {
      this.selected$.next(selected.filter(s => s.id !== notification.id))
    } else {
      this.selected$.next([...selected, notification])
    }
  }
}
