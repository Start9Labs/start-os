import { TuiLineClamp, TuiCheckbox, TuiSkeleton } from '@taiga-ui/kit'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
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
              [ngModel]="selected().includes(notification)"
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
            <td colspan="5"><div [tuiSkeleton]="true">Loading</div></td>
          </tr>
        }
      }
    </tbody>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host-context(tui-root._mobile) input {
      @include fullsize();
      opacity: 0;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    FormsModule,
    TuiCheckbox,
    TuiLineClamp,
    NotificationItemComponent,
    TuiSkeleton,
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
