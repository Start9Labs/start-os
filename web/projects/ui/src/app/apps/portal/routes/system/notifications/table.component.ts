import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
} from '@angular/core'
import {
  ServerNotification,
  ServerNotifications,
} from 'src/app/services/api/api.types'
import { TuiForModule } from '@taiga-ui/cdk'
import { BehaviorSubject } from 'rxjs'
import { TuiCheckboxModule, TuiLineClampModule } from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
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
      <tr
        *ngFor="let notification of notifications; else: loading; empty: blank"
      >
        <notification-item
          [notification]="notification"
          [selected]="selected$.value.includes(notification)"
          (onToggle)="handleToggle($event)"
        />
      </tr>
      <ng-template #blank>
        <tr>
          <td colspan="5">You have no notifications</td>
        </tr>
      </ng-template>
      <ng-template #loading>
        <tr *ngFor="let row of ['', '']">
          <td colspan="5"><div class="tui-skeleton">Loading</div></td>
        </tr>
      </ng-template>
    </tbody>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiForModule,
    TuiCheckboxModule,
    FormsModule,
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
