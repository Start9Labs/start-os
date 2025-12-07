import { TuiCheckbox, TuiSkeleton } from '@taiga-ui/kit'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
  OnChanges,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ServerNotification } from 'src/app/services/api/api.types'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { NotificationItemComponent } from './item.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  selector: '[notifications]',
  template: `
    <table [appTable]="['Title', 'Service', 'Message']">
      <th [style.text-indent.rem]="1.75">
        <input
          tuiCheckbox
          size="s"
          type="checkbox"
          [disabled]="!notifications()"
          [ngModel]="all()"
          (ngModelChange)="selected.set(($event && notifications()) || [])"
        />
        {{ 'Date' | i18n }}
      </th>
      @for (not of notifications(); track not) {
        <tr
          [notificationItem]="not"
          (longtap)="!selected().length && onToggle(not)"
          (click)="
            selected().length &&
              $any($event.target).closest('tui-root._mobile') &&
              onToggle(not)
          "
        >
          <input
            tuiCheckbox
            size="s"
            type="checkbox"
            [ngModel]="selected().includes(not)"
            (ngModelChange)="onToggle(not)"
          />
        </tr>
      } @empty {
        @if (notifications()) {
          <tr>
            <td colspan="4">{{ 'No notifications' | i18n }}</td>
          </tr>
        } @else {
          @for (i of ['', '']; track $index) {
            <tr>
              <td colspan="4">
                <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
              </td>
            </tr>
          }
        }
      }
    </table>
  `,
  styles: `
    input {
      position: absolute;
      top: 50%;
      left: 0.75rem;
      transform: translateY(-50%);
    }

    td:only-child {
      text-align: center;
    }

    :host-context(tui-root._mobile) {
      input {
        position: absolute;
        top: 2.875rem;
        left: 0;
        z-index: 1;
        pointer-events: none;
      }

      :host:not(:has(:checked)) input {
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
    TableComponent,
  ],
})
export class NotificationsTableComponent<T extends ServerNotification<number>>
  implements OnChanges
{
  readonly notifications = input<readonly T[] | null>(null)

  readonly selected = signal<readonly T[]>([])
  readonly all = computed(
    () =>
      !!this.selected()?.length &&
      (this.selected().length === this.notifications()?.length || null),
  )

  ngOnChanges() {
    this.selected.set([])
  }

  onToggle(notification: T) {
    if (this.selected().includes(notification)) {
      this.selected.update(selected => selected.filter(s => s !== notification))
    } else {
      this.selected.update(selected => [...selected, notification])
    }
  }
}
