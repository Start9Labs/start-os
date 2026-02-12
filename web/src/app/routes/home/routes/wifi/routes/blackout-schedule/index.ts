import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { Placeholder } from 'src/app/components/placeholder'
import { Help } from 'src/app/directives/help'
import { WifiBlackoutAside } from './aside'
import { ADD_BLACKOUT_WINDOW } from './dialog'
import { BlackoutService, BlackoutWindow } from './service'

// Display order: Mon–Sun; data order: Sun(0)–Sat(6)
const DISPLAY_ORDER = [1, 2, 3, 4, 5, 6, 0]
const DAY_LABELS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']

function formatTime(time: string): string {
  const [h, m] = time.split(':').map(Number)
  const period = h >= 12 ? 'PM' : 'AM'
  const hour = h % 12 || 12
  return `${hour}:${m.toString().padStart(2, '0')} ${period}`
}

@Component({
  template: `
    <wifi-blackout-aside *help />
    <table tuiTable class="g-table" [tuiSkeleton]="!service.data()">
      <thead>
        <tr>
          <th tuiTh>Start Time</th>
          <th tuiTh>End Time</th>
          <th tuiTh>Days</th>
          <th tuiTh>
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="add()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (item of windows(); track $index) {
          <tr>
            <td tuiTd>{{ item.startTime }}</td>
            <td tuiTd>{{ item.endTime }}</td>
            <td tuiTd>{{ item.daysLabel }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                appearance="icon"
                class="g-negative"
                iconStart="@tui.trash"
                (click)="delete($index)"
              >
                Delete
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="4">
              <app-placeholder icon="@tui.moon">
                No blackout windows configured
              </app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      max-width: 50rem;
    }

    td:nth-child(1),
    td:nth-child(2) {
      font-weight: bold;
    }

    th:last-child,
    td:last-child {
      text-align: end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    TuiButton,
    TuiTable,
    Placeholder,
    TuiSkeleton,
    WifiBlackoutAside,
    Help,
  ],
})
export default class BlackoutScheduleComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly service = inject(BlackoutService)
  protected readonly windows = computed(
    () =>
      this.service.data()?.map(w => ({
        startTime: formatTime(w.startTime),
        endTime: formatTime(w.endTime),
        daysLabel: DISPLAY_ORDER.filter(i => w.days[i])
          .map(i => DAY_LABELS[i])
          .join(', '),
      })) || [],
  )

  add() {
    this.dialogs
      .open<BlackoutWindow>(ADD_BLACKOUT_WINDOW, {
        label: 'Add Blackout Window',
      })
      .subscribe(result => {
        this.service.addWindow(result)
      })
  }

  delete(index: number) {
    this.service.deleteWindow(index)
  }
}
