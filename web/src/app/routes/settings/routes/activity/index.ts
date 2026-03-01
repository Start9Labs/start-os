import { DatePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  signal,
} from '@angular/core'
import {
  TUI_TABLE_PAGINATION_TEXTS,
  TuiTable,
  TuiTablePagination,
  tuiTablePaginationOptionsProvider,
} from '@taiga-ui/addon-table'
import { TuiButton } from '@taiga-ui/core'

@Component({
  template: `
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>Date</th>
          <th tuiTh>Detail</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of filtered(); track $index) {
          <tr>
            <td tuiTd>{{ item.date | date: 'medium' }}</td>
            <td tuiTd>{{ item.key }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                type="button"
                size="xs"
                appearance="icon"
                iconStart="@tui.trash"
                (click)="remove($index)"
              >
                Delete
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
    <tui-table-pagination
      [total]="activity().length"
      [page]="page()"
      [size]="size()"
      (paginationChange)="page.set($event.page); size.set($event.size)"
    />
  `,
  styles: `
    :host {
      max-width: 50rem;
    }

    td:last-child {
      text-align: end;
    }

    tui-table-pagination {
      margin-top: 0.5rem;
    }
  `,
  host: { class: 'g-page' },
  providers: [
    tuiTablePaginationOptionsProvider({ showPages: false }),
    {
      provide: TUI_TABLE_PAGINATION_TEXTS,
      useValue: signal({ linesPerPage: '', of: 'of', pages: '' }),
    },
  ],
  imports: [DatePipe, TuiTable, TuiTablePagination, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Activity {
  protected readonly page = signal(0)
  protected readonly size = signal(10)
  protected readonly activity = signal(
    Array.from({ length: 25 }, (_, i) => ({
      date: new Date(Date.now() - i * 1000 * 60 * 60 * 24),
      key: `Random activity key #${i + 1}`,
    })),
  )

  protected readonly filtered = computed(() =>
    this.activity().filter(
      (_, i) =>
        i < (this.page() + 1) * this.size() && i >= this.page() * this.size(),
    ),
  )

  protected remove(index: number): void {
    this.activity.set(
      this.activity().filter((_, i) => i !== index + this.page() * this.size()),
    )

    if (!this.filtered().length) {
      this.page.update(page => Math.max(page - 1, 0))
    }
  }
}
