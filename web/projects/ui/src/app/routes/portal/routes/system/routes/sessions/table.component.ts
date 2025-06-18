import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
  Input,
  OnChanges,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiIcon } from '@taiga-ui/core'
import { TuiCheckbox, TuiFade, TuiSkeleton } from '@taiga-ui/kit'
import { BehaviorSubject } from 'rxjs'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { Session } from 'src/app/services/api/api.types'
import { PlatformInfoPipe } from './platform-info.pipe'
import { i18nPipe } from '@start9labs/shared'

@Component({
  selector: '[sessions]',
  template: `
    <table
      [appTable]="
        single()
          ? ['User Agent', 'Platform', 'Last Active']
          : ['Platform', 'Last Active']
      "
    >
      @if (!single()) {
        <th [style.text-indent.rem]="1.75">
          <input
            tuiCheckbox
            size="s"
            type="checkbox"
            [disabled]="!sessions()"
            [ngModel]="all()"
            (ngModelChange)="selected.set(($event && sessions()) || [])"
          />
          {{ 'User Agent' | i18n }}
        </th>
      }
      @for (session of sessions(); track $index) {
        <tr>
          <td [style.padding-left.rem]="single() ? null : 2.5">
            <label>
              @if (!single()) {
                <input
                  tuiCheckbox
                  size="s"
                  type="checkbox"
                  [ngModel]="selected().includes(session)"
                  (ngModelChange)="onToggle(session)"
                />
              }
              <div tuiFade class="agent">{{ session.userAgent || '-' }}</div>
            </label>
          </td>
          @if (session.userAgent | platformInfo; as platform) {
            <td class="platform">
              <tui-icon [icon]="platform.icon" />
              {{ platform.name }}
            </td>
          }
          <td class="date">{{ session.lastActive | date: 'medium' }}</td>
        </tr>
      } @empty {
        @if (sessions()) {
          <tr>
            <td colspan="3">{{ 'No sessions' | i18n }}</td>
          </tr>
        } @else {
          @for (item of single() ? [''] : ['', '']; track $index) {
            <tr>
              <td colspan="3">
                <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
              </td>
            </tr>
          }
        }
      }
    </table>
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    td {
      position: relative;
      width: 25%;

      &[colspan] {
        grid-column: span 2;
      }

      &:first-child {
        width: 50%;
      }
    }

    input {
      position: absolute;
      top: 50%;
      left: 0.75rem;
      transform: translateY(-50%);
    }

    .platform {
      white-space: nowrap;
    }

    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 2.5rem 1fr;

        &:has(:checked) .platform {
          visibility: hidden;
        }
      }

      input {
        left: 0.25rem;

        &:not(:checked) {
          @include taiga.fullsize();
          z-index: 1;
          visibility: hidden;
          transform: none;
        }
      }

      td {
        width: 100%;

        &:first-child {
          padding: 0 !important;
        }
      }

      .agent {
        white-space: nowrap;
        display: block;
      }

      .platform {
        font-size: 0;
        grid-area: 1 / 1 / 3 / 1;
        place-content: center;
      }

      .date {
        color: var(--tui-text-secondary);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    PlatformInfoPipe,
    TuiIcon,
    TuiCheckbox,
    TuiFade,
    TuiSkeleton,
    TableComponent,
    i18nPipe,
  ],
})
export class SessionsTableComponent<T extends Session> implements OnChanges {
  readonly sessions = input<readonly T[] | null>(null)
  readonly single = input(false)

  readonly selected = signal<readonly T[]>([])
  readonly all = computed(
    () =>
      !!this.selected()?.length &&
      (this.selected().length === this.sessions()?.length || null),
  )

  ngOnChanges() {
    this.selected.set([])
  }

  onToggle(session: T) {
    if (this.selected().includes(session)) {
      this.selected.update(selected => selected.filter(s => s !== session))
    } else {
      this.selected.update(selected => [...selected, session])
    }
  }
}
