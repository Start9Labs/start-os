import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiIcon } from '@taiga-ui/core'
import { TuiCheckbox, TuiFade, TuiSkeleton } from '@taiga-ui/kit'
import { BehaviorSubject } from 'rxjs'
import { Session } from 'src/app/services/api/api.types'
import { PlatformInfoPipe } from './platform-info.pipe'

@Component({
  selector: 'table[sessions]',
  template: `
    <thead>
      <tr>
        <th
          tuiTh
          [style.width.%]="50"
          [style.padding-left.rem]="single ? null : 2"
        >
          @if (!single) {
            <input
              tuiCheckbox
              size="s"
              type="checkbox"
              [disabled]="!sessions?.length"
              [ngModel]="all"
              (ngModelChange)="onAll($event)"
            />
          }
          User Agent
        </th>
        <th tuiTh [style.width.%]="25">Platform</th>
        <th tuiTh [style.width.%]="25">Last Active</th>
      </tr>
    </thead>
    <tbody>
      @for (session of sessions; track $index) {
        <tr>
          <td [style.padding-left.rem]="single ? null : 2.25">
            @if (!single) {
              <input
                tuiCheckbox
                size="s"
                type="checkbox"
                [ngModel]="selected$.value.includes(session)"
                (ngModelChange)="onToggle(session)"
              />
            }
            <span tuiFade class="agent">{{ session.userAgent }}</span>
          </td>
          @if (session.metadata.platforms | platformInfo; as info) {
            <td class="platform">
              <tui-icon [icon]="info.icon" />
              {{ info.name }}
            </td>
          }
          <td class="date">{{ session.lastActive | date: 'medium' }}</td>
        </tr>
      } @empty {
        @if (sessions) {
          <tr><td colspan="5">No sessions</td></tr>
        } @else {
          @for (item of single ? [''] : ['', '']; track $index) {
            <tr>
              <td colspan="5"><div [tuiSkeleton]="true">Loading</div></td>
            </tr>
          }
        }
      }
    </tbody>
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      td {
        position: relative;
      }

      input {
        position: absolute;
        top: 50%;
        left: 0.5rem;
        transform: translateY(-50%);
      }

      :host-context(tui-root._mobile) {
        input {
          @include fullsize();
          z-index: 1;
          opacity: 0;
          transform: none;
        }

        td:first-child {
          padding: 0 0.25rem !important;
        }

        .agent {
          white-space: nowrap;
          display: block;
        }

        .platform {
          font-weight: bold;
          display: flex;
          align-items: center;
          gap: 0.5rem;
          padding: 0;
        }

        .date {
          color: var(--tui-text-secondary);
        }
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    PlatformInfoPipe,
    TuiIcon,
    TuiCheckbox,
    TuiFade,
    TuiSkeleton,
    TuiTable,
  ],
})
export class SSHTableComponent<T extends Session> implements OnChanges {
  readonly selected$ = new BehaviorSubject<readonly T[]>([])

  @Input()
  sessions: readonly T[] | null = null

  @Input()
  single = false

  get all(): boolean | null {
    if (!this.sessions?.length || !this.selected$.value.length) {
      return false
    }

    if (this.sessions?.length === this.selected$.value.length) {
      return true
    }

    return null
  }

  ngOnChanges() {
    this.selected$.next([])
  }

  onAll(selected: boolean) {
    this.selected$.next((selected && this.sessions) || [])
  }

  onToggle(session: T) {
    const selected = this.selected$.value

    if (selected.includes(session)) {
      this.selected$.next(selected.filter(s => s !== session))
    } else {
      this.selected$.next([...selected, session])
    }
  }
}
