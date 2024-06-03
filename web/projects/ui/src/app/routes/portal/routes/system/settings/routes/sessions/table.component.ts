import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiLinkModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCheckboxModule,
  TuiFadeModule,
  TuiIconModule,
} from '@taiga-ui/experimental'
import { BehaviorSubject } from 'rxjs'
import { Session } from 'src/app/services/api/api.types'
import { PlatformInfoPipe } from './platform-info.pipe'

@Component({
  selector: 'table[sessions]',
  template: `
    <thead>
      <tr>
        <th [style.width.%]="50" [style.padding-left.rem]="single ? null : 2">
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
        <th [style.width.%]="25">Platform</th>
        <th [style.width.%]="25">Last Active</th>
      </tr>
    </thead>
    <tbody>
      @for (session of sessions; track $index) {
        <tr>
          <td [style.padding-left.rem]="single ? null : 2">
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
              <td colspan="5"><div class="tui-skeleton">Loading</div></td>
            </tr>
          }
        }
      }
    </tbody>
  `,
  styles: [
    `
      input {
        position: absolute;
        top: 50%;
        left: 0.25rem;
        transform: translateY(-50%);
      }

      :host-context(tui-root._mobile) {
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
          color: var(--tui-text-02);
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
    TuiButtonModule,
    TuiLinkModule,
    TuiIconModule,
    TuiCheckboxModule,
    TuiFadeModule,
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
