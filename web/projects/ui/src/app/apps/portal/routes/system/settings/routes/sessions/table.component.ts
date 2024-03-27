import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
} from '@angular/core'
import { TuiLinkModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCheckboxModule,
  TuiIconModule,
} from '@taiga-ui/experimental'
import { BehaviorSubject } from 'rxjs'
import { TuiForModule } from '@taiga-ui/cdk'
import { Session } from 'src/app/services/api/api.types'
import { PlatformInfoPipe } from './platform-info.pipe'
import { FormsModule } from '@angular/forms'

@Component({
  selector: 'table[sessions]',
  template: `
    <thead>
      <tr>
        <th [style.width.%]="50" [style.padding-left.rem]="single ? null : 2">
          <input
            *ngIf="!single"
            tuiCheckbox
            size="s"
            type="checkbox"
            [disabled]="!sessions?.length"
            [ngModel]="all"
            (ngModelChange)="onAll($event)"
          />
          User Agent
        </th>
        <th [style.width.%]="25">Platform</th>
        <th [style.width.%]="25">Last Active</th>
      </tr>
    </thead>
    <tbody>
      <tr *ngFor="let session of sessions; else: loading">
        <td [style.padding-left.rem]="single ? null : 2">
          <input
            *ngIf="!single"
            tuiCheckbox
            size="s"
            type="checkbox"
            [ngModel]="selected$.value.includes(session)"
            (ngModelChange)="onToggle(session)"
          />
          {{ session.userAgent }}
        </td>
        <td *ngIf="session.metadata.platforms | platformInfo as info">
          <tui-icon [icon]="info.icon"></tui-icon>
          {{ info.name }}
        </td>
        <td>{{ session.lastActive }}</td>
      </tr>
      <ng-template #loading>
        <tr *ngFor="let _ of single ? [''] : ['', '']">
          <td colspan="5">
            <div class="tui-skeleton">Loading</div>
          </td>
        </tr>
      </ng-template>
    </tbody>
  `,
  styles: [
    `
      input {
        position: absolute;
        top: 50%;
        left: 0.5rem;
        transform: translateY(-50%);
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiForModule,
    TuiButtonModule,
    TuiLinkModule,
    PlatformInfoPipe,
    TuiIconModule,
    TuiCheckboxModule,
    FormsModule,
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
