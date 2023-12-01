import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Subject } from 'rxjs'
import { RR, ServerNotifications } from 'src/app/services/api/api.types'
import { NotificationService } from '../../../services/notification.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService } from '@start9labs/shared'
import { TuiForModule, TuiLetModule } from '@taiga-ui/cdk'
import { TuiFadeModule } from '@taiga-ui/experimental'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
} from '@taiga-ui/core'
import { NotificationsTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *tuiLet="notifications$ | async as notifications">
      <h3 class="g-title">
        Notifications
        <ng-container *ngIf="table.selected$ | async as selected">
          <tui-hosted-dropdown
            tuiDropdownAlign="right"
            [content]="dropdown"
            [sided]="true"
            [(open)]="open"
          >
            <button
              appearance="primary"
              iconRight="tuiIconChevronDown"
              tuiButton
              size="xs"
              type="button"
              [disabled]="!selected.length"
            >
              Batch Action
            </button>
          </tui-hosted-dropdown>
          <ng-template #dropdown>
            <tui-data-list>
              <button tuiOption (click)="markSeen(notifications!, selected)">
                Mark seen
              </button>
              <button tuiOption (click)="markUnseen(notifications!, selected)">
                Mark unseen
              </button>
              <button tuiOption (click)="remove(notifications!, selected)">
                Delete
              </button>
            </tui-data-list>
          </ng-template>
        </ng-container>
      </h3>
      <table #table class="g-table" [notifications]="notifications"></table>
    </ng-container>
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiForModule,
    TuiFadeModule,
    TuiHostedDropdownModule,
    TuiButtonModule,
    TuiDataListModule,
    NotificationsTableComponent,
    TuiLetModule,
  ],
})
export class NotificationsComponent {
  readonly service = inject(NotificationService)
  readonly api = inject(ApiService)
  readonly errorService = inject(ErrorService)

  readonly notifications$ = new Subject<ServerNotifications | null>()

  open = false

  ngOnInit() {
    this.getMore({})
  }

  async getMore(params: RR.GetNotificationsReq) {
    try {
      this.notifications$.next(null)
      const notifications = await this.api.getNotifications(params)
      this.notifications$.next(notifications)
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async markSeen(
    current: ServerNotifications,
    toUpdate: ServerNotifications,
  ): Promise<void> {
    this.open = false

    this.notifications$.next(
      current.map(c => ({
        ...c,
        read: toUpdate.some(n => n.id === c.id) ? true : c.read,
      })),
    )

    this.service.markSeen(toUpdate)
  }

  async markUnseen(
    current: ServerNotifications,
    toUpdate: ServerNotifications,
  ): Promise<void> {
    this.open = false

    this.notifications$.next(
      current.map(c => ({
        ...c,
        read: toUpdate.some(n => n.id === c.id) ? false : c.read,
      })),
    )

    this.service.markUnseen(toUpdate)
  }

  async remove(
    current: ServerNotifications,
    toDelete: ServerNotifications,
  ): Promise<void> {
    this.open = false

    this.notifications$.next(
      current.filter(c => !toDelete.some(n => n.id === c.id)),
    )

    this.service.remove(toDelete)
  }
}
