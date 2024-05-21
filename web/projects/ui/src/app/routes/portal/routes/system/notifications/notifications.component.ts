import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Subject } from 'rxjs'
import { RR, ServerNotifications } from 'src/app/services/api/api.types'
import { NotificationService } from 'src/app/services/notification.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService } from '@start9labs/shared'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiDataListModule, TuiHostedDropdownModule } from '@taiga-ui/core'
import { NotificationsTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *tuiLet="notifications$ | async as notifications">
      <h3 class="g-title">
        <tui-hosted-dropdown
          *ngIf="table.selected$ | async as selected"
          tuiDropdownAlign="right"
          [content]="dropdown"
          [sided]="true"
          [(open)]="open"
          [canOpen]="!!selected.length"
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
        </tui-hosted-dropdown>
      </h3>
      <table #table class="g-table" [notifications]="notifications"></table>
    </ng-container>
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiHostedDropdownModule,
    TuiButtonModule,
    TuiDataListModule,
    NotificationsTableComponent,
    TuiLetModule,
  ],
})
export default class NotificationsComponent {
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
      this.notifications$.next(await this.api.getNotifications(params))
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  markSeen(current: ServerNotifications, toUpdate: ServerNotifications) {
    this.open = false

    this.notifications$.next(
      current.map(c => ({
        ...c,
        read: toUpdate.some(n => n.id === c.id) || c.read,
      })),
    )

    this.service.markSeen(toUpdate)
  }

  markUnseen(current: ServerNotifications, toUpdate: ServerNotifications) {
    this.open = false

    this.notifications$.next(
      current.map(c => ({
        ...c,
        read: c.read && !toUpdate.some(n => n.id === c.id),
      })),
    )

    this.service.markUnseen(toUpdate)
  }

  remove(current: ServerNotifications, toDelete: ServerNotifications) {
    this.open = false

    this.notifications$.next(
      current.filter(c => !toDelete.some(n => n.id === c.id)),
    )

    this.service.remove(toDelete)
  }
}
