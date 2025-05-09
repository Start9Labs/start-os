import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { ErrorService, i18nPipe, isEmptyObject } from '@start9labs/shared'
import { TuiButton, TuiDataList, TuiDropdown } from '@taiga-ui/core'
import { RR, ServerNotifications } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { NotificationService } from 'src/app/services/notification.service'
import { TitleDirective } from 'src/app/services/title.service'
import { NotificationsTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>{{ 'Notifications' | i18n }}</ng-container>
    <h3 class="g-title">
      <button
        appearance="primary"
        iconEnd="@tui.chevron-down"
        tuiButton
        size="xs"
        type="button"
        tuiDropdownAlign="right"
        tuiDropdownSided
        [disabled]="!table.selected().length"
        [tuiDropdown]="dropdown"
        [tuiDropdownEnabled]="!!table.selected().length"
        [(tuiDropdownOpen)]="open"
      >
        {{ 'Batch action' | i18n }}
      </button>
      <ng-template #dropdown>
        <tui-data-list>
          <button
            tuiOption
            (click)="markSeen(notifications(), table.selected())"
          >
            {{ 'Mark seen' | i18n }}
          </button>
          <button
            tuiOption
            (click)="markUnseen(notifications(), table.selected())"
          >
            {{ 'Mark unseen' | i18n }}
          </button>
          <button tuiOption (click)="remove(notifications(), table.selected())">
            {{ 'Delete' | i18n }}
          </button>
        </tui-data-list>
      </ng-template>
    </h3>
    <table #table class="g-table" [notifications]="notifications()"></table>
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    TuiDropdown,
    TuiButton,
    TuiDataList,
    NotificationsTableComponent,
    TitleDirective,
    i18nPipe,
  ],
})
export default class NotificationsComponent {
  private readonly router = inject(Router)
  private readonly route = inject(ActivatedRoute)

  readonly service = inject(NotificationService)
  readonly api = inject(ApiService)
  readonly errorService = inject(ErrorService)
  readonly notifications = signal<ServerNotifications | undefined>(undefined)
  readonly toast = this.route.queryParams.subscribe(params => {
    this.router.navigate([], { relativeTo: this.route, queryParams: {} })

    if (isEmptyObject(params)) {
      this.getMore({})
    }
  })

  open = false

  async getMore(params: RR.GetNotificationsReq) {
    try {
      this.notifications.set(undefined)
      this.notifications.set(await this.api.getNotifications(params))
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  markSeen(
    current: ServerNotifications = [],
    toUpdate: ServerNotifications = [],
  ) {
    this.open = false

    this.notifications.set(
      current.map(c => ({
        ...c,
        read: toUpdate.some(n => n.id === c.id) || c.seen,
      })),
    )

    this.service.markSeen(toUpdate)
  }

  markUnseen(
    current: ServerNotifications = [],
    toUpdate: ServerNotifications = [],
  ) {
    this.open = false

    this.notifications.set(
      current.map(c => ({
        ...c,
        read: c.seen && !toUpdate.some(n => n.id === c.id),
      })),
    )

    this.service.markUnseen(toUpdate)
  }

  remove(
    current: ServerNotifications = [],
    toDelete: ServerNotifications = [],
  ) {
    this.open = false

    this.notifications.set(
      current.filter(c => !toDelete.some(n => n.id === c.id)),
    )

    this.service.remove(toDelete)
  }
}
