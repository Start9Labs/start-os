import { NgTemplateOutlet } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
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
    <ng-container *title>
      {{ 'Notifications' | i18n }}
      <ng-container *ngTemplateOutlet="button" />
    </ng-container>
    <section class="g-card">
      <header>
        {{ 'Notifications' | i18n }}
        <ng-container *ngTemplateOutlet="button" />
      </header>
      <table #table class="g-table" [notifications]="notifications()"></table>
      <ng-template #button>
        <button
          appearance="primary"
          iconEnd="@tui.chevron-down"
          tuiButton
          size="xs"
          type="button"
          tuiDropdownOpen
          tuiDropdownAlign="right"
          [tuiDropdown]="dropdown"
          [tuiDropdownEnabled]="!!table.selected().length"
          [style.margin-inline-start]="'auto'"
          [disabled]="!table.selected().length"
        >
          {{ 'Batch action' | i18n }}
          <ng-template #dropdown let-close>
            <tui-data-list (click)="close()">
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
              <button
                tuiOption
                (click)="remove(notifications(), table.selected())"
              >
                {{ 'Delete' | i18n }}
              </button>
            </tui-data-list>
          </ng-template>
        </button>
      </ng-template>
    </section>
  `,
  styles: `
    :host {
      padding: 1rem;
    }

    :host-context(tui-root._mobile) {
      header {
        display: none;
      }

      section {
        padding-block: 0;
      }
    }
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiDropdown,
    TuiButton,
    TuiDataList,
    NotificationsTableComponent,
    TitleDirective,
    i18nPipe,
    NgTemplateOutlet,
  ],
})
export default class NotificationsComponent implements OnInit {
  private readonly router = inject(Router)
  private readonly route = inject(ActivatedRoute)

  readonly service = inject(NotificationService)
  readonly api = inject(ApiService)
  readonly errorService = inject(ErrorService)
  readonly notifications = signal<ServerNotifications | undefined>(undefined)

  ngOnInit() {
    this.route.queryParams.subscribe(params => {
      this.router.navigate([], { relativeTo: this.route, queryParams: {} })

      if (isEmptyObject(params)) {
        this.getMore({})
      }
    })
  }

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
    this.notifications.set(
      current.filter(c => !toDelete.some(n => n.id === c.id)),
    )

    this.service.remove(toDelete)
  }
}
