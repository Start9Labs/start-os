import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
  signal,
  viewChild,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router } from '@angular/router'
import { ErrorService, i18nPipe, isEmptyObject } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { distinctUntilChanged, skip } from 'rxjs/operators'
import { ServerNotification } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BadgeService } from 'src/app/services/badge.service'
import { NotificationService } from 'src/app/services/notification.service'
import { TitleDirective } from 'src/app/services/title.service'
import { NotificationsTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>{{ 'Notifications' | i18n }}</ng-container>
    <section class="g-card">
      <header>
        {{ 'Notifications' | i18n }}
        <button
          tuiButton
          size="xs"
          iconStart="@tui.trash"
          appearance="primary-destructive"
          [style.margin]="'0 0.5rem 0 auto'"
          [disabled]="!table()?.selected()?.length"
          (click)="remove(notifications() || [])"
        >
          {{ 'Delete selected' | i18n }}
        </button>
      </header>
      <div [notifications]="notifications()"></div>
    </section>
  `,
  styles: `
    :host {
      padding: 1rem;
    }
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, NotificationsTableComponent, TitleDirective, i18nPipe],
})
export default class NotificationsComponent implements OnInit {
  private readonly router = inject(Router)
  private readonly route = inject(ActivatedRoute)
  private readonly loader = inject(TuiNotificationMiddleService)

  readonly service = inject(NotificationService)
  readonly api = inject(ApiService)
  readonly errorService = inject(ErrorService)
  readonly notifications = signal<T.NotificationWithId[] | null>(null)

  protected readonly table = viewChild<
    NotificationsTableComponent<ServerNotification<number>>
  >(NotificationsTableComponent)

  protected readonly badge = inject(BadgeService)
    .getCount('notifications')
    .pipe(
      distinctUntilChanged(),
      filter(Boolean),
      skip(1),
      takeUntilDestroyed(),
    )
    .subscribe(() => this.init())

  ngOnInit() {
    this.route.queryParams.subscribe(params => {
      this.router.navigate([], { relativeTo: this.route, queryParams: {} })

      if (isEmptyObject(params)) {
        this.init()
      }
    })
  }

  async getMore(params: T.ListNotificationParams) {
    try {
      this.notifications.set(null)
      this.notifications.set(await this.api.getNotifications(params))
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async remove(all: T.NotificationWithId[]) {
    const ids =
      this.table()
        ?.selected()
        .map(n => n.id) || []
    const loader = this.loader.open('Deleting').subscribe()

    try {
      await this.api.deleteNotifications({ ids })
      this.notifications.set(all.filter(n => !ids.includes(n.id)))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private init() {
    this.getMore({ before: null, limit: null }).then(() => {
      const latest = this.notifications()?.at(0)
      if (latest) {
        this.service.markSeenAll(latest.id)
      }
    })
  }
}
