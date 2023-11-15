import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiForModule } from '@taiga-ui/cdk'
import {
  TuiDialogOptions,
  TuiDialogService,
  TuiScrollbarModule,
} from '@taiga-ui/core'
import {
  TuiAvatarStackModule,
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject, filter, first } from 'rxjs'
import { ServerNotifications } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderNotificationComponent } from './header-notification.component'
import { toRouterLink } from '../../utils/to-router-link'
import { shouldCall } from '@tinkoff/ng-event-plugins'

const limit = 40

@Component({
  selector: 'header-notifications',
  template: `
    <h3 class="g-title" style="padding: 0 1rem">
      Notifications
      <button
        *ngIf="notification$.value?.length"
        tuiButton
        size="xs"
        appearance="secondary"
        [style.margin-left]="'auto'"
        (click)="clear()"
      >
        Clear All
      </button>
    </h3>
    <tui-scrollbar
      *ngIf="packageData$ | async as packageData"
      (scroll.silent)="onScroll($event.target)"
    >
      <header-notification
        *ngFor="
          let not of notification$ | async;
          let i = index;
          empty: blank;
          else: loading
        "
        tuiCell
        [notification]="not"
      >
        <ng-container *ngIf="not['package-id'] as pkgId">
          {{ $any(packageData[pkgId])?.manifest.title || pkgId }}
        </ng-container>
        <a
          *ngIf="not['package-id'] && packageData[not['package-id']]"
          tuiButton
          size="xs"
          appearance="secondary"
          [routerLink]="getLink(not['package-id'] || '')"
        >
          View Service
        </a>
        <button
          tuiIconButton
          iconLeft="tuiIconX"
          appearance="icon"
          size="xs"
          style="align-self: flex-start; margin: 0.75rem 0;"
          (click)="delete(not.id, i)"
        ></button>
      </header-notification>
      <ng-template #blank>
        <div style="padding: 0 1rem">
          Important system alerts and notifications from StartOS will display
          here
        </div>
      </ng-template>
      <ng-template #loading>
        <div tuiCell *ngFor="let item of skeleton">
          <tui-avatar class="tui-skeleton"></tui-avatar>
          <div tuiTitle>
            <div class="tui-skeleton" style="align-self: flex-start">
              Loading
            </div>
            <div tuiSubtitle class="tui-skeleton">Loading {{ item }}</div>
          </div>
        </div>
      </ng-template>
    </tui-scrollbar>
  `,
  styles: [
    `
      :host {
        display: flex;
        flex-direction: column;
        height: 100%;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    RouterLink,
    TuiForModule,
    TuiScrollbarModule,
    TuiButtonModule,
    HeaderNotificationComponent,
    TuiCellModule,
    TuiAvatarStackModule,
    TuiTitleModule,
  ],
})
export class HeaderNotificationsComponent implements OnInit {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly dialogs = inject(TuiDialogService)

  readonly skeleton = ['notification', '', 'content', 'notification text']
  readonly notification$ = new BehaviorSubject<ServerNotifications | null>(null)
  readonly packageData$ = inject(PatchDB<DataModel>)
    .watch$('package-data')
    .pipe(first())

  hasMore = false

  async ngOnInit() {
    this.notification$.next(await this.getNotifications())
  }

  getLink(id: string) {
    return toRouterLink(id)
  }

  @shouldCall(shouldLoad)
  async onScroll(_: unknown) {
    const more = await this.getNotifications()

    this.hasMore = false
    this.notification$.next(this.notification$.value?.concat(more) || [])
  }

  async delete(id: number, index: number): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()
    const { value } = this.notification$

    try {
      await this.api.deleteNotification({ id })
      this.notification$.next(value?.splice(index, 1) || null)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  clear() {
    this.dialogs
      .open(TUI_PROMPT, OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()
        const before = this.notification$.value![0].id + 1

        try {
          await this.api.deleteAllNotifications({ before })

          this.notification$.next([])
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  private async getNotifications(): Promise<ServerNotifications> {
    const index = this.notification$.value?.length ?? 0
    const before = this.notification$.value?.[index]?.id

    try {
      const notifications = await this.api.getNotifications({ before, limit })

      this.hasMore = notifications?.length === limit

      return notifications || []
    } catch (e: any) {
      this.errorService.handleError(e)
    }

    return []
  }
}

const OPTIONS: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Delete All?',
  size: 's',
  data: {
    content: 'Are you sure you want to delete all notifications?',
    yes: 'Delete',
    no: 'Cancel',
  },
}

function shouldLoad(
  this: HeaderNotificationsComponent,
  { scrollTop, clientHeight, scrollHeight }: HTMLElement,
) {
  return this.hasMore && scrollTop + clientHeight === scrollHeight
}
