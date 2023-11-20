import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
} from '@angular/core'
import { RouterLink } from '@angular/router'
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
import { filter, first } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderNotificationComponent } from './header-notification.component'
import { toRouterLink } from '../../utils/to-router-link'
import { shouldCall } from '@tinkoff/ng-event-plugins'
import { HeaderNotificationsService } from './header-notifications.service'

@Component({
  selector: 'header-notifications',
  template: `
    <h3 class="g-title" style="padding: 0 1rem">
      Notifications
      <button
        *ngIf="service.current.length"
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
          let not of service.notification$ | async;
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
          (click)="service.delete(not.id)"
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
        width: 22rem;
        max-width: 80vw;
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
  private readonly dialogs = inject(TuiDialogService)

  readonly skeleton = ['notification', '', 'content', 'notification text']
  readonly service = inject(HeaderNotificationsService)
  readonly packageData$ = inject(PatchDB<DataModel>)
    .watch$('package-data')
    .pipe(first())

  ngOnInit() {
    this.service.load()
  }

  @shouldCall(shouldLoad)
  onScroll(_: unknown) {
    this.service.load()
  }

  clear() {
    this.dialogs
      .open(TUI_PROMPT, OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(() => this.service.clear())
  }

  getLink(id: string) {
    return toRouterLink(id)
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
  return this.service.hasMore && scrollTop + clientHeight === scrollHeight
}
