import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiAlert, TuiLink } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { endWith, map, merge, Observable, pairwise, Subject } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'notifications-toast',
  template: `
    <ng-template
      [tuiAlert]="!!(visible$ | async)"
      [tuiAlertOptions]="{ label: 'StartOS' }"
      (tuiAlertChange)="onDismiss()"
    >
      {{ 'New notifications' | i18n }}
      <a tuiLink routerLink="/notifications" [queryParams]="{ toast: true }">
        {{ 'View' | i18n }}
      </a>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAlert, RouterLink, AsyncPipe, i18nPipe, TuiLink],
})
export class NotificationsToastComponent {
  private readonly dismiss$ = new Subject<boolean>()

  readonly visible$: Observable<boolean> = merge(
    this.dismiss$,
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'unreadNotificationCount')
      .pipe(
        pairwise(),
        map(([prev, cur]) => cur > prev),
        endWith(false),
      ),
  )

  onDismiss() {
    this.dismiss$.next(false)
  }
}
