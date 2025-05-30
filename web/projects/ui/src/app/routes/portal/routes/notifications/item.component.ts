import { TuiLineClamp } from '@taiga-ui/kit'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiIcon, TuiLink } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { first, Observable } from 'rxjs'
import { ServerNotification } from 'src/app/services/api/api.types'
import { NotificationService } from 'src/app/services/notification.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { toRouterLink } from 'src/app/utils/to-router-link'
import { i18nPipe } from '@start9labs/shared'

@Component({
  selector: '[notificationItem]',
  template: `
    <td class="checkbox"><ng-content /></td>
    <td class="date">
      {{ notificationItem.createdAt | date: 'medium' }}
    </td>
    <td class="title" [style.color]="color">
      <tui-icon [icon]="icon" [style.font-size.rem]="1" />
      {{ notificationItem.title }}
    </td>
    <td class="service">
      @if (manifest$ | async; as manifest) {
        <a tuiLink [routerLink]="getLink(manifest.id)">
          {{ manifest.title }}
        </a>
      } @else if (notificationItem.packageId) {
        {{ notificationItem.packageId }}
      } @else {
        -
      }
    </td>
    <td class="content">
      <tui-line-clamp
        style="pointer-events: none"
        [linesLimit]="4"
        [lineHeight]="21"
        [content]="notificationItem.message"
        (overflownChange)="overflow = $event"
      />
      @if (overflow) {
        <button tuiLink (click)="service.viewModal(notificationItem, true)">
          {{ 'View full' | i18n }}
        </button>
      }
      @if (notificationItem.code === 1 || notificationItem.code === 2) {
        <button tuiLink (click)="service.viewModal(notificationItem)">
          {{
            notificationItem.code === 1
              ? ('View report' | i18n)
              : ('View details' | i18n)
          }}
        </button>
      }
    </td>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: {
    '[class._new]': '!notificationItem.read',
  },
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      grid-template-columns: 1fr;

      &._new {
        background: var(--tui-background-neutral-1) !important;
      }
    }

    button {
      position: relative;
    }

    td {
      padding: 0.25rem;
      vertical-align: top;
    }

    .checkbox {
      padding-top: 0.4rem;
    }

    :host-context(tui-root._mobile) {
      gap: 0.5rem;
      padding: 0.75rem 1rem !important;

      .checkbox {
        @include taiga.fullsize();
        @include taiga.transition(box-shadow);

        &:has(:checked) {
          box-shadow: inset 0.25rem 0 var(--tui-background-accent-1);
        }
      }

      .date {
        order: 1;
        color: var(--tui-text-secondary);
      }

      .title {
        font-weight: bold;
        font-size: 1.2em;
        display: flex;
        gap: 0.5rem;
      }

      .service:not(:has(a)) {
        display: none;
      }
    }
  `,
  imports: [CommonModule, RouterLink, TuiLineClamp, TuiLink, TuiIcon, i18nPipe],
})
export class NotificationItemComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  readonly service = inject(NotificationService)

  @Input({ required: true }) notificationItem!: ServerNotification<number>

  overflow = false

  @tuiPure
  get manifest$(): Observable<T.Manifest> {
    return this.patch
      .watch$(
        'packageData',
        this.notificationItem.packageId || '',
        'stateInfo',
        'manifest',
      )
      .pipe(first())
  }

  get color(): string {
    return this.service.getColor(this.notificationItem)
  }

  get icon(): string {
    return this.service.getIcon(this.notificationItem)
  }

  getLink(id: string) {
    return toRouterLink(id)
  }
}
