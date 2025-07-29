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
        <button tuiLink (click.stop)="onClick()">
          {{ 'View full' | i18n }}
        </button>
      }
      @if ([1, 2].includes(notificationItem.code)) {
        <button tuiLink (click.stop)="onClick()">
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
    '[class._new]': '!notificationItem.seen',
    '(click)': 'onClick()',
  },
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      grid-template-columns: 1fr;

      &._new td {
        font-weight: bold;
        color: var(--tui-text-primary);

        &.checkbox {
          box-shadow: inset 0.25rem 0 var(--tui-text-action);
        }
      }
    }

    tui-icon {
      vertical-align: text-top;
    }

    button {
      position: relative;
    }

    td {
      padding: 0.25rem;
      vertical-align: top;
      color: var(--tui-text-secondary);
    }

    .checkbox {
      padding-top: 0.4rem;
    }

    :host-context(tui-root._mobile) {
      gap: 0.5rem;
      padding: 0.75rem 1rem !important;

      .checkbox {
        @include taiga.fullsize();
      }

      .date {
        order: 1;
        color: var(--tui-text-secondary);
      }

      .title {
        font-weight: bold;
        font-size: 1.2em;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }

      .service:not(:has(a)) {
        display: none;
      }
    }

    :host-context(tui-root._mobile table:has(:checked)) tui-icon {
      opacity: 0;
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

  onClick() {
    if (this.overflow) {
      this.service.viewModal(this.notificationItem, true)
      this.notificationItem.seen = true
    } else if ([1, 2].includes(this.notificationItem.code)) {
      this.service.viewModal(this.notificationItem)
      this.notificationItem.seen = true
    }
  }
}
