import { DatePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  signal,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { MarketplacePkg } from '@start9labs/marketplace'
import {
  DialogService,
  i18nKey,
  i18nPipe,
  MarkdownPipe,
  SafeLinksDirective,
} from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiExpand } from '@taiga-ui/experimental'
import {
  TUI_CONFIRM,
  TuiAvatar,
  TuiButtonLoading,
  TuiChevron,
  TuiFade,
  TuiProgressCircle,
} from '@taiga-ui/kit'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { PatchDB } from 'patch-db-client'
import { defaultIfEmpty, firstValueFrom } from 'rxjs'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/services/pipes/install-progress.pipe'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { getAllPackages } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'
import UpdatesComponent from './updates.component'

@Component({
  standalone: true,
  selector: 'updates-item',
  template: `
    <tr (click)="expanded.set(!expanded())">
      <td>
        <div [style.gap.rem]="0.75">
          <tui-avatar size="s"><img alt="" [src]="item().icon" /></tui-avatar>
          <span tuiTitle [style.margin]="'-0.125rem 0 0'">
            <b tuiFade>{{ item().title }}</b>
            <span tuiSubtitle tuiFade class="mobile">
              <span class="g-secondary">
                {{ local().stateInfo.manifest.version }}
              </span>
              <tui-icon icon="@tui.arrow-right" />
              <span class="g-positive">{{ item().version }}</span>
            </span>
          </span>
        </div>
      </td>
      <td class="desktop">
        <div>
          <span class="g-secondary">
            {{ local().stateInfo.manifest.version }}
          </span>
          <tui-icon icon="@tui.arrow-right" />
          <span class="g-positive">{{ item().version }}</span>
        </div>
      </td>
      <td class="desktop">{{ item().gitHash }}</td>
      <td class="desktop">{{ item().s9pk.publishedAt | date }}</td>
      <td>
        <div>
          @if (local().stateInfo.state === 'updating') {
            <tui-progress-circle
              class="g-positive"
              size="xs"
              [max]="100"
              [value]="
                (local().stateInfo.installingInfo?.progress?.overall
                  | installingProgress) || 0
              "
            />
          } @else {
            <button
              tuiButton
              size="s"
              [loading]="!ready()"
              [appearance]="error() ? 'destructive' : 'primary'"
              (click.stop)="onClick()"
            >
              {{ error() ? ('Retry' | i18n) : ('Update' | i18n) }}
            </button>
          }
          <button
            tuiIconButton
            size="s"
            appearance="icon"
            [tuiChevron]="expanded()"
          >
            {{ 'Show more' | i18n }}
          </button>
        </div>
      </td>
    </tr>
    <tr>
      <td colspan="5">
        @if (error()) {
          <p class="g-negative">{{ error() }}</p>
        }
        <tui-expand [expanded]="expanded()">
          <p tuiTitle class="mobile">
            <b>{{ 'Package Hash' | i18n }}</b>
            <span tuiSubtitle>{{ item().gitHash }}</span>
          </p>
          <p tuiTitle class="mobile">
            <b>{{ 'Published' | i18n }}</b>
            <span tuiSubtitle>{{ item().s9pk.publishedAt | date }}</span>
          </p>
          <p tuiTitle>
            <span>
              <b>{{ 'Release notes' | i18n }}</b>
              (
              <a
                tuiLink
                iconEnd="@tui.external-link"
                routerLink="/portal/marketplace"
                [queryParams]="{ url: parent.current()?.url, id: item().id }"
                [textContent]="'View listing' | i18n"
              ></a>
              )
            </span>
          </p>
          <p
            safeLinks
            [innerHTML]="item().releaseNotes | markdown | dompurify"
          ></p>
        </tui-expand>
      </td>
    </tr>
  `,
  styles: `
    :host {
      display: contents;
    }

    tui-icon {
      font-size: 1rem;
    }

    div {
      display: flex;
      align-items: center;
      gap: 0.25rem;
      white-space: nowrap;
    }

    tr:first-child {
      min-height: var(--tui-height-l);
      word-break: break-word;
      clip-path: inset(0 round var(--tui-radius-s));
      cursor: pointer;
    }

    td {
      min-width: 0;
      vertical-align: middle;

      &:first-child {
        white-space: nowrap;
      }

      &:last-child {
        text-align: right;
        white-space: nowrap;

        div {
          justify-content: flex-end;
        }
      }

      &[colspan]:only-child {
        padding: 0 3rem;
        text-align: left;
      }
    }

    [tuiTitle] {
      margin: 1rem 0;
    }

    .mobile {
      display: none;
    }

    :host-context(tui-root._mobile) {
      tr:first-child {
        display: grid;
        grid-template-columns: 1fr min-content;
        align-items: center;
        padding: 0.5rem 0;
      }

      td[colspan]:only-child {
        padding: 0 0.5rem;
      }

      .desktop {
        display: none;
      }

      .mobile {
        display: flex;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    TuiExpand,
    TuiButton,
    TuiButtonLoading,
    TuiChevron,
    TuiAvatar,
    TuiLink,
    TuiIcon,
    TuiProgressCircle,
    TuiTitle,
    TuiFade,
    MarkdownPipe,
    NgDompurifyModule,
    SafeLinksDirective,
    DatePipe,
    InstallingProgressPipe,
    i18nPipe,
  ],
})
export class UpdatesItemComponent {
  private readonly dialog = inject(DialogService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly service = inject(MarketplaceService)
  private readonly i18n = inject(i18nPipe)

  readonly parent = inject(UpdatesComponent)
  readonly expanded = signal(false)
  readonly error = signal('')
  readonly ready = signal(true)

  readonly item = input.required<MarketplacePkg>()
  readonly local =
    input.required<PackageDataEntry<InstalledState | UpdatingState>>()

  async onClick() {
    this.ready.set(false)
    this.error.set('')

    if (hasCurrentDeps(this.item().id, await getAllPackages(this.patch))) {
      if (await this.alert()) {
        await this.update()
      } else {
        this.ready.set(true)
      }
    } else {
      await this.update()
    }
  }

  private async update() {
    const { id, version } = this.item()
    const url = this.parent.current()?.url || ''

    try {
      await this.service.installPackage(id, version, url)
      this.ready.set(true)
    } catch (e: any) {
      this.ready.set(true)
      this.error.set(e.message)
    }
  }

  private async alert(): Promise<boolean> {
    return firstValueFrom(
      this.dialog
        .openConfirm<boolean>({
          label: 'Warning',
          size: 's',
          data: {
            content:
              `${this.i18n.transform('Services that depend on')} ${this.local().stateInfo.manifest.title} ${this.i18n.transform('will no longer work properly and may crash.')}` as i18nKey,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .pipe(defaultIfEmpty(false)),
    )
  }
}
