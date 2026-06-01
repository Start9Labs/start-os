import { KeyValuePipe } from '@angular/common'
import {
  Component,
  computed,
  inject,
  input,
  model,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { DocsLinkDirective, i18nPipe, LocalizePipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiDataList,
  TuiDropdown,
  TuiInput,
} from '@taiga-ui/core'
import { TuiButtonSelect, TuiSkeleton } from '@taiga-ui/kit'
import { TuiNavigation } from '@taiga-ui/layout'
import { StoreDataWithUrl } from '../types'

const ICONS: Record<string, string> = {
  all: '@tui.layout-grid',
  bitcoin: '@tui.bitcoin',
  messaging: '@tui.message-circle',
  communications: '@tui.message-circle',
  data: '@tui.file-text',
  'developer tools': '@tui.table-split',
  featured: '@tui.star',
  lightning: '@tui.zap',
  media: '@tui.circle-play',
  networking: '@tui.globe',
  social: '@tui.users',
  ai: '@tui.cpu',
}

@Component({
  selector: 'marketplace-aside',
  template: `
    <aside [tuiNavigationAside]="open()">
      <header>
        <ng-content />
        <button
          tuiButtonSelect
          tuiAsideItem
          iconStart="@tui.arrow-down-a-z"
          type="button"
          [(ngModel)]="sort"
        >
          {{ getLabel(sort()) }}
          <tui-data-list *tuiDropdown>
            <button tuiOption iconStart="@tui.arrow-down-a-z" value="a">
              {{ getLabel('a') }}
            </button>
            <button tuiOption iconStart="@tui.arrow-up-a-z" value="z">
              {{ getLabel('z') }}
            </button>
            <button tuiOption iconStart="@tui.calendar-arrow-down" value="1">
              {{ getLabel('1') }}
            </button>
          </tui-data-list>
        </button>
      </header>
      <tui-textfield tuiTextfieldSize="s" iconStart="@tui.search">
        <input
          tuiInput
          placeholder="Search"
          [(ngModel)]="query"
          (focus)="open.set(true)"
        />
      </tui-textfield>
      @for (cat of categories() || fallback | keyvalue: asIs; track cat.key) {
        <button
          [tuiSkeleton]="!categories()"
          [iconStart]="icons[cat.key.toLowerCase()] || '@tui.box'"
          [disabled]="!categories()"
          [class._active]="cat.key === category()"
          tuiAsideItem
          type="button"
          (click)="onCategory(cat.key)"
        >
          {{ cat.value.name ? (cat.value.name | localize) : cat.key }}
        </button>
      }
      <footer>
        <a
          iconStart="@tui.external-link"
          docsLink
          path="/packaging/quick-start.html"
          tuiButton
          [textContent]="'Package a service' | i18n"
        ></a>
        @if (mobile || !open()) {
          <button
            tuiAsideItem
            type="button"
            [iconStart]="open() ? '@tui.chevron-left' : '@tui.chevron-right'"
            (click)="open.set(!open())"
          >
            {{ open() ? 'Collapse' : 'Expand' }}
          </button>
        }
      </footer>
    </aside>
  `,
  styles: `
    :host {
      --tui-theme-color: var(--tui-background-elevation-1);

      aside {
        height: 100%;

        &::before {
          display: none;
        }

        tui-textfield {
          margin-bottom: 0.75rem;
          padding: 0 0.5rem;
          flex-wrap: nowrap;
          overflow: hidden;
          --t-start: 1.375rem;
        }

        [tuiButton] {
          justify-content: flex-start;
          gap: 0.625rem;
        }
      }
    }
  `,
  providers: [
    tuiButtonOptionsProvider({ size: 's', appearance: 'flat-grayscale' }),
  ],
  imports: [
    TuiNavigation,
    TuiInput,
    TuiDataList,
    TuiDropdown,
    TuiButtonSelect,
    TuiButton,
    TuiSkeleton,
    i18nPipe,
    KeyValuePipe,
    LocalizePipe,
    FormsModule,
    DocsLinkDirective,
  ],
})
export class MarketplaceAsideComponent {
  readonly registry = input<StoreDataWithUrl>()
  readonly sort = model('a')
  readonly query = model('')
  readonly category = model('')

  protected readonly icons = ICONS
  protected readonly asIs = () => 0
  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly open = signal(!this.mobile)
  protected readonly categories = computed(
    () => this.registry()?.info?.categories,
  )

  protected readonly fallback: Record<string, T.Category> = {
    a: { name: '' },
    b: { name: '' },
    c: { name: '' },
  }

  onCategory(category: string) {
    this.query.set('')
    this.category.set(category)
  }

  getLabel(sort: string) {
    switch (sort) {
      case 'a':
        return 'Alphabetic'
      case 'z':
        return 'Alphabetic Reversed'
      default:
        return 'Latest Release'
    }
  }
}
