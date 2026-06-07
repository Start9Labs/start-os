import { KeyValuePipe, NgTemplateOutlet } from '@angular/common'
import {
  Component,
  computed,
  contentChild,
  inject,
  input,
  model,
  signal,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { i18nPipe, LocalizePipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiCell,
  TuiDataListComponent,
  TuiInput,
  TuiOption,
  TuiOptionWithValue,
  TuiScrollbar,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiAvatar,
  TuiButtonSelect,
  TuiChevron,
  TuiSkeleton,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader, TuiNavigation } from '@taiga-ui/layout'

import { filterPackages } from '../pipes/filter-packages.pipe'
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
  selector: 'marketplace',
  template: `
    <aside [tuiNavigationAside]="open()">
      <ng-content />
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
        <button
          tuiAsideItem
          type="button"
          [iconStart]="open() ? '@tui.chevron-left' : '@tui.chevron-right'"
          (click)="open.set(!open())"
        >
          {{ open() ? 'Collapse' : 'Expand' }}
        </button>
      </footer>
    </aside>
    <div class="content">
      <header tuiHeader="h4">
        <hgroup tuiTitle>
          <h2>
            @if (registry()) {
              {{ name() | localize }}
            }
          </h2>
        </hgroup>
        <aside tuiAccessories>
          <button
            appearance="secondary-grayscale"
            tuiButton
            tuiButtonSelect
            tuiChevron
            [(ngModel)]="sortLabel"
          >
            {{ sortLabel }}
            <tui-data-list *tuiDropdown>
              @for (key of sortKeys; track key) {
                <button tuiOption [value]="getLabel(key)">
                  {{ getLabel(key) }}
                </button>
              }
            </tui-data-list>
          </button>
        </aside>
      </header>
      <tui-scrollbar>
        <section>
          @if (registry()) {
            @for ($implicit of packages(); track $index) {
              <ng-container
                *ngTemplateOutlet="template(); context: { $implicit }"
              />
            }
          } @else {
            @for (_ of '-'.repeat(6); track $index) {
              <div tuiCardLarge="compact" [tuiSkeleton]="true">
                <span tuiCell>
                  <span tuiAvatar></span>
                  <span tuiTitle>
                    Loading
                    <span tuiSubtitle>Loading</span>
                  </span>
                </span>
                <span tuiDescription>Loading</span>
              </div>
            }
          }
        </section>
      </tui-scrollbar>
    </div>
  `,
  styles: `
    :host {
      --tui-theme-color: var(--tui-background-elevation-1);

      display: flex;
      width: 100%;

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
      }
    }

    .content {
      flex: 1;
      min-width: 0;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }

    [tuiHeader] {
      white-space: nowrap;
      padding: 1rem 2rem 0;
    }

    tui-scrollbar {
      padding-block-start: 1rem;
      mask: linear-gradient(transparent, black 1rem);
    }

    section {
      padding: 0 2rem 1rem;
      display: grid;
      gap: 1rem;
      grid-template-columns: repeat(auto-fill, minmax(17rem, 1fr));
    }

    :host-context(tui-root._mobile) :is([tuiHeader], section) {
      padding-inline: 1rem;
    }
  `,
  providers: [
    tuiButtonOptionsProvider({ size: 's', appearance: 'flat-grayscale' }),
  ],
  imports: [
    TuiNavigation,
    TuiInput,
    TuiSkeleton,
    TuiButton,
    TuiButtonSelect,
    TuiChevron,
    TuiDataListComponent,
    TuiHeader,
    TuiOption,
    TuiOptionWithValue,
    TuiScrollbar,
    TuiTitle,
    TuiAvatar,
    TuiCardLarge,
    TuiCell,
    NgTemplateOutlet,
    KeyValuePipe,
    LocalizePipe,
    FormsModule,
  ],
})
export class MarketplaceComponent {
  readonly registry = input<StoreDataWithUrl>()
  readonly query = model('')
  readonly category = model('')

  protected readonly template = contentChild(TemplateRef)
  protected readonly sort = signal('a')
  protected readonly sortKeys = ['a', '1']
  protected readonly i18n = inject(i18nPipe)
  protected readonly icons = ICONS
  protected readonly asIs = () => 0
  protected readonly open = signal(!inject(WA_IS_MOBILE))
  protected readonly categories = computed(
    () => this.registry()?.info?.categories,
  )

  protected readonly name = computed(
    (c = this.category()) => this.registry()?.info?.categories?.[c]?.name || c,
  )

  protected readonly packages = computed(() =>
    filterPackages(
      this.registry()?.packages || [],
      this.query(),
      this.category(),
      this.sort(),
    ),
  )

  protected readonly fallback: Record<string, T.Category> = {
    a: { name: '' },
    b: { name: '' },
    c: { name: '' },
  }

  protected get sortLabel(): string {
    return this.getLabel(this.sort())
  }

  protected set sortLabel(label: string) {
    this.sort.set(
      this.sortKeys.find(key => this.getLabel(key) === label) || 'a',
    )
  }

  onCategory(category: string) {
    this.query.set('')
    this.category.set(category)
  }

  getLabel(sort: string) {
    return this.i18n.transform(
      sort === 'a' ? 'Alphabetical' : 'Recently updated',
    )
  }
}
