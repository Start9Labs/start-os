import { KeyValuePipe } from '@angular/common'
import {
  Component,
  computed,
  inject,
  input,
  model,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { LocalizePipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { tuiButtonOptionsProvider, TuiInput } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
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
          flex-wrap: nowrap;
          overflow: hidden;
          --t-start: 1.375rem;
        }
      }
    }

    // Widen the expanded sidebar to fit the longest registry title
    // (Community Registry) plus the chevron. A fixed length (not max-content)
    // keeps the expand/collapse width transition animating.
    aside[tuiNavigationAside]._expanded {
      inline-size: 18rem;
    }
  `,
  providers: [
    tuiButtonOptionsProvider({ size: 's', appearance: 'flat-grayscale' }),
  ],
  imports: [
    TuiNavigation,
    TuiInput,
    TuiSkeleton,
    KeyValuePipe,
    LocalizePipe,
    FormsModule,
  ],
})
export class MarketplaceAsideComponent {
  readonly registry = input<StoreDataWithUrl>()
  readonly query = model('')
  readonly category = model('')

  protected readonly icons = ICONS
  protected readonly asIs = () => 0
  protected readonly open = signal(!inject(WA_IS_MOBILE))
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
}
