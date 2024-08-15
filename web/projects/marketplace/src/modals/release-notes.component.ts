import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { Exver, MarkdownPipeModule } from '@start9labs/shared'
import { TuiButton, TuiLoader } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { map } from 'rxjs'
import { AbstractMarketplaceService } from '../services/marketplace.service'
import { MarketplacePkg } from '../types'

@Component({
  standalone: true,
  template: `
    @if (notes$ | async; as notes) {
      @for (note of notes | keyvalue: asIsOrder; track $index) {
        <button tuiButton (click)="setSelected(note.key)">
          {{ note.key }}
        </button>
        <div
          tuiCardLarge
          #element
          [id]="note.key"
          [style.max-height.px]="getDocSize(note.key, element)"
          [innerHTML]="note.value | markdown"
        ></div>
      }
    } @else {
      <tui-loader textContent="Loading Release Notes" />
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButton,
    TuiLoader,
    TuiCardLarge,
    MarkdownPipeModule,
  ],
})
export class ReleaseNotesComponent {
  @Input() pkg!: MarketplacePkg

  private selected: string | null = null
  private readonly exver = inject(Exver)

  readonly notes$ = inject(AbstractMarketplaceService)
    .getSelectedStore$()
    .pipe(
      map(s => {
        return Object.entries(this.pkg.otherVersions)
          .filter(
            ([v, _]) =>
              this.exver.getFlavor(v) === this.pkg.flavor &&
              this.exver.compareExver(this.pkg.version, v) === 1,
          )
          .reduce(
            (obj, [version, info]) => ({
              ...obj,
              [version]: info.releaseNotes,
            }),
            {
              [`${this.pkg.version} (current)`]: this.pkg.releaseNotes,
            },
          )
      }),
    )

  isSelected(key: string): boolean {
    return this.selected === key
  }

  setSelected(selected: string) {
    this.selected = this.isSelected(selected) ? null : selected
  }

  getDocSize(key: string, { scrollHeight }: HTMLElement) {
    return this.isSelected(key) ? scrollHeight : 0
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}

export const RELEASE_NOTES = new PolymorpheusComponent(ReleaseNotesComponent)
