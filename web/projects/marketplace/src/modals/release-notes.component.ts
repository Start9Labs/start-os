import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Exver, MarkdownPipeModule } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiLoader } from '@taiga-ui/core'
import { TuiAccordion } from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { map } from 'rxjs'
import { AbstractMarketplaceService } from '../services/marketplace.service'
import { MarketplacePkg } from '../../src/types'

@Component({
  standalone: true,
  template: `
    @if (notes$ | async; as notes) {
      <tui-accordion>
        @for (note of notes | keyvalue: asIsOrder; track $index) {
          <tui-accordion-item>
            {{ note.key }}
            <ng-template tuiAccordionItemContent>
              <div [innerHTML]="note.value | markdown"></div>
            </ng-template>
          </tui-accordion-item>
        }
      </tui-accordion>
    } @else {
      <tui-loader textContent="Loading Release Notes" />
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiButton,
    TuiLoader,
    TuiAccordion,
    MarkdownPipeModule,
  ],
})
export class ReleaseNotesComponent {
  private readonly exver = inject(Exver)
  private readonly pkg =
    inject<TuiDialogContext<void, MarketplacePkg>>(POLYMORPHEUS_CONTEXT).data

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

  asIsOrder(a: any, b: any) {
    return 0
  }
}

export const RELEASE_NOTES = new PolymorpheusComponent(ReleaseNotesComponent)
