import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Exver, MarkdownPipe } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { TuiAccordion } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { MarketplacePkg } from '../../src/types'

@Component({
  template: `
    <tui-accordion>
      @for (note of notes | keyvalue: asIsOrder; track $index) {
        <tui-accordion-item>
          {{ note.key }}
          <ng-template tuiAccordionItemContent>
            <div [innerHTML]="note.value | markdown | dompurify"></div>
          </ng-template>
        </tui-accordion-item>
      }
    </tui-accordion>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, TuiAccordion, MarkdownPipe, NgDompurifyPipe],
})
export class ReleaseNotesComponent {
  private readonly exver = inject(Exver)
  private readonly pkg =
    injectContext<TuiDialogContext<void, MarketplacePkg>>().data

  readonly notes = Object.entries(this.pkg.otherVersions)
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

  asIsOrder(a: any, b: any) {
    return 0
  }
}

export const RELEASE_NOTES = new PolymorpheusComponent(ReleaseNotesComponent)
