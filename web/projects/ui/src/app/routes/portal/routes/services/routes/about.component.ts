import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { getPkgId } from '@start9labs/shared'
import { TuiCell } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ServiceAdditionalItemComponent } from '../components/additional-item.component'
import { ToAdditionalPipe } from '../pipes/to-additional.pipe'

@Component({
  template: `
    @if (package(); as pkg) {
      <section class="g-card">
        @for (additional of pkg | toAdditional; track $index) {
          @if (additional.description.startsWith('http')) {
            <a tuiCell [additionalItem]="additional"></a>
          } @else {
            <button
              tuiCell
              [style.pointer-events]="!additional.icon ? 'none' : null"
              [additionalItem]="additional"
              (click)="additional.action?.()"
            ></button>
          }
        }
      </section>
    }
  `,
  styles: `
    section {
      display: flex;
      flex-direction: column;
      max-width: 32rem;
      padding: 0.75rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  host: { class: 'g-subpage' },
  imports: [ToAdditionalPipe, ServiceAdditionalItemComponent, TuiCell],
})
export default class ServiceAboutRoute {
  readonly package = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('packageData', getPkgId()),
  )
}
