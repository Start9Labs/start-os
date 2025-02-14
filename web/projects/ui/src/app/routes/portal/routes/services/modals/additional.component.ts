import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiDialogOptions } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'
import { ToAdditionalPipe } from 'src/app/routes/portal/routes/services/pipes/to-additional.pipe'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ServiceAdditionalItemComponent } from './additional-item.component'

@Component({
  selector: 'service-additional',
  template: `
    @for (additional of pkg | toAdditional; track $index) {
      @if (additional.description.startsWith('http')) {
        <a class="g-action" [additionalItem]="additional"></a>
      } @else {
        <button
          class="g-action"
          [style.pointer-events]="!additional.icon ? 'none' : null"
          [additionalItem]="additional"
          (click)="additional.action?.()"
        ></button>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ToAdditionalPipe, ServiceAdditionalItemComponent],
})
export class ServiceAdditionalModal {
  readonly pkg = injectContext<TuiDialogOptions<PackageDataEntry>>().data
}
