import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiDialogOptions } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ToAdditionalPipe } from 'src/app/routes/portal/routes/service/pipes/to-additional.pipe'
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
  readonly pkg =
    inject<TuiDialogOptions<PackageDataEntry>>(POLYMORPHEUS_CONTEXT).data
}
