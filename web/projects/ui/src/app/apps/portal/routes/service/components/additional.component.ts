import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ToAdditionalPipe } from '../pipes/to-additional.pipe'
import { ServiceAdditionalItemComponent } from './additional-item.component'

@Component({
  selector: 'service-additional',
  template: `
    <h3 class="g-title">Additional Info</h3>
    <ng-container *ngFor="let additional of pkg | toAdditional">
      <a
        *ngIf="additional.description.startsWith('http'); else button"
        class="g-action"
        [additionalItem]="additional"
      ></a>
      <ng-template #button>
        <button
          class="g-action"
          [style.pointer-events]="!additional.icon ? 'none' : null"
          [additionalItem]="additional"
          (click)="additional.action?.()"
        ></button>
      </ng-template>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, ToAdditionalPipe, ServiceAdditionalItemComponent],
})
export class ServiceAdditionalComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry
}
