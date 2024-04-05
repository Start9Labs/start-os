import {
  ChangeDetectionStrategy,
  Component,
  Input,
  inject,
} from '@angular/core'
import { AddressItemComponent } from './address-item.component'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { AddressDetails, AddressesService } from './interface.utils'

@Component({
  standalone: true,
  selector: 'app-address-group',
  template: `
    <div>
      @if (addresses.length) {
        <button
          class="icon-add-btn"
          tuiIconButton
          appearance="secondary"
          iconLeft="tuiIconPlus"
          (click)="service.add()"
        >
          Add
        </button>
      }
      <ng-content></ng-content>
    </div>
    @for (address of addresses; track $index) {
      <app-address-item [label]="address.label" [address]="address.url" />
    } @empty {
      <button
        tuiButton
        iconLeft="tuiIconPlus"
        [style.align-self]="'flex-start'"
        (click)="service.add()"
      >
        Add Address
      </button>
    }
  `,
  imports: [AddressItemComponent, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
  styles: `
    .icon-add-btn {
      float: right;
      margin-left: 2rem;
    }
  `,
})
export class AddressGroupComponent {
  readonly service = inject(AddressesService)

  @Input({ required: true }) addresses!: AddressDetails[]
}
