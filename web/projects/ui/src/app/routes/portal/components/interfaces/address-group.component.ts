import { TuiButton } from '@taiga-ui/core'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  inject,
} from '@angular/core'
import { AddressItemComponent } from './address-item.component'
import { AddressDetails, AddressesService } from './interface.utils'

@Component({
  standalone: true,
  selector: 'app-address-group',
  template: `
    <div>
      @if (addresses.length && !service.static) {
        <button
          class="icon-add-btn"
          tuiIconButton
          iconStart="@tui.plus"
          (click)="service.add()"
        ></button>
      }
      <ng-content />
    </div>
    @for (address of addresses; track $index) {
      <app-address-item [label]="address.label" [address]="address.url" />
    } @empty {
      @if (!service.static) {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.align-self]="'flex-start'"
          (click)="service.add()"
        >
          Add Address
        </button>
      }
    }
  `,
  imports: [AddressItemComponent, TuiButton],
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
