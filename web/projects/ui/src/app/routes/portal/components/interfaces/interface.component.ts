import { ChangeDetectionStrategy, Component, input, Input } from '@angular/core'
import { tuiButtonOptionsProvider } from '@taiga-ui/core'
import { InterfaceClearnetComponent } from 'src/app/routes/portal/components/interfaces/clearnet.component'
import { InterfaceLocalComponent } from 'src/app/routes/portal/components/interfaces/local.component'
import { InterfaceTorComponent } from 'src/app/routes/portal/components/interfaces/tor.component'
import { MappedServiceInterface } from './interface.utils'

@Component({
  standalone: true,
  selector: 'app-interface',
  template: `
    <section [clearnet]="serviceInterface().addresses.clearnet"></section>
    <section [tor]="serviceInterface().addresses.tor"></section>
    <section [local]="serviceInterface().addresses.local"></section>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }
  `,
  providers: [tuiButtonOptionsProvider({ size: 'xs' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    InterfaceClearnetComponent,
    InterfaceTorComponent,
    InterfaceLocalComponent,
  ],
})
export class InterfaceComponent {
  readonly packageId = input('')
  readonly serviceInterface = input.required<MappedServiceInterface>()
}
