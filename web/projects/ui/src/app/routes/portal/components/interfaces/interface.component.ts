import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { tuiButtonOptionsProvider } from '@taiga-ui/core'
import { MappedServiceInterface } from './interface.service'
import { InterfaceGatewaysComponent } from './gateways.component'
import { InterfaceTorDomainsComponent } from './tor-domains.component'
import { PublicDomainsComponent } from './public-domains/pd.component'
import { InterfacePrivateDomainsComponent } from './private-domains.component'
import { InterfaceAddressesComponent } from './addresses/addresses.component'

// @TODO translations

@Component({
  selector: 'service-interface',
  template: `
    <div>
      <section
        [gateways]="value()?.gateways"
        [isOs]="!!value()?.isOs"
      ></section>
      <section [publicDomains]="value()?.publicDomains"></section>
      <section [torDomains]="value()?.torDomains"></section>
      <section [privateDomains]="value()?.privateDomains"></section>
    </div>
    <hr [style.width.rem]="10" />
    <section [addresses]="value()?.addresses" [isRunning]="true"></section>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
      color: var(--tui-text-secondary);
      font: var(--tui-font-text-l);

      div {
        display: grid;
        grid-template-columns: repeat(6, 1fr);
        gap: inherit;
        flex-direction: column;
      }

      ::ng-deep [tuiSkeleton] {
        width: 100%;
        height: 1rem;
        border-radius: var(--tui-radius-s);
      }
    }

    :host-context(tui-root._mobile) div {
      display: flex;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiButtonOptionsProvider({ size: 'xs' })],
  imports: [
    InterfaceGatewaysComponent,
    InterfaceTorDomainsComponent,
    PublicDomainsComponent,
    InterfacePrivateDomainsComponent,
    InterfaceAddressesComponent,
  ],
})
export class InterfaceComponent {
  readonly packageId = input('')
  readonly value = input.required<MappedServiceInterface | undefined>()
  readonly isRunning = input.required<boolean>()
}
