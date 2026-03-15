import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { tuiButtonOptionsProvider } from '@taiga-ui/core'
import { MappedServiceInterface } from './interface.service'
import { InterfaceAddressesComponent } from './addresses/addresses.component'
import { PluginAddressesComponent } from './addresses/plugin.component'

@Component({
  selector: 'service-interface',
  template: `
    @for (group of value()?.gatewayGroups; track group.gatewayId) {
      <section
        [gatewayGroup]="group"
        [packageId]="packageId()"
        [value]="value()"
        [isRunning]="isRunning()"
      ></section>
    }
    @for (group of value()?.pluginGroups; track group.pluginId) {
      <section
        [pluginGroup]="group"
        [packageId]="packageId()"
        [value]="value()"
      ></section>
    }
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
      color: var(--tui-text-secondary);
      font: var(--tui-typography-body-l);

      ::ng-deep [tuiSkeleton] {
        width: 100%;
        height: 1rem;
        border-radius: var(--tui-radius-s);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiButtonOptionsProvider({ size: 'xs' })],
  imports: [InterfaceAddressesComponent, PluginAddressesComponent],
})
export class InterfaceComponent {
  readonly packageId = input('')
  readonly value = input.required<MappedServiceInterface | undefined>()
  readonly isRunning = input.required<boolean>()
}
