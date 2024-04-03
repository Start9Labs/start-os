import { ChangeDetectionStrategy, Component } from '@angular/core'
import {
  AbstractTuiPortalHostComponent,
  AbstractTuiPortalService,
} from '@taiga-ui/cdk'
import { MarketplaceSidebarService } from '../services/sidebar.service'

@Component({
  standalone: true,
  selector: 'marketplace-sidebars',
  template: '<ng-container #viewContainer></ng-container>',
  styles: [
    `
      :host {
        position: fixed;
        inset: 3.5rem 0 0;
        pointer-events: none;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    {
      provide: AbstractTuiPortalService,
      useExisting: MarketplaceSidebarService,
    },
  ],
})
export class MarketplaceSidebarsComponent extends AbstractTuiPortalHostComponent {}
