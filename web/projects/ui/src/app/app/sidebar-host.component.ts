import {
  ChangeDetectionStrategy,
  Component,
  Directive,
  Injectable,
} from '@angular/core'
import {
  AbstractTuiPortalHostComponent,
  AbstractTuiPortalService,
  TuiDropdownPortalService,
} from '@taiga-ui/cdk'

@Injectable({ providedIn: `root` })
export class SidebarService extends AbstractTuiPortalService {}

@Directive({
  selector: '[tuiSidebar]',
  standalone: true,
  providers: [
    { provide: TuiDropdownPortalService, useExisting: SidebarService },
  ],
})
export class SidebarDirective {}

@Component({
  selector: 'sidebar-host',
  template: '<ng-container #viewContainer></ng-container>',
  styles: [
    `
      :host {
        position: fixed;
        display: flex;
        flex-direction: column;
        align-items: center;
        top: 0;
        height: 0;
        width: 100%;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  providers: [
    { provide: AbstractTuiPortalService, useExisting: SidebarService },
  ],
})
export class SidebarHostComponent extends AbstractTuiPortalHostComponent {}
