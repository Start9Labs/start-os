import { TuiDropdownService } from '@taiga-ui/core'
import {
  ChangeDetectionStrategy,
  Component,
  Directive,
  Injectable,
} from '@angular/core'
import { TuiPortals, TuiPortalService } from '@taiga-ui/cdk'

@Injectable({ providedIn: `root` })
export class SidebarService extends TuiPortalService {}

@Directive({
  selector: '[tuiSidebar]',
  standalone: true,
  providers: [{ provide: TuiDropdownService, useExisting: SidebarService }],
})
export class SidebarDirective {}

@Component({
  selector: 'sidebar-host',
  template: '<ng-container #viewContainer></ng-container>',
  styles: [':host { position: fixed; top: 0; }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  providers: [{ provide: TuiPortalService, useExisting: SidebarService }],
})
export class SidebarHostComponent extends TuiPortals {}
