import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  inject,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  TUI_DEFAULT_MATCHER,
  TuiActiveZoneModule,
  TuiFilterPipeModule,
  TuiForModule,
} from '@taiga-ui/cdk'
import { TuiSvgModule, TuiTextfieldControllerModule } from '@taiga-ui/core'
import { TuiInputModule } from '@taiga-ui/kit'
import { map } from 'rxjs'
import { CardComponent } from '../card/card.component'
import { NavigationItem } from '../navigation/navigation.service'
import { ServicesService } from '../../services/services.service'
import { SYSTEM_UTILITIES } from './drawer.const'
import { toNavigationItem } from '../../utils/to-navigation-item'

@Component({
  selector: 'app-drawer',
  templateUrl: 'drawer.component.html',
  styleUrls: ['drawer.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    TuiSvgModule,
    TuiActiveZoneModule,
    TuiInputModule,
    TuiTextfieldControllerModule,
    TuiForModule,
    TuiFilterPipeModule,
    CardComponent,
    RouterLink,
  ],
})
export class DrawerComponent {
  @HostBinding('class._open')
  open = false

  search = ''

  readonly system = SYSTEM_UTILITIES
  readonly services$ = inject(ServicesService).pipe(
    map(services => services.map(toNavigationItem)),
  )

  readonly bySearch = (item: NavigationItem, search: string): boolean =>
    search.length < 2 || TUI_DEFAULT_MATCHER(item.title, search)
}
