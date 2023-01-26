import {
  ChangeDetectionStrategy,
  Component,
  ViewEncapsulation,
} from '@angular/core'
import { AbstractTuiThemeSwitcher } from '@taiga-ui/cdk'

@Component({
  selector: 'night-theme',
  template: '',
  styleUrls: ['./night-theme.component.scss'],
  encapsulation: ViewEncapsulation.None,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class NightThemeComponent extends AbstractTuiThemeSwitcher {}
