import {
  ChangeDetectionStrategy,
  Component,
  ViewEncapsulation,
} from '@angular/core'
import { AbstractTuiThemeSwitcher } from '@taiga-ui/cdk'

@Component({
  selector: 'light-theme',
  template: '',
  styleUrls: ['./light-theme.component.scss'],
  encapsulation: ViewEncapsulation.None,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LightThemeComponent extends AbstractTuiThemeSwitcher {}
