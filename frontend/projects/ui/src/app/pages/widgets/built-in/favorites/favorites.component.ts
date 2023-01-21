import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'widget-favorites',
  templateUrl: './favorites.component.html',
  styleUrls: ['./favorites.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FavoritesComponent {}
