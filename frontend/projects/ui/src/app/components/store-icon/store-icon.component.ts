import {
  ChangeDetectionStrategy,
  Component,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'

@Component({
  selector: 'store-icon',
  templateUrl: './store-icon.component.html',
  styleUrls: ['./store-icon.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class StoreIconComponent {
  @Input() url: string = ''
}

@Pipe({
  name: 'getIcon',
})
export class GetIconPipe implements PipeTransform {
  transform(url: string): string | null {
    if (url === 'https://registry.start9.com/') {
      return 'assets/img/icon.png'
    } else if (url === 'https://community-registry.start9.com/') {
      return 'assets/img/community-store.png'
    }
    return null
  }
}
