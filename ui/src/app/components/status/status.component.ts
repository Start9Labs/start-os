import { Component, Input } from '@angular/core'
import { PkgStatusRendering } from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'status',
  templateUrl: './status.component.html',
  styleUrls: ['./status.component.scss'],
})
export class StatusComponent {
  @Input() rendering: PkgStatusRendering
  @Input() size?: 'small' | 'medium' | 'large' | 'x-large' = 'large'
  @Input() style?: string = 'regular'
  @Input() weight?: string = 'normal'
  @Input() disconnected?: boolean = false
}

