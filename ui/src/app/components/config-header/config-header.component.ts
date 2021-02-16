import { Component, Input } from '@angular/core'
import { ValueSpec } from 'src/app/pkg-config/config-types'

@Component({
  selector: 'config-header',
  templateUrl: './config-header.component.html',
  styleUrls: ['./config-header.component.scss'],
})
export class ConfigHeaderComponent {
  @Input() spec: ValueSpec
  @Input() error: string
}
