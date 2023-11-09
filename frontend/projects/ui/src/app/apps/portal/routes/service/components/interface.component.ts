import { DOCUMENT, CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { ConfigService } from 'src/app/services/config.service'
import { InterfaceInfo } from 'src/app/services/patch-db/data-model'
import { ExtendedInterfaceInfo } from '../pipes/interface-info.pipe'

@Component({
  selector: 'a[serviceInterface]',
  template: `
    <tui-svg [src]="info.icon" [style.color]="info.color"></tui-svg>
    <div [style.flex]="1">
      <strong>{{ info.name }}</strong>
      <div>{{ info.description }}</div>
      <div [style.color]="info.color">{{ info.typeDetail }}</div>
    </div>
    <button
      *ngIf="info.type === 'ui'"
      tuiIconButton
      appearance="flat"
      iconLeft="tuiIconExternalLinkLarge"
      [style.border-radius.%]="100"
      (click.stop.prevent)="launchUI(info)"
      [disabled]="disabled"
    ></button>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule, CommonModule, TuiSvgModule],
})
export class ServiceInterfaceComponent {
  private readonly document = inject(DOCUMENT)
  private readonly config = inject(ConfigService)

  @Input({ required: true, alias: 'serviceInterface' })
  info!: ExtendedInterfaceInfo

  @Input()
  disabled = false

  launchUI(info: InterfaceInfo) {
    this.document.defaultView?.open(
      this.config.launchableAddress(info),
      '_blank',
      'noreferrer',
    )
  }
}
