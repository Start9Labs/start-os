import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { ConfigService } from 'src/app/services/config.service'
import { ExtendedInterfaceInfo } from '../pipes/interface-info.pipe'

@Component({
  selector: 'a[serviceInterfaceListItem]',
  template: `
    <tui-svg [src]="info.icon" [style.color]="info.color"></tui-svg>
    <div [style.flex]="1">
      <strong>{{ info.name }}</strong>
      <div>{{ info.description }}</div>
      <div [style.color]="info.color">{{ info.typeDetail }}</div>
    </div>
    <a
      *ngIf="info.type === 'ui'"
      tuiIconButton
      appearance="flat"
      iconLeft="tuiIconExternalLinkLarge"
      target="_blank"
      rel="noreferrer"
      [style.border-radius.%]="100"
      [attr.href]="href"
      (click.stop)="(0)"
    ></a>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule, CommonModule, TuiSvgModule],
})
export class ServiceInterfaceListItemComponent {
  private readonly config = inject(ConfigService)

  @Input({ required: true, alias: 'serviceInterfaceListItem' })
  info!: ExtendedInterfaceInfo

  @Input()
  disabled = false

  get href(): string | null {
    return this.disabled ? null : this.config.launchableAddress(this.info)
  }
}
