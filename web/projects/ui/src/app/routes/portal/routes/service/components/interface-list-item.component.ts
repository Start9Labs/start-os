import { TuiLet } from '@taiga-ui/cdk'
import { TuiLoader, TuiIcon, TuiButton } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { map, timer } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { ExtendedInterfaceInfo } from '../pipes/interface-info.pipe'

@Component({
  selector: 'a[serviceInterfaceListItem]',
  template: `
    <ng-container *tuiLet="healthCheck$ | async as check">
      @if (check === null) {
        <tui-loader />
      } @else if (check === '') {
        <tui-icon [icon]="info.icon" [style.color]="info.color" />
      } @else {
        <tui-icon icon="@tui.circle-x" class="g-error" />
      }
      <div [style.flex]="1">
        <strong>{{ info.name }}</strong>
        <div>{{ info.description }}</div>
        @if (check) {
          <div class="g-error">
            <b>Health check failed:</b>
            {{ check }}
          </div>
        } @else {
          <div [style.color]="info.color">{{ info.typeDetail }}</div>
        }
      </div>
      @if (info.type === 'ui') {
        <a
          tuiIconButton
          appearance="flat"
          iconStart="@tui.external-link"
          target="_blank"
          rel="noreferrer"
          title="Open"
          [style.border-radius.%]="100"
          [attr.href]="href"
          (click.stop)="(0)"
        ></a>
      }
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiButton, TuiLet, TuiLoader, TuiIcon],
})
export class ServiceInterfaceListItemComponent {
  private readonly config = inject(ConfigService)

  @Input({ required: true, alias: 'serviceInterfaceListItem' })
  info!: ExtendedInterfaceInfo

  @Input()
  disabled = false

  // TODO: Implement real health check
  readonly healthCheck$ = timer(3000).pipe(
    map(() => (Math.random() > 0.5 ? '' : 'You done f***d it up...')),
  )

  get href(): string | null {
    return this.disabled ? null : this.config.launchableAddress(this.info)
  }
}
