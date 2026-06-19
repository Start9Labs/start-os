import { Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ActionSuccessMemberComponent } from 'src/app/routes/portal/routes/services/modals/action-success/action-success-member.component'

export type PortRangeForwardData = {
  gatewayName: string
  range: string
}

@Component({
  selector: 'port-range-forward',
  template: `
    <p>
      {{ 'In your gateway' | i18n }} "{{ context.data.gatewayName }}",
      {{
        'forward the following port range to this server, for both TCP and UDP, if you have not already'
          | i18n
      }}:
    </p>
    <app-action-success-member [member]="member" />
    <footer class="g-buttons">
      <button tuiButton (click)="context.completeWith()">
        {{ 'Done' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    footer {
      margin-top: 1.5rem;
    }
  `,
  imports: [TuiButton, i18nPipe, ActionSuccessMemberComponent],
})
export class PortRangeForwardComponent {
  private readonly i18n = inject(i18nPipe)

  readonly context =
    injectContext<TuiDialogContext<void, PortRangeForwardData>>()

  protected readonly member: T.ActionResultMember & { type: 'single' } = {
    type: 'single',
    name: this.i18n.transform('Range'),
    description: null,
    value: this.context.data.range,
    copyable: true,
    qr: false,
    masked: false,
  }
}

export const PORT_RANGE_FORWARD = new PolymorpheusComponent(
  PortRangeForwardComponent,
)
