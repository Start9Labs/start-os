import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'
import { TuiAvatar, TuiAvatarLabeled, TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'
import { buildConnectionPath } from 'src/app/routes/outbound/utils'
import OutboundVPN from 'src/app/routes/outbound/routes/vpn/index'

@Component({
  selector: '[vpnSummary]',
  template: `
    <section>
      <label appSummary>
        Status
        <span tuiSubtitle>
          @if (parent.data()?.enabled) {
            <span tuiBadge tuiStatus appearance="positive">Connected</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </label>
      <label appSummary>
        Used by
        <span tuiSubtitle>{{ parent.data()?.used_by?.join(', ') || '-' }}</span>
      </label>
    </section>
    <section>
      <label appSummary>
        Connection path
        <span tuiSubtitle>
          @for (step of connectionPath(); track $index; let last = $last) {
            @if (step === 'Internet') {
              <tui-avatar-labeled label="Internet">
                <span
                  tuiAvatar="@tui.globe"
                  size="m"
                  appearance="action"
                  class="g-positive"
                ></span>
              </tui-avatar-labeled>
            } @else {
              <tui-avatar-labeled [label]="step">
                <span
                  appearance="action"
                  size="m"
                  tuiAvatar="@tui.globe-lock"
                ></span>
              </tui-avatar-labeled>
              <tui-icon icon="@tui.arrow-right" />
            }
          }
        </span>
      </label>
    </section>
  `,
  styles: `
    tui-avatar {
      margin: 0;
    }
  `,
  imports: [TuiBadge, TuiStatus, Summary, TuiAvatarLabeled, TuiAvatar, TuiIcon],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class VPNSummary {
  protected readonly parent = inject(OutboundVPN)

  readonly connectionPath = computed(() => {
    const data = this.parent.data()
    const allVpns = this.parent.service.data() ?? []
    if (!data || allVpns.length === 0) {
      return ['-', 'Internet']
    }
    return buildConnectionPath(data.label, allVpns)
  })
}
