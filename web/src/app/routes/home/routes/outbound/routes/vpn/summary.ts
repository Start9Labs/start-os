import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiIcon, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiAvatarLabeled, TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary } from 'src/app/components/summary'
import OutboundVPN from 'src/app/routes/home/routes/outbound/routes/vpn/index'

@Component({
  selector: '[vpnSummary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        Status
        <span tuiSubtitle>
          <span tuiBadge tuiStatus appearance="positive">Connected</span>
        </span>
      </label>
      <label appSummary>
        Label
        <span tuiSubtitle>{{ parent.form.value.label || '-' }}</span>
      </label>
      <label appSummary>
        Type
        <span tuiSubtitle>OpenVPN</span>
      </label>
      <label appSummary>
        Used by
        <span tuiSubtitle>
          <a tuiLink iconStart="@tui.scroll-text">Child</a>
        </span>
      </label>
    </section>
    <section>
      <label appSummary [style.flex]="1">
        Connection path
        <span tuiSubtitle>
          <tui-avatar-labeled [label]="parent.form.value.label || '-'">
            <tui-avatar appearance="action" size="m" src="@tui.globe-lock" />
          </tui-avatar-labeled>
          <tui-icon icon="@tui.arrow-right" />
          @if (parent.form.value.chaining) {
            <tui-avatar-labeled [label]="parent.form.value.vpn || '-'">
              <tui-avatar appearance="action" size="m" src="@tui.globe-lock" />
            </tui-avatar-labeled>
            <tui-icon icon="@tui.arrow-right" />
          }
          <tui-avatar-labeled label="Internet">
            <tui-avatar
              src="@tui.globe"
              size="m"
              appearance="action"
              class="g-positive"
            />
          </tui-avatar-labeled>
        </span>
      </label>
    </section>
  `,
  styles: `
    tui-avatar {
      margin: 0;
    }
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [
    AsyncPipe,
    TuiHeader,
    TuiTitle,
    TuiBadge,
    TuiStatus,
    Summary,
    TuiLink,
    TuiAvatarLabeled,
    TuiAvatar,
    TuiIcon,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class VPNSummary {
  protected readonly parent = inject(OutboundVPN)
}
