import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'general-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Preferences</button>
      <tui-expand>
        <h3 [style.margin-block-start]="0">Theme</h3>
        Choose from dark or light theme, or keep your system settings applied to
        the UI.
        <h3>Timezone</h3>
        Configure the router’s timezone based on your location. Schedules,
        activity, and logs will be based on this setting.
        <h3>Week Start Day</h3>
        Configure what day of the week to start schedules.
        <h3>Language</h3>
        Configure the preferred language for the user interface of the router.
        This setting affects the language displayed for all menus, options, and
        other text elements within the router’s web interface.
      </tui-expand>
      <button tuiAccordion appearance="">Updates</button>
      <tui-expand>
        Enable automatic updates at the designed time. The router will
        experience brief downtime during this process.
      </tui-expand>
      <button tuiAccordion appearance="">Power</button>
      <tui-expand>
        Controls for restarting or shutting down the router. Note that devices
        will not be able to connect to the Internet during these actions.
      </tui-expand>
      <button tuiAccordion appearance="">Advanced</button>
      <tui-expand>
        Advanced configuration options, use with caution. A factory reset will
        remove all custom configurations.
        <p>
          The LuCI web interface exposes additional advanced options for
          configuring and managing the router.
        </p>
      </tui-expand>
    </tui-accordion>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAccordion],
})
export class GeneralAside {}
