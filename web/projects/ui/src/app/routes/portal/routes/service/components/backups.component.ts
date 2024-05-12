import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiButtonModule } from '@taiga-ui/experimental'

@Component({
  selector: 'service-backups',
  template: `
    <div>
      <small>Last backup</small>
      6 days ago
    </div>
    <div>
      <small>Next backup</small>
      Not scheduled
    </div>
    <div>
      <a
        tuiButton
        iconLeft="tuiIconPlusSquare"
        routerLink="/portal/system/backups"
        size="s"
        appearance="secondary-warning"
      >
        Manage
      </a>
    </div>
  `,
  styles: `
    :host {
      display: flex;
      gap: 1rem;
      flex-wrap: wrap;
      white-space: nowrap;

      > :last-child {
        min-width: 100%;
        padding-bottom: 1rem;
      }

      small {
        display: block;
        text-transform: uppercase;
        color: var(--tui-text-02);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule, RouterLink],
})
export class ServiceBackupsComponent {}
