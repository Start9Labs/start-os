import { I18nPluralPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { TuiButtonModule } from '@taiga-ui/experimental'

@Component({
  selector: 'service-backups',
  template: `
    <div [style.flex]="1">
      <small>Last backup</small>
      {{ previous() | i18nPlural: ago }}
    </div>
    <div [style.flex]="1">
      <small>Next backup</small>
      {{ next() | i18nPlural: in }}
    </div>
    <div [style.min-width.%]="100">
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
      padding-bottom: 1rem;

      small {
        display: block;
        text-transform: uppercase;
        color: var(--tui-text-02);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule, RouterLink, I18nPluralPipe],
})
export class ServiceBackupsComponent {
  pkg = input.required<T.PackageDataEntry>()

  readonly previous = computed(() =>
    daysBetween(new Date(), new Date(this.pkg().lastBackup || new Date())),
  )

  readonly next = computed(() =>
    // TODO @lucy add this back in when types fixed for PackageDataEntry ie. when next/minor merge resolved
    // daysBetween(new Date(), new Date(this.pkg().nextBackup || new Date())),
    daysBetween(new Date(), new Date(new Date())),
  )

  readonly ago = {
    '=0': 'Never performed',
    '=1': 'day ago',
    other: '# days ago',
  }

  readonly in = {
    '=0': 'Not scheduled',
    '=1': 'Tomorrow',
    other: 'In # days',
  }
}

function daysBetween(one: Date, two: Date): number {
  return Math.abs(
    Math.round((one.valueOf() - two.valueOf()) / (1000 * 60 * 60 * 24)),
  )
}
