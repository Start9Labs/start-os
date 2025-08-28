import { Component, computed, input } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiFade } from '@taiga-ui/kit'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'service-card',
  template: `
    <tui-avatar><img alt="" [src]="service().icon" /></tui-avatar>
    <p tuiTitle="m">
      <span tuiFade>{{ manifest().title }}</span>
      <span tuiSubtitle class="g-secondary">{{ manifest().version }}</span>
    </p>
  `,
  styles: `
    :host {
      grid-column: span 2;
      padding: 1rem;
      align-items: center;
      justify-content: center;
      text-align: center;
      gap: 0.5rem;

      &::before {
        content: '';
        position: absolute;
        inset: 0;
        background: var(--background);
        background-size: 1px;
        mask: radial-gradient(circle at right bottom, black, transparent);
        opacity: 0.2;
      }

      [tuiTitle] {
        text-align: inherit;
        white-space: nowrap;
        max-width: 100%;
        gap: 0.5rem;
        font-weight: normal;
        line-height: normal;
      }
    }

    :host-context(tui-root._mobile) {
      flex-direction: row;
      justify-content: flex-start;
      text-align: left;
    }
  `,
  host: {
    class: 'g-card',
    '[style.--background]': '"url(" + service()?.icon + ")"',
  },
  imports: [TuiAvatar, TuiFade, TuiTitle],
})
export class ServiceCardComponent {
  public readonly service = input.required<PackageDataEntry>()

  protected readonly manifest = computed(() => getManifest(this.service()))
}
