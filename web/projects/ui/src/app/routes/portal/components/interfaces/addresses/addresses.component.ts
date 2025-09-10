import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiAccordion } from '@taiga-ui/experimental'
import { TuiSkeleton } from '@taiga-ui/kit'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'

import { MappedServiceInterface } from '../interface.service'
import { InterfaceAddressItemComponent } from './item.component'

@Component({
  selector: 'section[addresses]',
  template: `
    <header>{{ 'Addresses' | i18n }}</header>
    <table [appTable]="[null, 'Type', 'Access', 'Gateway', 'URL', null]">
      @for (address of addresses()?.common; track $index) {
        <tr [address]="address" [isRunning]="isRunning()"></tr>
      } @empty {
        @if (addresses()) {
          <tr>
            <td colspan="5">
              <app-placeholder icon="@tui.list-x">
                {{ 'No addresses' | i18n }}
              </app-placeholder>
            </td>
          </tr>
        } @else {
          @for (_ of [0, 1]; track $index) {
            <tr>
              <td colspan="6">
                <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
              </td>
            </tr>
          }
        }
      }
    </table>
    @if (addresses()?.uncommon?.length) {
      <tui-accordion>
        <tui-expand>
          <hr />
          <table class="g-table">
            @for (address of addresses()?.uncommon; track $index) {
              <tr [address]="address" [isRunning]="isRunning()"></tr>
            }
          </table>
        </tui-expand>
        <button
          appearance="secondary-grayscale"
          iconEnd=""
          [(tuiAccordion)]="uncommon"
        >
          @if (uncommon) {
            Hide uncommon
          } @else {
            Show uncommon
          }
        </button>
      </tui-accordion>
    }
  `,
  styles: `
    tui-accordion {
      border-radius: 0;
    }

    [tuiAccordion],
    tui-expand {
      box-shadow: none;
      padding: 0;
    }

    [tuiAccordion] {
      justify-content: center;
      height: 3rem;
      border-radius: 0 0 var(--tui-radius-m) var(--tui-radius-m) !important;
    }

    hr {
      margin: 0;
      height: 0.25rem;
      border-radius: 1rem;
    }

    :host-context(tui-root._mobile) {
      [tuiAccordion] {
        margin: 0.5rem 0;
        border-radius: var(--tui-radius-m) !important;
      }
    }
  `,
  host: { class: 'g-card' },
  imports: [
    TableComponent,
    PlaceholderComponent,
    i18nPipe,
    InterfaceAddressItemComponent,
    TuiAccordion,
    TuiSkeleton,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressesComponent {
  readonly addresses = input.required<
    MappedServiceInterface['addresses'] | undefined
  >()
  readonly isRunning = input.required<boolean>()

  uncommon = false
}
