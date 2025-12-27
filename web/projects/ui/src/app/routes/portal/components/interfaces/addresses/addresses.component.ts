import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { TuiAccordion } from '@taiga-ui/experimental'
import { TuiElasticContainer, TuiSkeleton } from '@taiga-ui/kit'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'

import { MappedServiceInterface } from '../interface.service'
import { InterfaceAddressItemComponent } from './item.component'

@Component({
  selector: 'section[addresses]',
  template: `
    <header>{{ 'Addresses' | i18n }}</header>
    <tui-elastic-container>
      <table [appTable]="['Type', 'Access', 'Gateway', 'URL', null]">
        <th [style.width.rem]="2"></th>
        @for (address of addresses()?.common; track $index) {
          <tr [address]="address" [isRunning]="isRunning()"></tr>
        } @empty {
          @if (addresses()) {
            <tr>
              <td colspan="6">
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
        <tbody [class.uncommon-hidden]="!uncommon">
          @if (addresses()?.uncommon?.length && uncommon) {
            <tr [style.background]="'var(--tui-background-neutral-1)'">
              <td colspan="6"></td>
            </tr>
          }
          @for (address of addresses()?.uncommon; track $index) {
            <tr [address]="address" [isRunning]="isRunning()"></tr>
          }
        </tbody>
        @if (addresses()?.uncommon?.length) {
          <caption [style.caption-side]="'bottom'">
            <button
              tuiButton
              size="m"
              appearance="secondary-grayscale"
              (click)="uncommon = !uncommon"
            >
              @if (uncommon) {
                Hide uncommon
              } @else {
                Show uncommon
              }
            </button>
          </caption>
        }
      </table>
    </tui-elastic-container>
  `,
  styles: `
    .g-table:has(caption) {
      border-bottom-left-radius: 0;
      border-bottom-right-radius: 0;
    }

    [tuiButton] {
      width: 100%;
      border-top-left-radius: 0;
      border-top-right-radius: 0;
    }

    :host-context(tui-root._mobile) {
      [tuiButton] {
        border-radius: var(--tui-radius-xs);
        margin-block-end: 0.75rem;
      }
    }
  `,
  host: { class: 'g-card' },
  imports: [
    TuiSkeleton,
    TuiButton,
    TableComponent,
    PlaceholderComponent,
    i18nPipe,
    InterfaceAddressItemComponent,
    TuiElasticContainer,
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
