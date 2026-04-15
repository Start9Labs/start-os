import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  output,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { DialogService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { TuiRadioList } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { MarketplaceItemComponent } from './item.component'

@Component({
  selector: 'marketplace-versions',
  template: `
    <marketplace-item
      (click)="promptSelectVersion(versionSelect)"
      [data]="version()"
      label="Version"
      icon="@tui.chevron-right"
      class="item-pointer"
    />
    <ng-template #versionSelect let-data="data" let-completeWith="completeWith">
      <tui-radio-list [items]="versions()" [(ngModel)]="data.version" />
      <footer class="g-buttons">
        <button tuiButton appearance="secondary" (click)="completeWith(null)">
          {{ 'Cancel' | i18n }}
        </button>
        <button
          tuiButton
          appearance="secondary"
          (click)="completeWith(data.version)"
        >
          {{ 'Ok' | i18n }}
        </button>
      </footer>
    </ng-template>
  `,
  styles: `
    :host {
      display: contents;
    }

    .item-pointer:hover {
      cursor: pointer;

      ::ng-deep label {
        cursor: pointer;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    MarketplaceItemComponent,
    TuiButton,
    FormsModule,
    TuiRadioList,
    i18nPipe,
  ],
})
export class MarketplaceVersionsComponent {
  private readonly dialog = inject(DialogService)
  readonly version = input.required<string>()
  readonly versions = input.required<string[]>()

  onVersion = output<string>()

  promptSelectVersion(template: TemplateRef<TuiDialogContext>) {
    this.dialog
      .openComponent<string>(template, {
        label: 'All versions',
        size: 's',
        data: { version: this.version() },
      })
      .pipe(filter(Boolean))
      .subscribe(selected => this.onVersion.emit(selected))
  }
}
