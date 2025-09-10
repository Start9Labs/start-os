import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  output,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { DialogService, i18nPipe, SharedPipesModule } from '@start9labs/shared'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { TuiRadioList } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { MarketplaceItemComponent } from './item.component'

@Component({
  selector: 'marketplace-versions',
  template: `
    <div class="background-border shadow-color-light box-shadow-lg">
      <div class="box-container">
        <h2 class="additional-detail-title">{{ 'Versions' | i18n }}</h2>
        <marketplace-item
          (click)="promptSelectVersion(versionSelect)"
          [data]="'Select another version' | i18n"
          icon="@tui.chevron-right"
          [label]="null"
          class="select"
        />
        <ng-template
          #versionSelect
          let-data="data"
          let-completeWith="completeWith"
        >
          <tui-radio-list [items]="versions()" [(ngModel)]="data.version" />
          <footer class="buttons">
            <button
              tuiButton
              appearance="secondary"
              (click)="completeWith(null)"
            >
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
      </div>
    </div>
  `,
  styles: `
    .box-container {
      background-color: rgb(39 39 42);
      border-radius: 0.75rem;
      padding: 1.25rem 1.75rem;
    }

    .select {
      border: 0;
      // border-top-width: 1px;
      border-bottom-width: 1px;
      border-color: rgb(113 113 122);
      border-style: solid;
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
    SharedPipesModule,
    FormsModule,
    TuiRadioList,
    i18nPipe,
  ],
})
export class MarketplaceVersionsComponent {
  private readonly dialog = inject(DialogService)
  readonly version = input.required<string | null>()
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
