import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiButtonModule } from '@taiga-ui/core'
import { TuiActiveZoneModule } from '@taiga-ui/cdk'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import { ItemModule, MarketplacePkg } from '@start9labs/marketplace'
import { MarketplaceShowComponentsModule } from '../../marketplace-show-preview/components/marketplace-show-components.module'
import { MarketplaceShowPreviewModule } from '../../marketplace-show-preview/marketplace-show-preview.module'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { filter, Observable, shareReplay } from 'rxjs'
import { PatchDB } from 'patch-db-client'

@Component({
  selector: 'marketplace-item-toggle',
  template: `
    <div
      [id]="pkg.manifest.id"
      class="items-center gap-3 h-full"
      (click)="toggle(true)"
      (tuiActiveZoneChange)="toggle($event)"
    >
      <marketplace-item [pkg]="pkg"></marketplace-item>
      <marketplace-show-preview
        [pkg]="pkg"
        *tuiSidebar="open; direction: 'right'; autoWidth: true"
        class="overflow-y-auto max-w-full md:max-w-[30rem]"
      >
        <button
          slot="close"
          [style.--tui-padding]="0"
          size="xs"
          class="place-self-end"
          tuiIconButton
          type="button"
          appearance="icon"
          icon="tuiIconClose"
          (tuiActiveZoneChange)="toggle($event)"
          (click)="toggle(false)"
        ></button>
        <marketplace-show-controls
          slot="controls"
          class="relative"
          [pkg]="pkg"
          [localPkg]="localPkg$ | async"
          (togglePreview)="toggle($event)"
        ></marketplace-show-controls>
      </marketplace-show-preview>
    </div>
  `,
  styles: [],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiActiveZoneModule,
    TuiButtonModule,
    TuiSidebarModule,
    MarketplaceShowPreviewModule,
    ItemModule,
    MarketplaceShowComponentsModule,
  ],
})
export class MarketplaceItemToggleComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  constructor(private readonly patch: PatchDB<DataModel>) {}

  localPkg$!: Observable<PackageDataEntry>
  open = false

  ngOnChanges() {
    this.localPkg$ = this.patch
      .watch$('package-data', this.pkg.manifest.id)
      .pipe(filter(Boolean), shareReplay({ bufferSize: 1, refCount: true }))
  }

  toggle(open: boolean) {
    this.open = open
  }
}
