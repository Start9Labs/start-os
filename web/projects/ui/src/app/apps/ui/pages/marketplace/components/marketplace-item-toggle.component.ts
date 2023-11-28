import { CommonModule, DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostListener,
  Inject,
  Input,
  inject,
} from '@angular/core'
import { TuiButtonModule } from '@taiga-ui/core'
import { TuiActiveZoneModule } from '@taiga-ui/cdk'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import { ItemModule, MarketplacePkg } from '@start9labs/marketplace'
import { MarketplaceShowControlsComponent } from '../marketplace-show-preview/components/marketplace-show-controls.component'
import { MarketplaceShowPreviewModule } from '../marketplace-show-preview/marketplace-show-preview.module'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { BehaviorSubject, filter, Observable, shareReplay } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { SidebarService } from 'src/app/services/sidebar.service'
import { ActivatedRoute } from '@angular/router'

@Component({
  selector: 'marketplace-item-toggle',
  template: `
    <div
      [id]="pkg.manifest.id"
      class="block h-full animate"
      style="--animation-order: {{ index }}"
      (click)="toggle(true)"
      (tuiActiveZoneChange)="toggle($event)"
    >
      <marketplace-item [pkg]="pkg"></marketplace-item>
      <marketplace-show-preview
        [pkg]="pkg"
        *tuiSidebar="
          !!(sidebarService.getToggleState(pkg.manifest.id) | async);
          direction: 'right';
          autoWidth: true
        "
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
          [pkg]="pkg"
          [localPkg]="localPkg$ | async"
        ></marketplace-show-controls>
      </marketplace-show-preview>
    </div>
  `,
  styles: [
    `
      .animate {
        animation-name: animateIn;
        animation-duration: 400ms;
        animation-delay: calc(var(--animation-order) * 200ms);
        animation-fill-mode: both;
        animation-timing-function: ease-in-out;
      }

      @keyframes animateIn {
        0% {
          opacity: 0;
          transform: scale(0.6) translateY(-20px);
        }

        100% {
          opacity: 1;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiActiveZoneModule,
    TuiButtonModule,
    TuiSidebarModule,
    MarketplaceShowPreviewModule,
    ItemModule,
    MarketplaceShowControlsComponent,
  ],
})
export class MarketplaceItemToggleComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input({ required: true })
  index!: number

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly activatedRoute: ActivatedRoute,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}
  readonly sidebarService = inject(SidebarService)
  localPkg$!: Observable<PackageDataEntry>
  pkgIdQueryParam = new BehaviorSubject<string>('')
  readonly pkgId = this.activatedRoute.queryParamMap.subscribe(params => {
    this.pkgIdQueryParam.next(params.get('id')!)
  })

  ngOnChanges() {
    this.localPkg$ = this.patch
      .watch$('package-data', this.pkg.manifest.id)
      .pipe(filter(Boolean), shareReplay({ bufferSize: 1, refCount: true }))
  }

  @HostListener('animationend', ['$event.target'])
  async onAnimationEnd(_target: EventTarget | null) {
    if (this.pkgIdQueryParam.value === this.pkg.manifest.id) {
      this.toggle(true)
    }
  }

  toggle(open: boolean) {
    this.sidebarService.toggleState(this.pkg.manifest.id, open)
  }
}
