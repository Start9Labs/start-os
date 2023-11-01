import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { EmverPipesModule } from '@start9labs/shared'
import {
  TuiAvatarModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'

@Component({
  selector: 'sideload-dependencies',
  template: `
    <h3 class="g-title" [style.text-indent.rem]="1">Dependencies</h3>
    <div *ngFor="let dep of package.manifest.dependencies | keyvalue" tuiCell>
      <tui-avatar [src]="getImage(dep.key)"></tui-avatar>
      <div tuiTitle>
        <div>
          <strong>{{ getTitle(dep.key) }}&nbsp;</strong>
          <ng-container [ngSwitch]="dep.value.requirement.type">
            <span *ngSwitchCase="'required'">(required)</span>
            <span *ngSwitchCase="'opt-out'">(required by default)</span>
            <span *ngSwitchCase="'opt-in'">(optional)</span>
          </ng-container>
        </div>
        <div tuiSubtitle [style.color]="'var(--tui-text-03)'">
          {{ dep.value.version | displayEmver }}
        </div>
        <div tuiSubtitle>
          {{ dep.value.description }}
        </div>
      </div>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiTitleModule,
    EmverPipesModule,
    TuiAvatarModule,
    TuiCellModule,
  ],
})
export class SideloadDependenciesComponent {
  @Input({ required: true })
  package!: MarketplacePkg

  getTitle(key: string): string {
    return this.package['dependency-metadata'][key]?.title || key
  }

  getImage(key: string): string {
    const icon = this.package['dependency-metadata'][key]?.icon

    return icon ? `data:image/png;base64,${icon}` : key.substring(0, 2)
  }
}
