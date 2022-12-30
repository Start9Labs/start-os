import { NgModule } from '@angular/core'
import { HealthComponent } from './health.component'
import { TuiPieChartModule } from '@taiga-ui/addon-charts'
import { TuiHintModule } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'

@NgModule({
  imports: [CommonModule, TuiPieChartModule, TuiHintModule],
  declarations: [HealthComponent],
  exports: [HealthComponent],
})
export class HealthModule {}
