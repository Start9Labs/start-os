import { NgModule } from '@angular/core'
import { HealthComponent } from './health.component'
import { TuiRingChartModule } from '@taiga-ui/addon-charts'
import { TuiHintModule } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'

@NgModule({
  imports: [CommonModule, TuiRingChartModule, TuiHintModule],
  declarations: [HealthComponent],
  exports: [HealthComponent],
})
export class HealthModule {}
