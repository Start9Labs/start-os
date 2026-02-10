import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'wifi-settings-aside',
  template: `
    <h3>SSID</h3>
    <p>
      The SSID (Service Set Identifier) is the name of your WiFi network. It
      identifies your network to devices, allowing them to find and connect to
      the network.
    </p>
    <p>
      Broadcasting the SSID makes the network discoverable by devices looking
      for wireless connections. Not broadcasting the SSID can add a layer of
      security by hiding the network from casual users.
    </p>
    <h3>Frequency Band</h3>
    <p>
      The frequency band (2.4GHz or 5GHz) your WiFi operates on. Determines the
      range and speed of your wireless network: 2.4GHz offers better range,
      while 5GHz offers higher speeds. Choose the appropriate frequency band
      based on your needs.
    </p>
    <h3>Frequency Range</h3>
    <p>
      Specific channels within the chosen frequency band. Avoids interference
      from other networks and optimizes performance. Select the frequency range
      for optimal performance.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WifiSettingsAside {}
