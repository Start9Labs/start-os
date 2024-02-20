import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

@Component({
  standalone: true,
  selector: 'app-documentation',
  template: `
    <!doctype html>
    <html lang="en">
      <head>
        <title>StartOS Address Info</title>
      </head>
      <body>
        <div
          style="
        font-family: Montserrat, sans-serif;
        color: #333333;
        display: flex;
        flex-direction: column;
        margin: auto;
        width: clamp(900px, 35vw, 600px);
      "
        >
          <h1
            style="
          font-variant-caps: all-small-caps;
          text-align: center;
          padding: 1rem;
        "
          >
            StartOS Address Info
          </h1>

          <section
            style="
          padding: 1rem 3rem 2rem 3rem;
          margin-bottom: 24px;
          border: solid #c4c4c5 3px;
          border-radius: 20px;
        "
          >
            <div>
              <h3 style="color: #f8546a; font-weight: bold">Important!</h3>
              <p>
                Download your server's Root CA and
                <a
                  href="https://docs.start9.com/0.3.5.x/user-manual/connecting-lan"
                  target="_blank"
                  rel="noreferrer"
                  style="color: #6866cc; font-weight: bold; text-decoration: none"
                >
                  follow the instructions
                </a>
                to establish a secure connection with your server.
              </p>
            </div>
            <div style="text-align: center">
              <a
                id="cert"
                [download]="crtName"
                style="
              display: inline-block;
              padding: 1em 1.2em;
              box-sizing: border-box;
              font-size: 1rem;
              text-decoration: none;
              text-align: center;
              border-radius: clamp(2rem, 3rem, 4rem);
              cursor: pointer;
              box-shadow: rgba(0, 0, 0, 0.1) 0px 4px 6px -1px,
                rgba(0, 0, 0, 0.06) 0px 2px 4px -1px;
              background: #6866cc;
              color: #f4f4f5;
            "
              >
                Download certificate
              </a>
            </div>
          </section>
          <section
            style="
          padding: 1rem 3rem 2rem 3rem;
          border: solid #c4c4c5 3px;
          border-radius: 20px;
          margin-bottom: 24px;
        "
          >
            <h2 style="font-variant-caps: all-small-caps">
              Access from home (LAN)
            </h2>
            <p>
              Visit the address below when you are connected to the same WiFi or
              Local Area Network (LAN) as your server.
            </p>
            <p
              style="
            padding: 16px;
            font-weight: bold;
            font-size: 1.1rem;
            overflow: auto;
          "
            >
              <code id="lan-addr"></code>
            </p>

            <h2 style="font-variant-caps: all-small-caps">
              Access on the go (Tor)
            </h2>
            <p>Visit the address below when you are away from home.</p>
            <p>
              <span style="font-weight: bold">Note:</span>
              This address will only work from a Tor-enabled browser.
              <a
                href="https://docs.start9.com/0.3.5.x/user-manual/connecting-tor"
                target="_blank"
                rel="noreferrer"
                style="color: #6866cc; font-weight: bold; text-decoration: none"
              >
                Follow the instructions
              </a>
              to get setup.
            </p>
            <p
              style="
            padding: 16px;
            font-weight: bold;
            font-size: 1.1rem;
            overflow: auto;
          "
            >
              <code id="tor-addr"></code>
            </p>
          </section>
        </div>
      </body>
    </html>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DocumentationComponent {
  @Input({ required: true }) lanAddress!: string

  get crtName(): string {
    return `${new URL(this.lanAddress).hostname}.crt`
  }
}
