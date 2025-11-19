import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { DocsLinkDirective } from '@start9labs/shared'

@Component({
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
        font-family: system-ui;
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
              <h2 style="font-variant-caps: all-small-caps">
                Root Certificate Authority
              </h2>
              <p>
                Download your server's Root CA and
                <a
                  docsLink
                  path="/user-manual/trust-ca.html"
                  style="color: #6866cc; font-weight: bold; text-decoration: none"
                >
                  follow instructions
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
              box-shadow: rgba(0, 0, 0, 0.1) 0 4px 6px -1px,
                rgba(0, 0, 0, 0.06) 0 2px 4px -1px;
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
              Permanent Local Address
            </h2>
            <p>
              You must be connected to the same Local Area Network (LAN) as your
              server to access this address.
            </p>
            <p
              style="
            padding: 16px 0;
            font-weight: bold;
            font-size: 1.1rem;
            overflow: auto;
          "
            >
              <code id="lan-addr"></code>
            </p>
          </section>
        </div>
      </body>
    </html>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [DocsLinkDirective],
})
export class DocumentationComponent {
  @Input({ required: true }) lanAddress!: string

  get crtName(): string {
    return `${new URL(this.lanAddress).hostname}.crt`
  }
}
