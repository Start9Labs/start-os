export default {
  homepage: {
    name: "Homepage",
    description:
      "The page that will be displayed when your Start9 Pages .onion address is visited. Since this page is technically publicly accessible, you can choose to which type of page to display.",
    type: "union",
    default: "welcome",
    tag: {
      id: "type",
      name: "Type",
      "variant-names": {
        welcome: "Welcome",
        index: "Table of Contents",
        "web-page": "Web Page",
        redirect: "Redirect",
      },
    },
    variants: {
      welcome: {},
      index: {},
      "web-page": {
        source: {
          name: "Folder Location",
          description: "The service that contains your website files.",
          type: "enum",
          values: ["filebrowser", "nextcloud"],
          "value-names": {},
          default: "nextcloud",
        },
        folder: {
          type: "string",
          name: "Folder Path",
          placeholder: "e.g. websites/resume",
          description:
            'The path to the folder that contains the static files of your website. For example, a value of "projects/resume" would tell Start9 Pages to look for that folder path in the selected service.',
          pattern:
            "^(\\.|[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)(/[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|/([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)*/?$",
          "pattern-description": "Must be a valid relative file path",
          nullable: false,
        },
      },
      redirect: {
        target: {
          type: "string",
          name: "Target Subdomain",
          description:
            "The name of the subdomain to redirect users to. This must be a valid subdomain site within your Start9 Pages.",
          pattern: "^[a-z-]+$",
          "pattern-description":
            "May contain only lowercase characters and hyphens.",
          nullable: false,
        },
      },
    },
  },
  subdomains: {
    type: "list",
    name: "Subdomains",
    description: "The websites you want to serve.",
    default: [],
    range: "[0, *)",
    subtype: "object",
    spec: {
      "unique-by": "name",
      "display-as": "{{name}}",
      spec: {
        name: {
          type: "string",
          nullable: false,
          name: "Subdomain name",
          description:
            'The subdomain of your Start9 Pages .onion address to host the website on. For example, a value of "me" would produce a website hosted at http://me.xxxxxx.onion.',
          pattern: "^[a-z-]+$",
          "pattern-description":
            "May contain only lowercase characters and hyphens",
        },
        settings: {
          type: "union",
          name: "Settings",
          description:
            "The desired behavior you want to occur when the subdomain is visited. You can either redirect to another subdomain, or load a stored web page.",
          default: "web-page",
          tag: {
            id: "type",
            name: "Type",
            "variant-names": { "web-page": "Web Page", redirect: "Redirect" },
          },
          variants: {
            "web-page": {
              source: {
                name: "Folder Location",
                description: "The service that contains your website files.",
                type: "enum",
                values: ["filebrowser", "nextcloud"],
                "value-names": {},
                default: "nextcloud",
              },
              folder: {
                type: "string",
                name: "Folder Path",
                placeholder: "e.g. websites/resume",
                description:
                  'The path to the folder that contains the website files. For example, a value of "projects/resume" would tell Start9 Pages to look for that folder path in the selected service.',
                pattern:
                  "^(\\.|[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)(/[a-zA-Z0-9_ -][a-zA-Z0-9_ .-]*|/([a-zA-Z0-9_ .-][a-zA-Z0-9_ -]+\\.*)+)*/?$",
                "pattern-description": "Must be a valid relative file path",
                nullable: false,
              },
            },
            redirect: {
              target: {
                type: "string",
                name: "Target Subdomain",
                description:
                  "The subdomain of your Start9 Pages .onion address to redirect to. This should be the name of another subdomain on Start9 Pages. Leave empty to redirect to the homepage.",
                pattern: "^[a-z-]+$",
                "pattern-description":
                  "May contain only lowercase characters and hyphens.",
                nullable: false,
              },
            },
          },
        },
      },
    },
  },
}
