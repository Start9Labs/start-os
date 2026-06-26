# SMTP

Configure StartOS with third-party SMTP credentials so that services like NextCloud, Vaultwarden, and Gitea can send email notifications. SMTP is configured under **System > SMTP**.

> [!WARNING]
> This guide is _not_ for using StartOS as an email server. It is for granting StartOS the simple ability to _send_ emails through a 3rd party SMTP server.

## Getting SMTP Credentials

The guides below are for using Gmail, Amazon SES, or Proton Mail for SMTP, but you may also use another third party provider of your choice.

{{#tabs global="smtp-provider"}}

{{#tab name="Gmail"}}

1. Access your Google 2-step verification settings: https://myaccount.google.com/signinoptions/two-step-verification.

1. Enable 2-Step verification if not already.

1. Go to the App Passwords page: https://myaccount.google.com/apppasswords. Google no longer links to it from the 2-Step Verification page, so navigate there directly.

1. Choose a name for the new App Password. You may call it anything, such as "SMTP", then click "Create".

1. A random 16-character password will be created and shown to you. This is your app password. Save it somewhere secure, such as your Vaultwarden password manager, then click "Done".

1. In StartOS, go to **System > SMTP**, select **Gmail** as the provider, and enter the values below. Choosing Gmail pre-fills the host and defaults to TLS on port 465.

   | Parameter           | Value                          |
   | ------------------- | ------------------------------ |
   | Host                | smtp.gmail.com                 |
   | Connection Security | TLS                            |
   | Port                | 465                            |
   | From Address        | your-username@gmail.com        |
   | Username            | your-username@gmail.com        |
   | Password            | your App Password (from above) |

   > [!NOTE]
   > Selecting Gmail pre-fills and locks the **Host**, and choosing **TLS** locks the **Port** at 465. To use STARTTLS instead, set **Connection Security** to **STARTTLS** and the **Port** becomes **587**. The **From Address** may optionally include a display name, e.g. `Your Name <your-username@gmail.com>`.

{{#endtab}}

{{#tab name="Amazon SES"}}

To use Amazon SES you will need:

- An Amazon Web Services (AWS) account. If you don't have one, you can [register here](https://aws.amazon.com/) for free.
- To set up Amazon Simple Email Service (SES) on AWS, from [inside your AWS](https://aws.amazon.com/ses) console, also free for a time within [certain limits](https://aws.amazon.com/ses/pricing/).
- Optional: To purchase your own domain name to send emails from, then add the domain records Amazon provides you. This will allow you to request 'Production Access' to send emails to unverified addresses (i.e. to more than just your own email address).

You can then refer to the [Amazon SES docs](https://docs.aws.amazon.com/ses/latest/dg/smtp-credentials.html) to create a SMTP user.

{{#endtab}}

{{#tab name="Proton Mail"}}

Access to Proton Mail's SMTP settings is currently only made available by Proton to their customers with **Proton for Business**, as well as certain higher tier individual and family plans (**Proton Duo**, **Proton Family** – both with "SMTP Submission" as a listed feature), and then only when you point a custom domain to your account.

To set up Proton Mail for SMTP you will need:

- To purchase and point a domain name from a domain registrar to Proton's servers by following this guide here: [Custom Domain](https://proton.me/support/custom-domain)
- To follow the steps in the section **How to set up SMTP** in the guide here: [SMTP Submission](https://proton.me/support/smtp-submission)

{{#endtab}}

{{#endtabs}}

## Configuring StartOS

1. Navigate to `System > SMTP`

1. Enter your SMTP credentials and hit "Save".

1. On the same page, send yourself a test email. Remember to check your spam folder. If the email goes to spam, mark it as not spam.

1. For each service you want to use the credentials to send emails, go to the dashboard of that service, click "Actions", and locate the relevant action.
