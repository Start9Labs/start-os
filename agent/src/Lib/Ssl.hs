{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib.Ssl where

import           Startlude

import           Control.Lens
import           Data.String.Interpolate.IsString
import           System.Process

root_CA_CERT_NAME :: Text
root_CA_CERT_NAME = "Embassy Local Root CA"

root_CA_OPENSSL_CONF :: FilePath -> ByteString
root_CA_OPENSSL_CONF path = [i|
# OpenSSL root CA configuration file.
# Copy to `/root/ca/openssl.cnf`.

[ ca ]
# `man ca`
default_ca = CA_default

[ CA_default ]
# Directory and file locations.
dir               = #{path}
certs             = $dir/certs
crl_dir           = $dir/crl
new_certs_dir     = $dir/newcerts
database          = $dir/index.txt
serial            = $dir/serial
RANDFILE          = $dir/private/.rand

# The root key and root certificate.
private_key       = $dir/private/ca.key.pem
certificate       = $dir/certs/ca.cert.pem

# For certificate revocation lists.
crlnumber         = $dir/crlnumber
crl               = $dir/crl/ca.crl.pem
crl_extensions    = crl_ext
default_crl_days  = 30

# SHA-1 is deprecated, so use SHA-2 instead.
default_md        = sha256

name_opt          = ca_default
cert_opt          = ca_default
default_days      = 375
preserve          = no
policy            = policy_loose

[ policy_loose ]
# Allow the intermediate CA to sign a more diverse range of certificates.
# See the POLICY FORMAT section of the `ca` man page.
countryName             = optional
stateOrProvinceName     = optional
localityName            = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ req ]
# Options for the `req` tool (`man req`).
default_bits        = 4096
distinguished_name  = req_distinguished_name
string_mask         = utf8only
prompt              = no

# SHA-1 is deprecated, so use SHA-2 instead.
default_md          = sha256

# Extension to add when the -x509 option is used.
x509_extensions     = v3_ca

[ req_distinguished_name ]
# See <https://en.wikipedia.org/wiki/Certificate_signing_request>.
CN = #{root_CA_CERT_NAME}
O = Start9 Labs
OU = Embassy

[ v3_ca ]
# Extensions for a typical CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ v3_intermediate_ca ]
# Extensions for a typical intermediate CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true, pathlen:0
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ usr_cert ]
# Extensions for client certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = client, email
nsComment = "OpenSSL Generated Client Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, nonRepudiation, digitalSignature, keyEncipherment
extendedKeyUsage = clientAuth, emailProtection

[ server_cert ]
# Extensions for server certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = server
nsComment = "OpenSSL Generated Server Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer:always
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth

[ crl_ext ]
# Extension for CRLs (`man x509v3_config`).
authorityKeyIdentifier=keyid:always

[ ocsp ]
# Extension for OCSP signing certificates (`man ocsp`).
basicConstraints = CA:FALSE
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, digitalSignature
extendedKeyUsage = critical, OCSPSigning
|]

intermediate_CA_OPENSSL_CONF :: Text -> ByteString
intermediate_CA_OPENSSL_CONF path = [i|
# OpenSSL intermediate CA configuration file.
# Copy to `/root/ca/intermediate/openssl.cnf`.

[ ca ]
# `man ca`
default_ca = CA_default

[ CA_default ]
# Directory and file locations.
dir               = #{path}
certs             = $dir/certs
crl_dir           = $dir/crl
new_certs_dir     = $dir/newcerts
database          = $dir/index.txt
serial            = $dir/serial
RANDFILE          = $dir/private/.rand

# The root key and root certificate.
private_key       = $dir/private/intermediate.key.pem
certificate       = $dir/certs/intermediate.cert.pem

# For certificate revocation lists.
crlnumber         = $dir/crlnumber
crl               = $dir/crl/intermediate.crl.pem
crl_extensions    = crl_ext
default_crl_days  = 30

# SHA-1 is deprecated, so use SHA-2 instead.
default_md        = sha256

name_opt          = ca_default
cert_opt          = ca_default
default_days      = 375
preserve          = no
copy_extensions   = copy
policy            = policy_loose


[ policy_loose ]
# Allow the intermediate CA to sign a more diverse range of certificates.
# See the POLICY FORMAT section of the `ca` man page.
countryName             = optional
stateOrProvinceName     = optional
localityName            = optional
organizationName        = optional
organizationalUnitName  = optional
commonName              = supplied
emailAddress            = optional

[ req ]
# Options for the `req` tool (`man req`).
default_bits        = 4096
distinguished_name  = req_distinguished_name
string_mask         = utf8only
prompt              = no

# SHA-1 is deprecated, so use SHA-2 instead.
default_md          = sha256

# Extension to add when the -x509 option is used.
x509_extensions     = v3_ca

[ req_distinguished_name ]
CN = Embassy Local Intermediate CA
O = Start9 Labs
OU = Embassy

[ v3_ca ]
# Extensions for a typical CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ v3_intermediate_ca ]
# Extensions for a typical intermediate CA (`man x509v3_config`).
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
basicConstraints = critical, CA:true, pathlen:0
keyUsage = critical, digitalSignature, cRLSign, keyCertSign

[ usr_cert ]
# Extensions for client certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = client, email
nsComment = "OpenSSL Generated Client Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, nonRepudiation, digitalSignature, keyEncipherment
extendedKeyUsage = clientAuth, emailProtection

[ server_cert ]
# Extensions for server certificates (`man x509v3_config`).
basicConstraints = CA:FALSE
nsCertType = server
nsComment = "OpenSSL Generated Server Certificate"
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer:always
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth

[ crl_ext ]
# Extension for CRLs (`man x509v3_config`).
authorityKeyIdentifier=keyid:always

[ ocsp ]
# Extension for OCSP signing certificates (`man ocsp`).
basicConstraints = CA:FALSE
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid,issuer
keyUsage = critical, digitalSignature
extendedKeyUsage = critical, OCSPSigning
|]

domain_CSR_CONF :: Text -> ByteString
domain_CSR_CONF name = [i|
[req]
default_bits = 4096
default_md = sha256
distinguished_name = req_distinguished_name
prompt = no

[req_distinguished_name]
CN = #{name}
O = Start9 Labs
OU = Embassy
|]

writeRootCaCert :: MonadIO m => FilePath -> FilePath -> FilePath -> m (ExitCode, String, String)
writeRootCaCert confPath keyFilePath certFileDestinationPath = liftIO $ readProcessWithExitCode
    "openssl"
    [ "req"
    , -- use x509
      "-new"
    , -- new request
      "-x509"
    , -- self signed x509
      "-nodes"
    , -- no passphrase
      "-days"
    , -- expires in...
      "3650"
    , -- valid for 10 years. Max is 20 years
      "-key"
    , -- source private key
      toS keyFilePath
    , "-out"
    -- target cert path
    , toS certFileDestinationPath
    , "-config"
      -- configured by...
    , toS confPath
    ]
    ""

data DeriveCertificate = DeriveCertificate
    { applicantConfPath :: FilePath
    , applicantKeyPath  :: FilePath
    , applicantCertPath :: FilePath
    , signingConfPath   :: FilePath
    , signingKeyPath    :: FilePath
    , signingCertPath   :: FilePath
    , duration          :: Integer
    }
writeIntermediateCert :: MonadIO m => DeriveCertificate -> m (ExitCode, String, String)
writeIntermediateCert DeriveCertificate {..} = liftIO $ interpret $ do
    -- openssl genrsa -out dump/int.key 4096
    segment $ openssl [i|genrsa -out #{applicantKeyPath} 4096|]
    -- openssl req -new -config dump/int-csr.conf -key dump/int.key -nodes -out dump/int.csr
    segment $ openssl [i|req -new
                             -config #{applicantConfPath}
                             -key #{applicantKeyPath}
                             -nodes
                             -out #{applicantCertPath <> ".csr"}|]
    -- openssl x509 -CA dump/ca.crt -CAkey dump/ca.key -CAcreateserial -days 3650 -req -in dump/int.csr -out dump/int.crt
    segment $ openssl [i|ca -batch
                            -config #{signingConfPath}
                            -rand_serial
                            -keyfile #{signingKeyPath}
                            -cert #{signingCertPath}
                            -extensions v3_intermediate_ca
                            -days #{duration}
                            -notext
                            -in #{applicantCertPath <> ".csr"}
                            -out #{applicantCertPath}|]
    liftIO $ readFile signingCertPath >>= appendFile applicantCertPath

writeLeafCert :: MonadIO m => DeriveCertificate -> Text -> Text -> m (ExitCode, String, String)
writeLeafCert DeriveCertificate {..} hostname torAddress = liftIO $ interpret $ do
    segment $ openssl [i|genrsa -out #{applicantKeyPath} 4096|]
    segment $ openssl [i|req -config #{applicantConfPath}
                             -key #{applicantKeyPath}
                             -new
                             -addext subjectAltName=DNS:#{hostname},DNS:*.#{hostname},DNS:#{torAddress},DNS:*.#{torAddress}
                             -out #{applicantCertPath <> ".csr"}|]
    segment $ openssl [i|ca -batch
                            -config #{signingConfPath}
                            -rand_serial
                            -keyfile #{signingKeyPath}
                            -cert #{signingCertPath}
                            -extensions server_cert
                            -days #{duration}
                            -notext
                            -in #{applicantCertPath <> ".csr"}
                            -out #{applicantCertPath}
                            |]
    liftIO $ readFile signingCertPath >>= appendFile applicantCertPath

openssl :: Text -> IO (ExitCode, String, String)
openssl = ($ "") . readProcessWithExitCode "openssl" . fmap toS . words
{-# INLINE openssl #-}

interpret :: ExceptT ExitCode (StateT (String, String) IO) () -> IO (ExitCode, String, String)
interpret = fmap (over _1 (either id (const ExitSuccess)) . regroup) . flip runStateT ("", "") . runExceptT
{-# INLINE interpret #-}

regroup :: (a, (b, c)) -> (a, b, c)
regroup (a, (b, c)) = (a, b, c)
{-# INLINE regroup #-}

segment :: IO (ExitCode, String, String) -> ExceptT ExitCode (StateT (String, String) IO) ()
segment action = liftIO action >>= \case
    (ExitSuccess, o, e) -> modify (bimap (<> o) (<> e))
    (ec         , o, e) -> modify (bimap (<> o) (<> e)) *> throwE ec
{-# INLINE segment #-}
