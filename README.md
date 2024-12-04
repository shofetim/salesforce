# Janet Salesforce API wrapper

# Install

```sh
jpm install https://github.com/shofetim/salesforce
```

## Quickstart

Set environment variables for your Salesforce Password, Security Token,
Username, and URL, then test that you can login.

```sh
export SF_PASSWORD="your-password"
export SF_TOKEN="SF API Token"
export SF_USERNAME="your@@username"
export SF_URL="https://{projectname}.my.salesforce.com"

janet -e "(import ./salesforce :as sf) (pp (sf/login))"
```




