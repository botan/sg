
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sg

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Minimalistic ‘SendGrid’ Email API client for sending emails.

## Installation

Install the released version of sg from CRAN:

``` r
install.packages("sg")
```

Or install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("botan/sg")
```

## Authentication

Set your API key using the `SENDGRID_API_KEY` environment variable. The
package will automatically detect and use it when sending emails.

## Usage

This is a basic example which shows you how to send an email:

``` r
library(sg)

sg_mime() |>
  sg_from("sender@example.com") |>
  sg_to("recipient1@example.com", "recipient2@example.com") |>
  sg_cc("cc1@example.com", "cc2@example.com") |>
  sg_bcc("bcc1@example.com", "bcc2@example.com") |>
  sg_subject("This is the subject") |>
  sg_body("Hello from sg!") |>
  sg_attachments("path/to/file1.csv", "path/to/file2.pdf") |>
  sg_send()
```
