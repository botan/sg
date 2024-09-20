#' Create and send MIME formatted message objects
#'
#' @description
#' Methods for creating and sending MIME messages.
#'
#' @return A MIME object.
#' @export
#' @examples
#' \dontrun{
#'   sg_mime() |>
#'     sg_from("sender@example.com") |>
#'     sg_to("recipient1@example.com", "recipient2@example.com") |>
#'     sg_cc("cc1@example.com", "cc2@example.com") |>
#'     sg_bcc("bcc1@example.com", "bcc2@example.com") |>
#'     sg_subject("This is the subject") |>
#'     sg_body("Hello from sg!") |>
#'     sg_attachments("path/to/file1.csv", "path/to/file2.pdf") |>
#'     sg_send()
#' }
sg_mime <- function() {
  structure(
    list(
      to = list(),
      cc = list(),
      bcc = list(),
      from = NULL,
      subject = NULL,
      content = list(),
      attachments = list()
    ),
    class = "mime"
  )
}

#' Generic for setting the 'from' field
#'
#' @param x A MIME object.
#' @param email The sender's email address.
#' @rdname sg_mime
#' @export
sg_from <- function(x, email) {
  UseMethod("sg_from")
}

#' @export
sg_from.mime <- function(x, email) {
  x$from <- list(email = email)
  x
}

#' Generic for adding recipients to 'to' field
#'
#' @param ... One or more email addresses.
#' @rdname sg_mime
#' @export
sg_to <- function(x, ...) {
  UseMethod("sg_to")
}

#' @export
sg_to.mime <- function(x, ...) {
  emails <- rlang::list2(...)
  x$to <- lapply(emails, \(email) list(email = email))
  x
}

#' Generic for adding recipients to 'cc' field
#' @rdname sg_mime
#' @export
sg_cc <- function(x, ...) {
  UseMethod("sg_cc")
}

#' @export
sg_cc.mime <- function(x, ...) {
  emails <- rlang::list2(...)
  x$cc <- lapply(emails, \(email) list(email = email))
  x
}

#' Generic for adding recipients to 'bcc' field
#' @rdname sg_mime
#' @export
sg_bcc <- function(x, ...) {
  UseMethod("sg_bcc")
}

#' @export
sg_bcc.mime <- function(x, ...) {
  emails <- rlang::list2(...)
  x$bcc <- lapply(emails, \(email) list(email = email))
  x
}

#' Generic for setting the email subject
#' @rdname sg_mime
#' @param subject The email subject.
#' @export
sg_subject <- function(x, subject) {
  UseMethod("sg_subject")
}

#' @export
sg_subject.mime <- function(x, subject) {
  x$subject <- subject
  x
}

#' Generic for setting the email body
#' @rdname sg_mime
#' @param body The body content of the email.
#' @param type The content type (e.g., "text/plain")
#' @export
sg_body <- function(x, body, type = "text/html") {
  UseMethod("sg_body")
}

#' @export
sg_body.mime <- function(x, body, type = "text/html") {
  x$content <- list(type = type, value = body)
  x
}

#' Generic for adding attachments
#' @rdname sg_mime
#' @export
sg_attachments <- function(x, ...) {
  UseMethod("sg_attachments")
}

#' @export
sg_attachments.mime <- function(x, ...) {
  paths <- rlang::list2(...)

  attachments <-
    lapply(paths, function(path) {
      content <- base64enc::base64encode(path)
      filename <- basename(path)
      mime_type <- mime::guess_type(path)

      list(
        content = content,
        filename = filename,
        type = mime_type,
        disposition = "attachment"
      )
    })

  x$attachments <- c(x$attachments, attachments)
  x
}

#' Generic for sending the email
#' @rdname sg_mime
#' @export
sg_send <- function(x) {
  UseMethod("sg_send")
}

#' @export
sg_send.mime <- function(x) {
  if (is.null(x$from)) {
    rlang::abort("The 'from' field is required.")
  }

  if (length(x$to) == 0) {
    rlang::abort("At least one 'to' recipient is required.")
  }

  if (!is_email_address(x$from)) {
    rlang::abort("The 'from' field must contain a valid email address.")
  }

  validate_recipients <- function(recipients, field_name) {
    if (length(recipients) > 0 && !all(sapply(recipients, \(r) is_email_address(r$email)))) {
      rlang::abort(paste("One or more invalid email addresses in the", field_name, "field."))
    }
  }

  validate_recipients(x$to, "to")
  validate_recipients(x$cc, "cc")
  validate_recipients(x$bcc, "bcc")

  personalizations <-
    rlang::list2(
      to = x$to,
      cc = if (length(x$cc) > 0) x$cc else NULL,
      bcc = if (length(x$bcc) > 0) x$bcc else NULL
    ) |>
    Filter(Negate(is.null), x = _)

  email <-
    rlang::list2(
      personalizations = list(personalizations),
      from = x$from,
      subject = x$subject,
      content = list(x$content),
      attachments = if (!is.null(x$attachments)) x$attachments else NULL
    ) |>
    Filter(Negate(is.null), x = _)

  req <-
    httr2::request("https://api.sendgrid.com/v3/mail/send") |>
    httr2::req_auth_bearer_token(get_api_key()) |>
    httr2::req_body_json(email, auto_unbox = TRUE)

  resp <- req |> httr2::req_perform()

  if (httr2::resp_status(resp) == 202L) {
    rlang::inform("The email is successfully sent!")
  }

  invisible(email)
}

#' Get API key for auth.
#'
#' @param api_key SendGrid API key.
#' @return api_key
#' @keywords internal
get_api_key <- function(api_key = Sys.getenv("SENDGRID_API_KEY")) {
  if (!nzchar(api_key)) {
    rlang::abort("No API key found, please supply with api_key argument or with SENDGRID_API_KEY env var")
  }

  api_key
}

#' Check if a string is in a valid email format
#'
#' @param email A character string.
#' @return A logical value.
#' @keywords internal
is_email_address <- function(email) {
  email_pattern <- "^[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}$"
  grepl(email_pattern, email)
}