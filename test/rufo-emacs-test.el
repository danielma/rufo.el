(defun assert-format-with-no-errors ()
  (rufo-format))
  ;; (should (equal (get-buffer "*rufo-mode error*") nil)))

(ert-deftest verify-format-size-1 ()
  (with-temp-buffer
    (insert "[ 1 ]")
    (assert-format-with-no-errors)
    (should (equal (buffer-string) "[1]\n"))))

(ert-deftest verify-format-size-2 ()
  (with-temp-buffer
    (insert "
class NeedsFormatting
  def method no_parens
   [poorly_indented, no_parens ]
  end
end;")
    (assert-format-with-no-errors)
    (should (equal (buffer-string) "
class NeedsFormatting
  def method(no_parens)
    [poorly_indented, no_parens]
  end
end
"))))

(ert-deftest verify-format-size-3 ()
  (with-temp-buffer
    (setq valid-ruby "
class ApplicationController < ActionController::Base
  helper ApplicationHelper

  include Pundit

  protect_from_forgery with: :exception

  rescue_from Pundit::NotAuthorizedError, with: :policy_not_authorized

  before_action :authenticate

  private

  def authenticate
    do_some_authentication
  end

  def policy_not_authorized(exception)
    fail(exception) if Rails.env.development? || Rails.env.test?

    redirect_to failed_authorization_url
  end

  def redirect_back(fallback: nil)
    redirect_to :back
  rescue ActionController::RedirectBackError
    raise unless fallback.present?
    redirect_to fallback
  end

  def alerts
    alerts = []
    alerts << \"package limit!\" if display_package_limit_alert?
    flash.now[:warning] = alerts.join(\"\n\").html_safe
  end
end
")
    (insert valid-ruby)
    (assert-format-with-no-errors)
    (should (equal (buffer-string) valid-ruby))))

(ert-deftest debug-mode-should-show-diff ()
  (with-temp-buffer
    (insert "[ 1 ]")
    (setq rufo-mode-debug-mode t)
    (assert-format-with-no-errors)
    (with-current-buffer (messages-buffer)
      (should (s-contains? "d1" (buffer-string))))))
