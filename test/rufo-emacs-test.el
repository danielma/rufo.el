(defun reset-custom-variables (variables)
  (dolist (var variables)
    (set var (eval (car (get var 'standard-value))))))

(defmacro with-test-setup (&rest body)
  "Create a temporary buffer and test setup, and evaluate BODY there like `progn`."
  `(progn
     (reset-custom-variables (list 'rufo-minor-mode-executable 'rufo-minor-mode-use-bundler 'rufo-minor-mode-debug-mode))
     (with-temp-buffer
       (progn ,@body)))
  )

(defun assert-global-executable-format ()
  (setq rufo-minor-mode-executable
        (concat
         (replace-regexp-in-string "\n$" "" 
                                   (shell-command-to-string "bundle show rufo"))
         "/exe/rufo"))
  (rufo-format))

(ert-deftest verify-format-with-filename ()
  (with-test-setup
   (setq buffer-file-name "source.rb")
   (insert "[ 1 ]")
   (assert-global-executable-format)
   (should (equal (buffer-string) "[1]\n"))))

(ert-deftest verify-format-size-1 ()
  (with-test-setup
   (insert "[ 1 ]")
   (assert-global-executable-format)
   (should (equal (buffer-string) "[1]\n"))))

(ert-deftest verify-format-size-2 ()
  (with-test-setup
   (insert "
class NeedsFormatting
  def method no_parens
   [poorly_indented, no_parens ]
  end
end;")
   (assert-global-executable-format)
   (should (equal (buffer-string) "
class NeedsFormatting
  def method(no_parens)
    [poorly_indented, no_parens]
  end
end
"))))

(ert-deftest verify-format-size-3 ()
  (with-test-setup
   (setq rufo-minor-mode-debug-mode t)
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
   (assert-global-executable-format)
   (should (equal (buffer-string) valid-ruby))))

(ert-deftest verify-bundle-format ()
  (with-test-setup
   (insert "[ 1 ]")
   (setq rufo-minor-mode-use-bundler t)
   (rufo-format)
   (should (equal (buffer-string) "[1]\n"))))

(ert-deftest failure-with-bad-rufo ()
  (with-test-setup
   (insert "[ 1 ]")
   (setq rufo-minor-mode-executable "rufa")
   (rufo-format)
   (with-current-buffer "*Messages*"
     (should (s-contains? "Could not find rufo." (buffer-string))))
   (should (equal (buffer-string) "[ 1 ]"))))

(ert-deftest debug-mode-should-show-diff ()
  (with-test-setup
   (insert "[ 1 ]")
   (setq rufo-minor-mode-debug-mode t)
   (assert-global-executable-format)
   (with-current-buffer "*Messages*"
     (should (s-contains? "d1" (buffer-string))))))
