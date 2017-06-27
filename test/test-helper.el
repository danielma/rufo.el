(require 'f)

(defvar rufo-test/root-path (f-parent (f-parent (f-this-file))))

(require 'rufo-mode (f-join rufo-test/root-path "rufo-mode.el"))
