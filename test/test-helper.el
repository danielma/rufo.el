(require 'f)

(defvar rufo-test/root-path (f-parent (f-parent (f-this-file))))

(require 'rufo-minor-mode (f-join rufo-test/root-path "rufo.el"))
